
E:		equ 80 * 2		; # bytes in one line
BASE_ADDR_64:		equ 0x00021000
STACK_TOP:		equ 0x001fffe8
SIZE:			equ 0x1000

struc EFILDR_IMAGE
.CheckSum:	resd 1
.Offset:	resd 1
.Length:	resd 1
.FileName:	resb 52
endstruc

struc EFILDR_HEADER
.Signature:		resd 1
.HeaderCheckSum:	resd 1
.FileLength:		resd 1
.NumberOfImages:	resd 1
endstruc

struc PE_HEADER
			resb 6
.NumberOfSections:	resw 1
			resb 12
.SizeOfOptionalHeader:	resw 1
			resb 2
.Magic:			resb 16
.AddressOfEntryPoint:	resd 1
			resb 4
.ImageBase64:		resd 1
endstruc

struc PE_SECTION_HEADER
			resb 12
.VirtualAddress:	resd 1
.SizeOfRawData:		resd 1
.PointerToRawData:	resd 1
			resb 16
endstruc

%macro StubWithNoCode 1
	push	0	; push error code place holder on the stack
	push	%1
	jmp	strict qword commonIdtEntry
%endmacro

%macro StubWithACode 1
	times 2 nop
	push	%1
	jmp	strict qword commonIdtEntry
%endmacro

%macro PrintReg 2
	mov	esi, %1
	call	PrintString
	mov	rax, [rbp + %2]
	call	PrintQword
%endmacro

	bits 64
	org BASE_ADDR_64
global _start
_start:
	mov	esp, STACK_TOP
;
; set OSFXSR and OSXMMEXCPT because some code will use XMM registers
;
	mov	rax, cr4
	or	rax, 0x600
	mov	cr4, rax

;
; Populate IDT with meaningful offsets for exception handlers...
;
	sidt	[REL Idtr]

	mov	eax, StubTable
	mov	ebx, eax		; use bx to copy 15..0 to descriptors
	shr	eax, 16			; use ax to copy 31..16 to descriptors
					; 63..32 of descriptors is 0
	mov	ecx, 0x78		; 78h IDT entries to initialize with unique entry points
	mov	edi, [REL Idtr + 2]

.StubLoop:	; loop through all IDT entries exception handlers and initialize to default handler
	mov	[rdi], bx			; write bits 15..0 of offset
	mov	word [rdi + 2], SYS_CODE_SEL64	; SYS_CODE_SEL64 from GDT
	mov	word [rdi + 4], 0x8e00		; type = 386 interrupt gate, present
	mov	[rdi + 6], ax			; write bits 31..16 of offset
	mov	dword [rdi + 8], 0		; write bits 63..32 of offset
	add	edi, 16				; move up to next descriptor
	add	ebx, DEFAULT_HANDLER_SIZE	; move to next entry point
	loop	.StubLoop			; loop back through again until all descriptors are initialized

;
; Load EFILDR
;
	mov	esi, BlockSignature + 2
	add	esi, [rsi + EFILDR_HEADER_size + EFILDR_IMAGE.Offset]	; esi = Base of EFILDR.C
	mov	ebp, [rsi + PE_OFFSET_IN_MZ_STUB]
	add	ebp, esi					; ebp = PE Image Header for EFILDR.C
	mov	edi, [rbp + PE_HEADER.ImageBase64]		; edi = ImageBase (63..32 is zero, ignore)
	mov	eax, [rbp + PE_HEADER.AddressOfEntryPoint]	; eax = EntryPoint
	add	eax, edi					; eax = ImageBase + EntryPoint
	mov	[REL .EfiLdrOffset], eax			; Modify jump instruction for correct entry point

	movzx	ebx, word [rbp + PE_HEADER.NumberOfSections]	; bx = Number of sections
	movzx	eax, word [rbp + PE_HEADER.SizeOfOptionalHeader]; ax = Optional Header Size
	add	ebp, eax
	add	ebp, PE_HEADER.Magic				; ebp = Start of 1st Section

.SectionLoop:
	push	rsi						; Save Base of EFILDR.C
	push	rdi						; Save ImageBase
	add	esi, [rbp + PE_SECTION_HEADER.PointerToRawData]	; esi = Base of EFILDR.C + PointerToRawData
	add	edi, [rbp + PE_SECTION_HEADER.VirtualAddress]	; edi = ImageBase + VirtualAddress
	mov	ecx, [rbp + PE_SECTION_HEADER.SizeOfRawData]	; ecx = SizeOfRawData

	cld
	shr	ecx, 2
	rep movsd

	pop	rdi				; Restore ImageBase
	pop	rsi				; Restore Base of EFILDR.C

	add	ebp, PE_SECTION_HEADER_size	; ebp = Pointer to next section record
	dec	ebx
	jnz	.SectionLoop

	movzx	ecx, word [REL Idtr]	; get size of IDT
	inc	ecx
	add	ecx, [REL Idtr + 2]	; add to base of IDT to get location of memory map...
					; argument in RCX
.EfiLdrOffset:	equ $ + 1
	mov	eax, 0x00401000
	jmp	rax			; jump to entry point

StubTable:
%assign i 0
%rep 8
	StubWithNoCode  i
%assign i i+1
%endrep
	StubWithACode   8	; Double Fault
	StubWithNoCode  9
	StubWithACode  10	; Invalid TSS
	StubWithACode  11	; Segment Not Present
	StubWithACode  12	; Stack Fault
	StubWithACode  13	; GP Fault
	StubWithACode  14	; Page Fault
	StubWithNoCode 15
	StubWithNoCode 16
	StubWithACode  17	; Alignment Check
%assign i 18
%rep 102
	StubWithNoCode i
%assign i i+1
%endrep

commonIdtEntry:
	push	rax
	push	rcx
	push	rdx
	push	rbx
	push	rsp
	push	rbp
	push	rsi
	push	rdi
	push	r8
	push	r9
	push	r10
	push	r11
	push	r12
	push	r13
	push	r14
	push	r15
	mov	rbp, rsp
;
;   At this point the stack looks like this:
;
;       Calling SS
;       Calling RSP
;       rflags
;       Calling CS
;       Calling RIP
;       Error code or 0
;       Int num or 0ffh for unknown int num
;       rax
;       rcx
;       rdx
;       rbx
;       rsp
;       rbp
;       rsi
;       rdi
;       r8
;       r9
;       r10
;       r11
;       r12
;       r13
;       r14
;       r15 <------- RSP, RBP
;

	call	ClearScreen
	mov	esi, String1
	call	PrintString
	mov	eax, [rbp + 16 * 8]		; move Int number into EAX
	cmp	eax, 19
	ja	.PrintDefaultString
	mov	esi, [rax * 8 + StringTable]	; get offset from StringTable to actual string address
	jmp	.PrintTheString
.PrintDefaultString:
	mov	esi, IntUnknownString
; patch Int number
	mov	edx, eax
	call	A2C
	mov	[rsi + 1], al
	mov	eax, edx
	shr	eax, 4
	call	A2C
	mov	[rsi], al
.PrintTheString:
	call	PrintString
	PrintReg String2, 19 * 8	; CS
	mov	byte [rdi], ':'
	add	edi, 2
	mov	rax, [rbp + 18 * 8]	; RIP
	call	PrintQword
	mov	esi, String3
	call	PrintString

	mov	edi, VGA_FB + 2 * VGA_LINE

	PrintReg StringRax, 15 * 8

	PrintReg StringRcx, 14 * 8

	PrintReg StringRdx, 13 * 8

	mov	edi, VGA_FB + 3 * VGA_LINE

	PrintReg StringRbx, 12 * 8

	PrintReg StringRsp, 11 * 8

	PrintReg StringRbp, 10 * 8

	PrintReg StringRsi,  9 * 8

	PrintReg StringRdi,  8 * 8

	mov	edi, VGA_FB + 4 * VGA_LINE

	PrintReg StringR8,  7 * 8

	PrintReg StringR9,  6 * 8

	PrintReg StringR10, 5 * 8

	PrintReg StringR11, 4 * 8

	PrintReg StringR12, 3 * 8

	PrintReg StringR13, 2 * 8

	PrintReg StringR14, 1 * 8

	PrintReg StringR15, 0 * 8

	mov	edi, VGA_FB + 5 * VGA_LINE

	mov	esi, String4
	call	PrintString
	mov	rax, [rbp + 17 * 8]	; RFLAGS
	call	PrintQword

	mov	edi, VGA_FB + 6 * VGA_LINE

	mov	esi, String5
	call	PrintString
	mov	rax, [rbp + 16 * 8]	; Int number
	call	PrintQword

	mov	edi, VGA_FB + 7 * VGA_LINE

	mov	esi, String6
	call	PrintString
	mov	rax, [rbp + 20 * 8]	; Error code
	call	PrintQword

	mov	edi, VGA_FB + 8 * VGA_LINE

	mov	esi, String7
	call	PrintString
	mov	rax, [rbp + 21 * 8]	; Calling RIP
	call	PrintQword

	mov	edi, VGA_FB + 9 * VGA_LINE

	mov	esi, String8
	call	PrintString
	mov	rax, [rbp + 22 * 8]	; Calling CS
	call	PrintQword

	mov	edi, VGA_FB + 10 * VGA_LINE

	mov	esi, String9
	call	PrintString
	mov	rax, [rbp + 23 * 8]	; Calling RSP
	call	PrintQword

	mov	edi, VGA_FB + 11 * VGA_LINE

	mov	esi, String10
	call	PrintString
	mov	rax, [rbp + 24 * 8]	; Calling SS
	call	PrintQword

	mov	edi, VGA_FB + 12 * VGA_LINE

	mov	esi, String11
	call	PrintString
	mov	rax, [rbp + 25 * 8]	; Calling RFLAGS
	call	PrintQword

	mov	edi, VGA_FB + 13 * VGA_LINE

	mov	esi, String12
	call	PrintString
	mov	rax, [rbp + 26 * 8]	; RAX
	call	PrintQword

	mov	edi, VGA_FB + 14 * VGA_LINE

	mov	esi, String13
	call	PrintString
	mov	rax, [rbp + 27 * 8]	; RCX
	call	PrintQword

	mov	edi, VGA_FB + 15 * VGA_LINE

	mov	esi, String14
	call	PrintString
	mov	rax, [rbp + 28 * 8]	; RDX
	call	PrintQword

	mov	edi, VGA_FB + 16 * VGA_LINE

	mov	esi, String15
	call	PrintString
	mov	rax, [rbp + 29 * 8]	; RBX
	call	PrintQword

	mov	edi, VGA_FB + 17 * VGA_LINE

	mov	esi,
String16
	call	PrintString
	mov	rax, [rbp + 30 * 8]	; RSP
	call	PrintQword

	mov	edi, VGA_FB + 18 * VGA_LINE

	mov	esi, String17
	call	PrintString
	mov	rax, [rbp + 31 * 8]	; RBP
	call	PrintQword

	mov	edi, VGA_FB + 19 * VGA_LINE

	mov	esi, String18
	call	PrintString
	mov	rax, [rbp + 32 * 8]	; RSI
	call	PrintQword

	mov	edi, VGA_FB + 20 * VGA_LINE

	mov	esi, String19
	call	PrintString
	mov	rax, [rbp + 33 * 8]	; RDI
	call	PrintQword

	mov	edi, VGA_FB + 21 * VGA_LINE

	mov	esi, String20
	call	PrintString
	mov	rax, [rbp + 34 * 8]	; R8
	call	PrintQword

	mov	edi, VGA_FB + 22 * VGA_LINE

	mov	esi, String21
	call	PrintString
	mov	rax, [rbp + 35 * 8]	; R9
	call	PrintQword

	mov	edi, VGA_FB + 23 * VGA_LINE

	mov	esi, String22
	call	PrintString
	mov	rax, [rbp + 36 * 8]	; R10
	call	PrintQword

	mov	edi, VGA_FB + 24 * VGA_LINE

	mov	esi, String23
	call	PrintString
	mov	rax, [rbp + 37 * 8]	; R11
	call	PrintQword

	mov	edi, VGA_FB + 25 * VGA_LINE

	mov	esi, String24
	call	PrintString
	mov	rax, [rbp + 38 * 8]	; R12
	call	PrintQword

	mov	edi, VGA_FB + 26 * VGA_LINE

	mov	esi, String25
	call	PrintString
	mov	rax, [rbp + 39 * 8]	; R13
	call	PrintQword

	mov	edi, VGA_FB + 27 * VGA_LINE

	mov	esi, String26
	call	PrintString
	mov	rax, [rbp + 40 * 8]
	call	PrintQword

	mov	edi, VGA_FB + 28 * VGA_LINE

	mov	esi, String27
