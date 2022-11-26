Copyright (c) 1999-2003 MR , Inc. All rights reserved.
;
; @APPLE_LICENSE_HEADER_START@
; 
; Portions Copyright (c) 1999-2003 Apple Computer, Inc.  All Rights
; Reserved.  This file contains Original Code and/or Modifications of
; Original Code as defined in and that are subject to the Apple Public
; Source License Version 2.0 (the "License").  You may not use this file
; except in compliance with the License.  Please obtain a copy of the
; License at http://www.apple.com/publicsource and read it before using
; this file.
; 
; The Original Code and all software distributed under the License are
; distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, EITHER
; EXPRESS OR IMPLIED, AND APPLE HEREBY DISCLAIMS ALL SUCH WARRANTIES,
; INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE OR NON- INFRINGEMENT.  Please see the
; License for the specific language governing rights and limitations
; under the License.
; 
; @APPLE_LICENSE_HEADER_END@
;
; Partition Boot Loader: boot1h
;
; This program is designed to reside in sector 0+1 of an HFS+ partition.
; It expects that the MBR has left the drive number in DL
; and a pointer to the partition entry in SI.
; 
; This version requires a BIOS with EBIOS (LBA) support.
;
; This code is written for the NASM assembler.
;   nasm boot1.s -o boot1h

;
; This version of boot1h tries to find a stage2 boot file in the root folder.
;
; NOTE: this is an experimental version with multiple extent support.
;
; Written by Tam�s Kos�rszky on 2008-04-14
;

;
; Set to 1 to enable obscure debug messages.
;
DEBUG				EQU		0

;
; Set to 1 to enable unused code.
;
UNUSED				EQU		0

;
; Set to 1 to enable verbose mode.
;
VERBOSE				EQU		0

;
; Set to 1 to enable debug mode.


DEBUG_MODE			EQU		0


;
; Set to 1 to enable debug mode for the boot file search.
;
DEBUG_SEARCH			EQU		0

;
; Set to 1 to enable debug mode for the boot file loading.
;
DEBUG_LOAD			EQU		0

;
; Set to 1 to enable debug mode for the boot file execution.


; Format of fdisk partition entry.
;
; The symbol 'part_size' is automatically defined as an `EQU'
; giving the size of the structure.
;
			struc part
.bootid		resb 1		; bootable or not 
.head		resb 1		; starting head, sector, cylinder
.sect		resb 1		;
.cyl		resb 1		;
.type		resb 1		; partition type
.endhead	resb 1		; ending head, sector, cylinder
.endsect	resb 1		;
.endcyl		resb 1		;
.lba		resd 1		; starting lba
.sectors	resd 1		; size in sectors
			endstruc

;-------------------------------------------------------------------------
; HFS+ related structures and constants
;
kHFSPlusSignature		EQU		'H+'		; HFS+ volume signature
kHFSPlusCaseSignature	EQU		'HX'		; HFS+ volume case-sensitive signature
kHFSPlusCaseSigX		EQU		'X'			; upper byte of HFS+ volume case-sensitive signature
kHFSPlusExtentDensity	EQU		8			; 8 extent descriptors / extent record

;
; HFSUniStr255
;
					struc	HFSUniStr255
.length				resw	1
.unicode			resw	255
					endstruc

;
; HFSPlusExtentDescriptor
;
					struc	HFSPlusExtentDescriptor
.startBlock			resd	1
.blockCount			resd	1
					endstruc

;
; HFSPlusForkData
;
					struc	HFSPlusForkData
.logicalSize		resq	1
.clumpSize			resd	1
.totalBlocks		resd	1
.extents			resb	kHFSPlusExtentDensity * HFSPlusExtentDescriptor_size
					endstruc

;
; HFSPlusVolumeHeader
;
					struc	HFSPlusVolumeHeader
.signature			resw	1
.version			resw	1
.attributes			resd	1
.lastMountedVersion resd	1
.journalInfoBlock	resd	1
.createDate			resd	1
.modifyDate			resd	1
.backupDate			resd	1
.checkedDate		resd	1
.fileCount			resd	1
.folderCount		resd	1
.blockSize			resd	1
.totalBlocks		resd	1
.freeBlocks			resd	1
.nextAllocation		resd	1
.rsrcClumpSize		resd	1
.dataClumpSize		resd	1
.nextCatalogID		resd	1
.writeCount			resd	1
.encodingsBitmap	resq	1
.finderInfo			resd	8
.allocationFile		resb	HFSPlusForkData_size
.extentsFile		resb	HFSPlusForkData_size
.catalogFile		resb	HFSPlusForkData_size
.attributesFile		resb	HFSPlusForkData_size
.startupFile		resb	HFSPlusForkData_size
					endstruc

;
; B-tree related structures and constants
;

kBTIndexNode		EQU		0
kBTMaxRecordLength	EQU		264					; sizeof(kHFSPlusFileThreadRecord)
kHFSRootParentID	EQU		1					; Parent ID of the root folder
kHFSRootFolderID	EQU		2					; Folder ID of the root folder
kHFSExtentsFileID	EQU		3					; File ID of the extents overflow file
kHFSCatalogFileID	EQU		4					; File ID of the catalog file
kHFSPlusFileRecord	EQU		0x200
kForkTypeData		EQU		0
kForkTypeResource	EQU		0xFF

;
; BTNodeDescriptor
;
					struc	BTNodeDescriptor
.fLink				resd	1
.bLink				resd	1
.kind				resb	1
.height				resb	1
.numRecords			resw	1
.reserved			resw	1
					endstruc

;
; BTHeaderRec
;
					struc	BTHeaderRec
.treeDepth			resw	1
.rootNode			resd	1
.leafRecords		resd	1
.firstLeafNode		resd	1
.lastLeafNode		resd	1
.nodeSize			resw	1
.maxKeyLength		resw	1
.totalNodes			resd	1
.freeNodes			resd	1
.reserved1			resw	1
.clumpSize			resd	1
.btreeType			resb	1
.keyCompareType		resb	1
.attributes			resd	1
.reserved3			resd	16
					endstruc

;
; BTIndexRec
;
					struc	BTIndexRec
.childID			resd	1
					endstruc

;
; HFSPlusCatalogKey
;
					struc	HFSPlusCatalogKey
;
; won't use the keyLength field for easier addressing data inside this structure
;
;.keyLength			resw	1

.parentID			resd	1
.nodeName			resb	HFSUniStr255_size
					endstruc

;
; HFSPlusExtentKey
;
					struc	HFSPlusExtentKey
;
; won't use the keyLength field for easier addressing data inside this structure
;
;.keyLength			resw	1

.forkType			resb	1
.pad				resb	1
.fileID				resd	1
.startBlock			resd	1
					endstruc

;
; HFSPlusBSDInfo
;
					struc	HFSPlusBSDInfo
.ownerID			resd	1
.groupID			resd	1
.adminFlags			resb	1
.ownerFlags			resb	1
.fileMode			resw	1
.special			resd	1
					endstruc
					
;
; FileInfo
;
					struc	FileInfo
.fileType			resd	1
.fileCreator		resd	1
.finderFlags		resw	1
.location			resw	2
.reservedField		resw	1
					endstruc

;
; ExtendedFileInfo
;
					struc	ExtendedFileInfo
.reserved1			resw	4
.extFinderFlags		resw	1
.reserved2			resw	1
.putAwayFolderID	resd	1
					endstruc

;
; HFSPlusCatalogFile
;
					struc	HFSPlusCatalogFile
.recordType			resw	1
.flags				resw	1
.reserved1			resd	1
.fileID				resd	1
.createDate			resd	1
.contentModDate		resd	1
.attributeModDate	resd	1
.accessDate			resd	1
.backupDate			resd	1
.permissions		resb	HFSPlusBSDInfo_size
.userInfo			resb	FileInfo_size
.finderInfo			resb	ExtendedFileInfo_size
.textEncoding		resd	1
.reserved2			resd	1
.dataFork			resb	HFSPlusForkData_size
.resourceFork		resb	HFSPlusForkData_size
					endstruc

;
; Macros.
;
%macro jmpabs 1
	push	WORD %1
	ret
%endmacro

%macro DebugCharMacro 1
	pushad
	mov		al, %1
	call	print_char
	call	getc
	popad
%endmacro

%macro PrintCharMacro 1
	pushad
	mov		al, %1
	call	print_char
	popad
%endmacro

%macro PutCharMacro 1
	call	print_char
%endmacro

%macro PrintHexMacro 1
	call	print_hex
%endmacro

%macro PrintString 1
	mov		si, %1
	call	print_string
%endmacro
        
%macro LogString 1
	mov		di, %1
	call	log_string
%endmacro

%if DEBUG
  %define DebugChar(x) DebugCharMacro x
  %define PrintChar(x) PrintCharMacro x
  %define PutChar(x) PutCharMacro
  %define PrintHex(x) PrintHexMacro x
%else
  %define DebugChar(x)
  %define PrintChar(x)
  %define PutChar(x)
  %define PrintHex(x)
%endif
	
;--------------------------------------------------------------------------
; Start of text segment.

    SEGMENT .text

	ORG		kBoot1RelocAddr

;--------------------------------------------------------------------------
; Boot code is loaded at 0:7C00h.
;
start:
    ;
    ; Set up the stack to grow down from kBoot1StackSegment:kBoot1StackAddress.
    ; Interrupts should be off while the stack is being manipulated.
    ;
    cli                             ; interrupts off
    xor		ax, ax                  ; zero ax
    mov		ss, ax                  ; ss <- 0
    mov     sp, kBoot1StackAddress  ; sp <- top of stack
    sti                             ; reenable interrupts

    mov     ds, ax                  ; ds <- 0
    mov     es, ax                  ; es <- 0

    ;
    ; Relocate boot1 code.
    ;
    push	si
    mov		si, kBoot1LoadAddr		; si <- source
    mov		di, kBoot1RelocAddr		; di <- destination
    cld								; auto-increment SI and/or DI registers
    mov		cx, kSectorBytes		; copy 256 words
    rep		movsb					; repeat string move (word) operation
    pop		si
    
    ;
    ; Code relocated, jump to startReloc in relocated location.
    ;
	; FIXME: Is there any way to instruct NASM to compile a near jump
	; using absolute address instead of relative displacement?
	;
	jmpabs	startReloc

;--------------------------------------------------------------------------
; Start execution from the relocated location.
;
startReloc:

    ;
    ; Initializing global variables.
    ;
    mov     eax, [si + part.lba]
    mov     [gPartLBA], eax					; save the current partition LBA offset
    mov     [gBIOSDriveNumber], dl			; save BIOS drive number
	mov		WORD [gMallocPtr], mallocStart	; set free space pointer

    ;
    ; Loading upper 512 bytes of boot1h and HFS+ Volume Header.
    ;
	xor		ecx, ecx						; sector 1 of current partition
	inc		ecx
    mov     al, 2							; read 2 sectors: sector 1 of boot1h + HFS+ Volume Header
    mov     edx, kBoot1Sector1Addr
    call    readLBA

    ;
    ; Initializing more global variables.
    ;
	mov		eax, [kHFSPlusBuffer + HFSPlusVolumeHeader.blockSize]
	bswap	eax								; convert to little-endian
	shr		eax, 9							; convert to sector unit
	mov		[gBlockSize], eax				; save blockSize as little-endian sector unit!

	;
	; Looking for HFSPlus ('H+') or HFSPlus case-sensitive ('HX') signature.
	;
	mov		ax, [kHFSPlusBuffer + HFSPlusVolumeHeader.signature]
	cmp		ax, kHFSPlusCaseSignature
	je		findRootBoot
    cmp     ax, kHFSPlusSignature
    jne     error

;--------------------------------------------------------------------------
; Find stage2 boot file in a HFS+ Volume's root folder.
;
findRootBoot:
	mov		al, kHFSCatalogFileID
	lea		si, [searchCatalogKey]
	lea		di, [kHFSPlusBuffer + HFSPlusVolumeHeader.catalogFile + HFSPlusForkData.extents]
	call	lookUpBTree
	jne		error

	lea		si, [bp + BTree.recordDataPtr]
	mov		si, [si]
	cmp		WORD [si], kHFSPlusFileRecord
	jne		error

;  EAX = Catalog File ID
;  	BX = read size in sectors
;  ECX = file offset in sectors
;  EDX = address of read buffer
;   DI = address of HFSPlusForkData

	;
	; Use the second big-endian double-word as the file length in HFSPlusForkData.logicalSize
	;
	mov		ebx, [si + HFSPlusCatalogFile.dataFork + HFSPlusForkData.logicalSize + 4]
	bswap	ebx									; convert file size to little-endian
	add		ebx, kSectorBytes - 1				; adjust size before unit conversion
	shr		ebx, 9								; convert file size to sector unit
	cmp		bx, kBoot2Sectors					; check if bigger than max stage2 size
	ja		error
	mov		eax, [si + HFSPlusCatalogFile.fileID]
	bswap	eax									; convert fileID to little-endian
	xor		ecx, ecx
	mov		edx, (kBoot2Segment << 4) + kBoot2Address
	lea		di, [si + HFSPlusCatalogFile.dataFork + HFSPlusForkData.extents]
	call	readExtent

%if VERBOSE
	LogString(root_str)
%endif

boot2:

%if DEBUG
	DebugChar ('!')
%endif

%if UNUSED
	;
	; Waiting for a key press.
	;

    mov     ah, 0
    int		0x16
%endif
	mov     ax, 0x1900
    mov     es, ax
	mov     BYTE [es:4], 1

    mov     dl, [gBIOSDriveNumber]			; load BIOS drive number
    jmp     kBoot2Segment:kBoot2Address

error:

%if VERBOSE
    LogString(error_str)
%endif
	
hang:
    hlt
    jmp     hang

;--------------------------------------------------------------------------
; readSectors - Reads more than 127 sectors using LBA addressing.
;
; Arguments:
;   AX = number of 512-byte sectors to read (valid from 1-1280).
;   EDX = pointer to where the sectors should be stored.
;   ECX = sector offset in partition 
;
; Returns:
;   CF = 0  success
;        1 error
;
readSectors:
	pushad
	mov		bx, ax

.loop:
	xor		eax, eax						; EAX = 0
	mov		al, bl							; assume we reached the last block.
	cmp		bx, maxSectorCount				; check if we really reached the last block
	jb		.readBlock						; yes, BX < MaxSectorCount
	mov		al, maxSectorCount				; no, read MaxSectorCount

.readBlock:
	call	readLBA
	sub		bx, ax							; decrease remaning sectors with the read amount
	jz		.exit							; exit if no more sectors left to be loaded
	add		ecx, eax						; adjust LBA sector offset
	shl		ax, 9							; convert sectors to bytes
	add		edx, eax						; adjust target memory location
	jmp		.loop							; read remaining sectors

.exit:
	popad
	ret

;--------------------------------------------------------------------------
; readLBA - Read sectors from a partition using LBA addressing.
;
; Arguments:
;   AL = number of 512-byte sectors to read (valid from 1-127).
;   EDX = pointer to where the sectors should be stored.
;   ECX = sector offset in partition 
;   [bios_drive_number] = drive number (0x80 + unit number)
;
; Returns:
;   CF = 0  success
;        1 error
;
readLBA:
    pushad                          		; save all registers
    push    es								; save ES
    mov     bp, sp                 			; save current SP

    ;
    ; Convert EDX to segment:offset model and set ES:BX
    ;
    ; Some BIOSes do not like offset to be negative while reading
    ; from hard drives. This usually leads to "boot1: error" when trying
    ; to boot from hard drive, while booting normally from USB flash.
    ; The routines, responsible for this are apparently different.
    ; Thus we split linear address slightly differently for these
    ; capricious BIOSes to make sure offset is always positive.
    ;

	mov		bx, dx							; save offset to BX
	and		bh, 0x0f						; keep low 12 bits
	shr		edx, 4							; adjust linear address to segment base
	xor		dl, dl							; mask low 8 bits
	mov		es, dx							; save segment to ES

    ;
    ; Create the Disk Address Packet structure for the
    ; INT13/F42 (Extended Read Sectors) on the stack.
    ;

    ; push    DWORD 0              			; offset 12, upper 32-bit LBA
    push    ds                      		; For sake of saving memory,
    push    ds                      		; push DS register, which is 0.

    add     ecx, [gPartLBA]         		; offset 8, lower 32-bit LBA
    push    ecx

    push    es                      		; offset 6, memory segment

    push    bx                      		; offset 4, memory offset

    xor     ah, ah             				; offset 3, must be 0
    push    ax                      		; offset 2, number of sectors

    push    WORD 16                 		; offset 0-1, packet size

    ;
    ; INT13 Func 42 - Extended Read Sectors
    ;
    ; Arguments:
    ;   AH    = 0x42
    ;   [bios_drive_number] = drive number (0x80 + unit number)
    ;   DS:SI = pointer to Disk Address Packet
    ;
    ; Returns:
    ;   AH    = return status (sucess is 0)
    ;   carry = 0 success
    ;           1 error
    ;
    ; Packet offset 2 indicates the number of sectors read
    ; successfully.
    ;
	mov     dl, [gBIOSDriveNumber]			; load BIOS drive number
	mov     si, sp
	mov     ah, 0x42
	int     0x13

	jc		error

    ;
    ; Issue a disk reset on error.
    ; Should this be changed to Func 0xD to skip the diskette controller
    ; reset?
    ;
;	xor     ax, ax                  		; Func 0
;	int     0x13                    		; INT 13
;	stc                             		; set carry to indicate error

.exit:
    mov     sp, bp                  		; restore SP
    pop     es								; restore ES
    popad
    ret

%if VERBOSE

;--------------------------------------------------------------------------
; Write a string with 'boot1: ' prefix to the console.
;
; Arguments:
;   ES:DI   pointer to a NULL terminated string.
;
; Clobber list:
;   DI
;
log_string:
    pushad

    push	di
    mov		si, log_title_str
    call	print_string

    pop		si
    call	print_string

    popad
    
    ret

;-------------------------------------------------------------------------
; Write a string to the console.
;
; Arguments:
;   DS:SI   pointer to a NULL terminated string.
;
; Clobber list:
;   AX, BX, SI
;
print_string:
    mov     bx, 1                   		; BH=0, BL=1 (blue)

.loop:
    lodsb                           		; load a byte from DS:SI into AL
    cmp     al, 0               			; Is it a NULL?
    je      .exit                   		; yes, all done
    mov     ah, 0xE                 		; INT10 Func 0xE
    int     0x10                    		; display byte in tty mode
    jmp     .loop

.exit:
    ret

%endif ; VERBOSE

%if DEBUG

;--------------------------------------------------------------------------
; Write the 4-byte value to the console in hex.
;
; Arguments:
;   EAX = Value to be displayed in hex.
;
print_hex:
    pushad
    mov     cx, WORD 4
    bswap   eax
.loop:
    push    ax
    ror     al, 4
    call    print_nibble            		; display upper nibble
    pop     ax
    call    print_nibble            		; display lower nibble
    ror     eax, 8
    loop    .loop

%if UNUSED
	mov     al, 10							; carriage return
	call    print_char
	mov     al, 13
	call    print_char
%endif ; UNUSED

    popad
    ret
	
print_nibble:
    and     al, 0x0f
    add     al, '0'
    cmp     al, '9'
    jna     .print_ascii
    add     al, 'A' - '9' - 1
.print_ascii:
    call    print_char
    ret

;--------------------------------------------------------------------------
; getc - wait for a key press
;
getc:
    pushad
    mov     ah, 0
    int		0x16
    popad
    ret

;--------------------------------------------------------------------------
; Write a ASCII character to the console.
;
; Arguments:
;   AL = ASCII character.
;
print_char:
    pushad
    mov     bx, 1                   		; BH=0, BL=1 (blue)
    mov     ah, 0x0e                		; bios INT 10, Function 0xE
    int     0x10                    		; display byte in tty mode
    popad
    ret

%endif ; DEBUG

%if UNUSED

;--------------------------------------------------------------------------
; Convert null terminated string to HFSUniStr255
;
; Arguments:
;   DS:DX   pointer to a NULL terminated string.
;   ES:DI   pointer to result.
;
ConvertStrToUni:
    pushad									; save registers
    push	di								; save DI for unicode string length pointer
    mov		si, dx							; use SI as source string pointer
    xor		ax, ax							; AX = unicode character
    mov		cl, al							; CL = string length

.loop:
    stosw									; store unicode character (length 0 at first run)
    lodsb									; load next character to AL
    inc		cl								; increment string length count
    cmp		al, NULL						; check for string terminator
    jne		.loop
    
    pop		di								; restore unicode string length pointer
    dec		cl								; ignoring terminator from length count
    mov		[di], cl						; save string length
    popad									; restore registers
    ret

%endif ; UNUSED

%if DEBUG

;--------------------------------------------------------------------------
; Print a 32-bit value in hex.
;
; Arguments:
;   EAX = Value to be displayed in hex.
;
PrintHex:
	pushad
	mov     cx, WORD 4
	bswap   eax
.loop:
	push    ax
	ror     al, 4
	call    PrintNibble            		; display upper nibble
	pop     ax
	call    PrintNibble            		; display lower nibble
	ror     eax, 8
	loop    .loop

	mov     al, 10							; carriage return
	call    PrintChar
	mov     al, 13
	call    PrintChar

	popad
	ret

PrintNibble:
	and     al, 0x0f
	add     al, '0'
	cmp     al, '9'
	jna     .print_ascii
	add     al, 'A' - '9' - 1
.print_ascii:
	call    PrintChar
	ret

;--------------------------------------------------------------------------
; Print a ASCII character.
;
; Arguments:
;   AL = ASCII character.
;
PrintChar:
	pushad
	mov     bx, 1                   		; BH=0, BL=1 (blue)
	mov     ah, 0x0e                		; bios INT 10, Function 0xE
	int     0x10                    		; display byte in tty mode
	popad
	ret

%endif ; DEBUG

%if DEBUG

;--------------------------------------------------------------------------
