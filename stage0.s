; This Forth implementation is based on jonesforth - https://github.com/nornagon/jonesforth
; Any similarities are probably not accidental.

; The first bootstrap stage of 2K Linux is implemented as a bootloader. This bootsector implements
; FAT32, with the assumption that the sector and cluster sizes are both 512 bytes. Long file names
; are not supported, but their presence for files we don't care about is not harmful. All disk I/O
; is done using EDD, which means won't work on very old PCs (like pre-Pentium old) or when booting
; from booting from a floppy. Both of these problems don't concern me a lot, primarily because CHS
; addressing isn't the most pleasant to work with. Patches welcome. The FAT partition contains all
; of the necessary source code, and should be the first physical partition of the drive.

; EBP is always set to the value 0x7C00 to generate shorter instructions for accessing some memory
; memory locations. Constants that start with `d` represent an offset from EBP. Almost all of them
; are also defined in image-files/stage1.frt. It is imperative that these values match between the
; two files.

; We use a part of the code section as variables after executing it:
;  7C00 -  7C0F -> The EDD disk packet
%define dDiskPacket            0
%define dDiskPacketDestOffset  4
%define dDiskPacketDestSegment 6
%define dDiskPacketLBA         8
;  7C10 -  7C23 -> Forth variables, all are 4 bytes long. More detail can be found in stage1.frt.
%define dBLK    16 ; The currently loaded cluster
%define dTOIN   20 ; The address of the next byte KEY will read, relative to FileBuffer
%define dLATEST 24 ; The LFA of the last Forth word defined.
%define dHERE   28 ; The address of the first free byte of Forth memory.
%define dSTATE  32 ; 1 if compiling words, 0 if interpreting.
%define dLENGTH 36 ; The number of characters left in the file currently being read

; The last two partition entries are reused as a buffer for WORD.

; The general memory map looks like this:
;  0000 -  03FF -> Real mode interrupt vector table
;  0400 -  04FF -> The BIOS data area
;  0500 -  14FF -> Forth return stack
%define ForthR0 0x1500

;  1500 -  7BFF -> the stack, used as the Forth parameter stack
;  7C00 -  7DFF -> The MBR - the first part of this file
;  7E00 -  83FF -> 3 sectors loaded from the FAT filesystem - the second part of this file
ORG 0x7C00

;  8400 -  85FF -> A buffer for one sector of a file or directory
%define FileBuffer 0x8400

;  8600 -  87FF -> A buffer for one sector of FAT
%define FATBuffer  0x8600

;  8800 -  89FF -> A buffer for the sector with BPB
%define BPBBuffer  0x8800

;  8A00 - 7FFFF -> The Forth memory. This is where the definitions of all words are stored, except
;                  the ones defined in this file. HERE is initialized to point to the beginning of
;                  this memory region.
%define ForthMemoryStart 0x8A00

; 80000 - 9FFFF -> Mostly unassigned, but the end is used by the Extended BIOS Data Area. Its size
;                  varies, and this 128 KiB is the maximum
; A0000 - BFFFF -> Video RAM
; C0000 - FFFFF -> ROMs and memory mapped hardware

; Various constants
%define FATNameLength  11
%define SectorLength   512

; Addresses of the values in the BPB we need to correctly parse the FAT filesystem.
%define BPBReservedSectors BPBBuffer+14
%define BPBFATCount        BPBBuffer+16
%define BPBSectorsPerFAT   BPBBuffer+36
%define BPBRootCluster     BPBBuffer+44

; Likewise, the offsets of important fields in a FAT directory entry.
%define DirAttributes  11
%define DirHighCluster 20
%define DirLowCluster  26
%define DirFileSize    28
%define DirEntrySize   32

; BIOS loads the first sector of the hard drive at 7C00 and, if the boot signature at offset 0x1FE
; matches, jumps here, in 16-bit Real Mode.
BITS 16

MBR:
; While all BIOSes agree about the destination of the jump, the memory segmentation of x86 present
; in Real Mode makes it possible to encode the address in two different ways, i. e. 0000:7C00 (the
; sane option) and 07C0:0000 (the overcomplicated and non-standard option). Because, on x86, jumps
; and calls are relative, the difference is not immediately obvious, which is probably why the bug
; went unnoticed until it was too late. However, trying to write code that could be loaded at more
; than one address without the help of relocation table is tricky. Additionally, the threaded code
; representation commonly used by Forth systems contains absolute addresses, which breaks when the
; load address doesn't match, and trying to make it use relative addresses by modifying NEXT is an
; unnecessarily complex solution to an inherently simple problem - the simplest way to fix this is
; a long jump at the very beginning of the code.
	jmp 0:start
start:
; Since an interrupt can happen at any time, and interrupts use the stack, one has to disable them
; before moving the stack to a controlled location, since it is not an atomic operation. The other
; option is pulling your hair out over mysterious intermittent failures.
	cli
	mov bp, MBR
	mov sp, bp

; Here, we set up the segment registers. All real mode code operates in the 00000-0FFFF range, and
; therefore no values other than zero are necessary...
	xor cx, cx
	mov ss, cx
	mov ds, cx
	mov es, cx

; ... except for probing the A20 gate, for which access to segment FFFF is necessary. Look for the
; Check_A20 routine for more information.

	dec cx
	mov fs, cx

; When BIOS jumps to 0000:7C00, a few valuable values are left in the registers. One of them is of
; particular interest to any developer of a bootloader or any code that works on a similar level -
; the DL register contains the BIOS number of the disk the MBR was loaded from, which is primarily
; used as a parameter to the BIOS disk calls.

; You will see self modifying code in a few places. Every label used to mark these situations uses
; a suffix `Patch` and, perhaps more importantly, the prefix `..@`, which decouples the label from
; the system of global and local labels. Please refer to yasm's documentation for more details.
	mov byte[..@DiskNumberPatch], dl
	sti

; Setting the video mode makes screen output work even if the BIOS leaves the VGA card in graphics
; mode, like some new BIOSes like to do. This also clears the screen from any BIOS messages.

	mov ax, 0x0003
	int 0x10

; Interpreting a FAT filesystem starts with the BIOS Parameter Block, which is stored in the first
; sector of the partition.
	mov eax, dword[P1LBA]
	mov di, BPBBuffer
	call DiskRead

; First FAT sector = Partition Start LBA + Reserved Sector Count
; What follows is the first instruction that isn't overlapping with the variable area at all. The
; EDD packet is stored in the first 16 bytes and therefore it is safe to use much earlier.
	movzx ebx, word[BPBReservedSectors]
	add ebx, dword[P1LBA]
	mov dword[..@FirstFATSectorPatch], ebx

; Cluster Zero LBA = First FAT sector + FAT count * sectors per FAT - 2
	mov eax, dword[BPBSectorsPerFAT]
	movzx ecx, byte[BPBFATCount]
	mul ecx
	add eax, ebx
	dec eax
	dec eax
	mov dword[..@ClusterZeroLBAPatch], eax

	mov di, StageZeroFilename
	push word LoadPartTwo
	; fallthrough
; push X / jmp Y is equivalent to call Y / jmp X, but here jmp Y is a noop, so it was omitted
; TL;DR: call FindFileRoot / jmp LoadPartTwo

; FindFileRoot: like FindFile, but looks in the root directory of the partition, as opposed to the
; one currently loaded.
FindFileRoot:
	push di
	mov eax, dword[BPBRootCluster]
	call ReadCluster
	pop di
	; fallthrough

; FindFile: read the currently loaded file as a directory, find the file with a specified name and
; load its first cluster. Also sets >IN and LENGTH appropriately.
; Input:
;  DI = pointer to filename
FindFile:
	xor ecx, ecx
	mov dword[bp+dTOIN], ecx
	mov cl, SectorLength / DirEntrySize
	mov si, FileBuffer
.loop:
	mov al, byte[si]
	or al, al
	jz short NotFoundError
; usually, one should check whether the first byte is 0xE5 (if so, you should skip the entry), but
; it won't won't match the filename anyway
	test byte[si+DirAttributes], 0x0e
	jnz short .next
	pusha
	mov cl, FATNameLength
.cmploop:
	lodsb
	cmp al, 'a'
	jb .noconvert
	cmp al, 'z'
	ja .noconvert
	sub al, 'a' - 'A'
.noconvert:
	cmp al, byte[di]
	jne short .nomatch
	inc di
	loop short .cmploop
	popa
	mov eax, dword[si+DirFileSize]
	mov dword[bp+dLENGTH], eax
; Load the doubleword two bytes earlier to make the desired part land in the most significant word
	mov eax, dword[si+DirHighCluster-2]
	mov ax, word[si+DirLowCluster]
	jmp short ReadCluster
.nomatch:
	popa
.next:
	add si, DirEntrySize
	loop short .loop
	push di
	call ReadNextCluster
	pop di
	jnc short FindFile
NotFoundError:
	mov si, di
	call PrintText
	mov si, NotFoundMsg
	jmp short Error

ReadNextCluster:
	mov eax, dword[bp+dBLK]
	shr eax, 7
	db 0x66, 0x05 ; add eax, imm32
..@FirstFATSectorPatch:
	dd 0

	mov di, FATBuffer
	call DiskRead
	movzx bx, byte[bp+dBLK]
	shl bl, 1
	shl bx, 1
	mov eax, dword[di+bx]
	cmp eax, 0x0ffffff8
	cmc
	jc short ..@Return

ReadCluster:
	mov dword[bp+dBLK], eax
	db 0x66, 0x05 ; add eax, imm32
..@ClusterZeroLBAPatch:
	dd 0

	db 0xBF, 0x00 ; mov di, imm16
..@ReadClusterDestinationPatch:
	db FileBuffer>>8
	; fallthrough

; DiskRead: read a sector
; Input:
;   EAX = LBA
;   DI  = output buffer
DiskRead:
	mov dword[bp+dDiskPacketLBA], eax
	xor eax, eax
	mov dword[bp+dDiskPacketLBA+4], eax
	mov dword[bp+dDiskPacket], 0x10010
	mov word[bp+dDiskPacketDestOffset], di
	mov word[bp+dDiskPacketDestSegment], ax
	db 0xB2 ; mov dl, imm8
..@DiskNumberPatch:
	db 0xFF
	mov ah, 0x42
	mov si, bp
	int 0x13
	jnc short ..@Return

	mov al, ah
	call PrintHexByte
	mov si, DiskErrorMsg
	; fallthrough
Error:
	call PrintText
	; fallthrough
Halt:
	cli
	hlt

PrintHexByte:
	push ax
	shr al, 4
	call PrintHexDigit
	pop ax
	and al, 0x0f
	; fallthrough
PrintHexDigit:
	add al, '0'
	cmp al, '9'
	jbe PrintChar
	add al, 'A' - '0' - 0x0A
	; fallthrough
PrintChar:
	pusha
	cmp al, 10
	jne .skipcr
	mov al, 13
	call PrintChar
	mov al, 10
.skipcr:
	xor bx, bx
	mov ah, 0x0e
	int 0x10
	popa
..@Return:
	ret

PrintText:
	lodsb
	or al, al
	jz short ..@Return
	call PrintChar
	jmp short PrintText

; NULL terminators in filenames are only necessary for error handling
StageZeroFilename:
	db 'STAGENOTBIN', 0

StageOneFilename:
	db 'STAGE1  FRT', 0

DiskErrorMsg:
	db ' IOERR', 0

NotFoundMsg:
	db ' NOTFOUND', 0

A20ErrorMsg:
	db 'ERRA20', 0

EOFMessage:
	db 'EOF!',0

GDT:
	dw GDT_End-GDT-1
	dd GDT
	dw 0

%define Selector_Code16 0x08
	dw 0xffff
	dw 0
	db 0
	db 0x9a
	db 0x8f
	db 0

%define Selector_Code32 0x10
	dw 0xffff
	dw 0
	db 0
	db 0x9a
	db 0xcf
	db 0

%define Selector_Data   0x18
	dw 0xffff
	dw 0
	db 0
	db 0x92
	db 0xcf
	db 0
GDT_End:

LoadPartTwo:
	mov byte[..@ReadClusterDestinationPatch], 0x7E
.loop:
	call ReadNextCluster
	jc short A20
	add byte[..@ReadClusterDestinationPatch], 2
	jmp short .loop

KBC_SendCommand:
	in al, 0x64
	test al, 2
	jnz KBC_SendCommand
	pop si
	lodsb
	out 0x64, al
	jmp si

MBR_FREESPACE EQU 446 - ($ - $$)
	times MBR_FREESPACE db 0

PartitionTable:
	dw MBR_FREESPACE ; these bytes will be overwritten by the partition table
	dw REST_FREESPACE ; extracted by the build script to show the amount of free space available
	times 4 db 0

P1LBA:      dd 0
P1Length:   dd 0

	times 16 db 0

WORDBuffer:

	times 32 db 0

	dw 0xaa55

A20:
	call Check_A20
	mov ax, 0x2401
	int 0x15
	call Check_A20

	call KBC_SendCommand
	db 0xAD

	call KBC_SendCommand
	db 0xD0

.readwait:
	in al, 0x64
	test al, 1
	jz .readwait

	in al, 0x60
	push ax

	call KBC_SendCommand
	db 0xD1

	pop ax
	or al, 2
	out 0x60, al

	call KBC_SendCommand
	db 0xAE

	call Check_A20
	in al, 0x92
	or al, 2
	and al, 0xfe
	out 0x92, al
	call Check_A20
	mov si, A20ErrorMsg
	jmp Error

Check_A20:
	; we have set DS to 0 and FS to 0xFFFF and the very beginning
	cli
	mov si, 0x7dfe
.loop:
	mov al, byte[si]
	inc byte[fs:si+0x10]
	wbinvd
	cmp al, byte[si]
	jz .ok
	loop .loop
	ret
.ok:
	pop ax ; discard the return address
	push dword PM_Entry-2
	jmp short GoPM

BITS 32
CallRM:
	xchg ebp, eax
	mov eax, dword[esp]
	mov eax, dword[eax]
	jmp Selector_Code16:.code16
BITS 16
.code16:
	push word GoPM
	push ax
	mov eax, cr0
	and al, 0xfe
	mov cr0, eax
	jmp 0:.rmode
.rmode:
	xor ax, ax
	mov ds, ax
	mov es, ax
	mov ss, ax
	xchg eax, ebp
	mov bp, MBR
	sti
	ret
GoPM:
	cli
	lgdt [GDT]
	mov ebp, eax
	mov eax, cr0
	or al, 1
	mov cr0, eax
	jmp Selector_Code32:.code32
BITS 32
.code32:
	mov ax, Selector_Data
	mov ds, ax
	mov es, ax
	mov ss, ax
	movzx esp, sp
	add dword[esp], 2
	mov eax, ebp
	mov ebp, MBR
	ret

; Here is where the actual Forth implementation starts. In contrast to jonesforth, we are using
; direct threaded code. Also, the link fields in the dictionary are relative.
%macro NEXT 0
	lodsd
	jmp eax
%endmacro

%define F_IMMED   0x80
%define F_HIDDEN  0x20
%define F_LENMASK 0x1f

PM_Entry:
	mov di, StageOneFilename
	call CallRM
	dw FindFileRoot

	mov dword[ebp+dLATEST], LATESTInitialValue

	xor eax, eax
	mov dword[ebp+dSTATE], eax

	mov ah, ForthMemoryStart >> 8
	mov dword[ebp+dHERE], eax

	mov ah, ForthR0 >> 8
	xchg edi, eax
	; fallthrough

; Clear the return stack and invoke the INTERPRETer repeatedly
QUIT:
	call DOCOL
.loop:
	dd INTERPRET
	dd BRANCH, .loop

DOCOL:
	sub edi, 4
	mov [edi], esi
	pop esi
	NEXT

; ( -- )
; Stop executing the current word and continue executing its callee. Appended automatically by ; at
; the end of every definition, but may be used explicitly, usually in conditionals
link_EXIT:
	dw 0
	db 4, 'EXIT'
EXIT:
	mov esi, [edi]
	add edi, 4
	NEXT

link_LIT:
	dw $-link_EXIT
	db 3, 'LIT'
LIT:
	lodsd
	push eax
	NEXT

link_DROP:
	dw $-link_LIT
	db 4, 'DROP'
DROP:
	pop eax
	NEXT

link_SWAP:
	dw $-link_DROP
	db 4, 'SWAP'
SWAP:
	pop eax
	pop ebx
	push eax
	push ebx
	NEXT

link_DUP:
	dw $-link_SWAP
	db 3, 'DUP'
DUP:
	push dword[esp]
	NEXT

link_OVER:
	dw $-link_DUP
	db 4, 'OVER'
OVER:
	push dword[esp+4]
	NEXT

link_ROT:
	dw $-link_OVER
	db 3, 'ROT'
ROT:
	pop eax
	pop ebx
	pop ecx
	push ebx
	push eax
	push ecx
	NEXT

link_NROT:
	dw $-link_ROT
	db 4, '-ROT'
NROT:
	pop eax
	pop ebx
	pop ecx
	push eax
	push ecx
	push ebx
	NEXT

link_INC:
	dw $-link_NROT
	db 2, '1+'
_INC:
	inc dword[esp]
	NEXT

link_DEC:
	dw $-link_INC
	db 2, '1-'
_DEC:
	dec dword[esp]
	NEXT

link_4INC:
	dw $-link_DEC
	db 2, '4+'
_4INC:
	add dword[esp], 4
	NEXT

link_4DEC:
	dw $-link_4INC
	db 2, '4-'
_4DEC:
	sub dword[esp], 4
	NEXT

link_ADD:
	dw $-link_4DEC
	db 1, '+'
_ADD:
	pop eax
	add dword[esp], eax
	NEXT

link_SUB:
	dw $-link_ADD
	db 1, '-'
_SUB:
	pop eax
	sub dword[esp], eax
	NEXT

link_MUL:
	dw $-link_SUB
	db 1, '*'
_MUL:
	pop ecx
	pop eax
	imul ecx
	push eax
	NEXT

link_DIVMOD:
	dw $-link_MUL
	db 4, '/MOD'
DIVMOD:
	pop ecx
	pop eax
	cdq
	idiv ecx
	push edx
	push eax
	NEXT

link_UDIVMOD:
	dw $-link_DIVMOD
	db 5, 'U/MOD'
UDIVMOD:
	pop ecx
	pop eax
	xor edx, edx
	div ecx
	push edx
	push eax
	NEXT

link_EQ:
	dw $-link_UDIVMOD
	db 1, '='
EQ:
	pop ecx
	pop ebx
	xor eax, eax
	cmp ebx, ecx
	setne al
	dec eax
	push eax
	NEXT

link_LT:
	dw $-link_EQ
	db 1, '<'
LT:
	pop ecx
	pop ebx
	xor eax, eax
	cmp ebx, ecx
	setnl al
	dec eax
	push eax
	NEXT

link_GT:
	dw $-link_LT
	db 1, '>'
GT:
	pop ecx
	pop ebx
	xor eax, eax
	cmp ebx, ecx
	setng al
	dec eax
	push eax
	NEXT

link_ZEQ:
	dw $-link_GT
	db 2, '0='
ZEQ:
	pop ebx
	xor eax, eax
	test ebx, ebx
	setne al
	dec eax
	push eax
	NEXT

link_ZLT:
	dw $-link_ZEQ
	db 2, '0<'
ZLT:
	pop ebx
	xor eax, eax
	test ebx, ebx
	setnl al
	dec eax
	push eax
	NEXT

link_ZGT:
	dw $-link_ZLT
	db 2, '0>'
ZGT:
	pop ebx
	xor eax, eax
	test ebx, ebx
	setng al
	dec eax
	push eax
	NEXT

link_ULT:
	dw $-link_ZGT
	db 2, 'U<'
ULT:
	pop ecx
	pop ebx
	xor eax, eax
	cmp ebx, ecx
	setnb al
	dec eax
	push eax
	NEXT

link_UGT:
	dw $-link_ULT
	db 2, 'U>'
UGT:
	pop ecx
	pop ebx
	xor eax, eax
	cmp ebx, ecx
	setna al
	dec eax
	push eax
	NEXT

link_AND:
	dw $-link_UGT
	db 3, 'AND'
_AND:
	pop eax
	and dword[esp], eax
	NEXT

link_OR:
	dw $-link_AND
	db 2, 'OR'
_OR:
	pop eax
	or dword[esp], eax
	NEXT

link_XOR:
	dw $-link_OR
	db 3, 'XOR'
_XOR:
	pop eax
	xor dword[esp], eax
	NEXT

link_INVERT:
	dw $-link_XOR
	db 6, 'INVERT'
INVERT:
	not dword[esp]
	NEXT

link_LSHIFT:
	dw $-link_INVERT
	db 6, 'LSHIFT'
LSHIFT:
	pop ecx
	shl dword[esp], cl
	NEXT

link_RSHIFT:
	dw $-link_LSHIFT
	db 6, 'RSHIFT'
RSHIFT:
	pop ecx
	shr dword[esp], cl
	NEXT

link_STORE:
	dw $-link_RSHIFT
	db 1, '!'
STORE:
	pop ebx
	pop eax
	mov [ebx], eax
	NEXT

link_FETCH:
	dw $-link_STORE
	db 1, '@'
FETCH:
	pop eax
	mov eax, [eax]
	push eax
	NEXT

link_ADDSTORE:
	dw $-link_FETCH
	db 2, '+!'
ADDSTORE:
	pop ebx
	pop eax
	add [ebx], eax
	NEXT

link_SUBSTORE:
	dw $-link_ADDSTORE
	db 2, '-!'
SUBSTORE:
	pop ebx
	pop eax
	sub [ebx], eax
	NEXT

link_COMMA:
	dw $-link_SUBSTORE
	db 1, ','
COMMA:
	pop eax
	call doCOMMA
	NEXT

doCOMMA:
	lea edx, [ebp+dHERE]
	mov ebx, [edx]
	mov [ebx], eax
	add dword[edx], 4
	ret

link_CSTORE:
	dw $-link_COMMA
	db 2, 'C!'
CSTORE:
	pop ebx
	pop eax
	mov [ebx], al
	NEXT

link_CFETCH:
	dw $-link_CSTORE
	db 2, 'C@'
CFETCH:
	pop eax
	movzx eax, byte[eax]
	push eax
	NEXT

link_CMOVE:
	dw $-link_CFETCH
	db 5, 'CMOVE'
_CMOVE:
	push esi
	push edi
	mov ecx, [esp+8]
	mov edi, [esp+12]
	mov esi, [esp+16]
	rep movsb
	pop edi
	pop esi
	add esp, 12
	NEXT

link_TOR:
	dw $-link_CMOVE
	db 2, '>R'
TOR:
	pop eax
	sub edi, 4
	mov [edi], eax
	NEXT

link_FROMR:
	dw $-link_TOR
	db 2, 'R>'
FROMR:
	push dword[edi]
	add edi, 4
	NEXT

link_RPEEK:
	dw $-link_FROMR
	db 2, 'R@'
RPEEK:
	push dword[edi]
	NEXT

link_RDROP:
	dw $-link_RPEEK
	db 5, 'RDROP'
RDROP:
	add edi, 4
	NEXT

link_RPSTORE:
	dw $-link_RDROP
	db 3, 'RP!'
RPSTORE:
	pop edi
	NEXT

link_RPFETCH:
	dw $-link_RPSTORE
	db 3, 'RP@'
RPFETCH:
	push edi
	NEXT

link_SPSTORE:
	dw $-link_RPFETCH
	db 3, 'SP!'
SPSTORE:
	pop esp
	NEXT

link_SPFETCH:
	dw $-link_SPSTORE
	db 3, 'SP@'
SPFETCH:
	mov eax, esp
	push eax
	NEXT

link_BRANCH:
	dw $-link_SPFETCH
	db 6, 'BRANCH'
BRANCH:
	lodsd
	xchg esi, eax
	NEXT

link_0BRANCH:
	dw $-link_BRANCH
	db 7, '0BRANCH'
_0BRANCH:
	lodsd
	pop ebx
	or ebx, ebx
	jnz .dontbranch
	xchg esi, eax
.dontbranch:
	NEXT

link_KEY:
	dw $-link_0BRANCH
	db 3, 'KEY'
KEY:
	call doKEY
	push eax
	NEXT

doKEY:
	mov eax, [ebp+dLENGTH]
	or eax, eax
	jz .end
	dec dword[ebp+dLENGTH]

	mov ebx, dword[ebp+dTOIN]
	cmp bx, 0x200
	jb .nonextcluster

	pushad
	call CallRM
	dw ReadNextCluster
	popad

	xor ebx, ebx
.nonextcluster:
	xor eax, eax
	mov al, byte[FileBuffer+ebx]
	inc ebx
	mov dword[ebp+dTOIN], ebx
.end:
	ret

link_WORD:
	dw $-link_KEY
	db 4, 'WORD'
_WORD:
	call doWORD
	push eax
	push ecx
	NEXT

doWORD:
	call doKEY
	or al, al
	jz .eof
	cmp al, ' '
	jbe doWORD
	xor ecx, ecx
	mov edx, WORDBuffer
.loop:
	mov [edx+ecx], al
	inc ecx
	call doKEY
	cmp al, ' '
	ja .loop

	; ungetc
	dec dword[ebp+dTOIN]
	inc dword[ebp+dLENGTH]

	mov byte[edx+ecx], 0
	xchg edx, eax
	ret
.eof:
	mov esi, EOFMessage
	call CallRM
	dw Error

link_NUMBER:
	dw $-link_WORD
	db 6, 'NUMBER'
NUMBER:
	pop ecx
	pop eax
	call doNUMBER
	push eax
	push ecx
	jmp short ..@NEXT

; Parses a number
; Input:
;  ECX = string length
;  EAX = string buffer
; Output:
;  EAX = the number represented in the string buffer
;  ECX = the number of unparsed characters (may indicate a failure)
doNUMBER:
	xchg eax, esi
	push eax
	mov word[.negate_patch], 0x9066 ; two byte nop - assume we don't need to negate
	xor ebx, ebx
	mul ebx      ; zeroes EAX, EBX and EDX
	mov dl, 10
	mov bl, [esi]
	cmp bl, '$'
	jne .nothex
	mov dl, 16
	inc esi
	dec ecx
.nothex:
	cmp bl, '-'
	jne .loop
	mov word[.negate_patch], 0xd8f7
	inc esi
	dec ecx
.loop:
	mov bl, [esi]
	sub bl, '0'
	jb .end
	cmp bl, 9
	jbe .gotdigit
	sub bl, 'A' - '0'
	jb .end
	add bl, 10
.gotdigit:
	cmp bl, dl
	jae .end
	push edx
	mul edx
	add eax, ebx
	pop edx
	inc esi
	loop .loop
.end:
.negate_patch:
	dw 0xd8f7 ; either `neg eax' or `nop'
	pop esi
	ret

link_EMIT:
	dw $-link_NUMBER
	db 4, 'EMIT'
EMIT:
	pop eax
	call CallRM
	dw PrintChar
	jmp short ..@NEXT

; ( cluster -- )
; A thin wrapper around ReadCluster
link_LOAD:
	dw $-link_EMIT
	db 4, 'LOAD'
LOAD:
	pop eax
	pushad
	call CallRM
	dw ReadCluster
	popad
..@NEXT:
	NEXT

; ( name-pointer -- )
; A thin wrapper around FindFile
link_FILE:
	dw $-link_LOAD
	db 4, 'FILE'
FILE:
	pop eax
	xchg edi, eax
	pushad
	call CallRM
	dw FindFile
	popad
	xchg edi, eax
	jmp short ..@NEXT

link_CREATE:
	dw $-link_FILE
	db 11, 'CREATE-BARE'
CREATE:
	pop ecx
	pop eax
	call doCREATE
	NEXT

doCREATE:
	push esi
	push edi
	xchg esi, eax
	mov edi, [ebp+dHERE]
	mov eax, edi
	sub eax, [ebp+dLATEST]
	mov [ebp+dLATEST], edi
	stosw
	mov al, cl
	stosb
	and cl, F_LENMASK
	rep movsb
	mov [ebp+dHERE], edi
	pop edi
	pop esi
	ret

link_FIND:
	dw $-link_CREATE
	db 4, 'FIND'
FIND:
	pop ecx
	pop ebx
	call doFIND
	push edx
	NEXT


; Input:
;  ECX = name length
;  EBX = name pointer
; Output:
;  EDX = word pointer, or 0 if not found
doFIND:
	push esi
	push edi

	mov edx, [ebp+dLATEST]
.loop:
	mov al, [edx+2]
	and al, F_HIDDEN|F_LENMASK
	cmp al, cl
	jnz .next

	lea esi, [edx+3]
	mov edi, ebx
	push ecx
	repe cmpsb
	pop ecx
	je .found
.next:
	movzx eax, word[edx]
	or eax, eax
	jz short .notfound
	sub edx, eax
	jmp .loop
.notfound:
	xor edx, edx
.found:
	pop edi
	pop esi
	ret

link_COLON:
	dw $-link_FIND
	db 1, ':'
COLON:
	call doWORD
	or cl, F_HIDDEN
	call doCREATE

	push edi
	mov edi, [ebp+dHERE]
	mov al, 0xE8
	stosb
	mov eax, DOCOL-4 ; eax = DOCOL - (edi + 4)
	sub eax, edi
	stosd
	mov [ebp+dHERE], edi
	pop edi

	xor eax, eax
	dec eax
	jmp short ChangeState

link_SEMICOLON:
	dw $-link_COLON
	db F_IMMED|1, ';'
SEMICOLON:
	mov eax, EXIT
	call doCOMMA

	mov eax, [ebp+dLATEST]
	and byte[eax+2], ~F_HIDDEN

	xor eax, eax
ChangeState:
	mov [ebp+dSTATE], eax
	NEXT

link_INTERPRET:
	dw $-link_SEMICOLON
	db 9, 'INTERPRET'
INTERPRET:
	call doWORD
	mov ebx, eax
	call doFIND
	or edx, edx
	jz short .handle_number ; if the word isn't found, assume it's a number

	add edx, 2
	mov cl, [edx]
	movzx eax, cl
	and al, F_LENMASK
	add edx, eax
	inc edx

	xchg eax, edx
	mov ebx, [ebp+dSTATE]
	or ebx, ebx
	jz short .interpret ; if we're in interpreting mode, execute the word

	and cl, F_IMMED
	jz short .comma_next

.interpret:
	jmp eax

.handle_number:
	mov eax, WORDBuffer
	call doNUMBER

	or ecx, ecx
	jnz .error

	mov ebx, [ebp+dSTATE]
	or ebx, ebx
	jz .interpret_number

	push eax
	mov eax, LIT
	call doCOMMA
	pop eax

.comma_next:
	call doCOMMA
	NEXT

.interpret_number:
	push eax
	NEXT

.error:
	mov di, WORDBuffer
	call CallRM
	dw NotFoundError

LATESTInitialValue EQU link_INTERPRET

REST_FREESPACE EQU 2048 - ($ - $$)
	times REST_FREESPACE db 0x00
