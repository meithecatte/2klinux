; This Forth implementation is based on jonesforth - https://github.com/nornagon/jonesforth
; Any similarities are probably not accidental.

; The first bootstrap stage of this project is implemented as a bootloader. This bootsector
; implements FAT32, with the assumption that the sector and cluster sizes are both 512 bytes. Long
; file names are not supported, but their presence for files we don't care about is not harmful. All
; disk access is done using EDD, which means problems for very old PCs (like pre-Pentium old) or
; booting from a floppy. Both of these problems don't concern me, like, at all. The FAT partition
; with all the files should be the first physical partition of the drive.

; EBP is always set to the value 0x7C00 to generate shorter instructions for accessing some
; memory locations. Constants that start with a lowercase d represent the offset from 0x7C00, and
; therefore EBP, of some memory address.

; We use a part of the code section as variables after executing it:
;  7C00 -  7C0F -> The EDD disk packet
%define dDiskPacket            0
%define dDiskPacketDestOffset  4
%define dDiskPacketDestSegment 6
%define dDiskPacketLBA         8
;  7C10 -  7C13 -> The currently loaded cluster
%define dCurrentCluster 16
;  7C14 -  7C23 -> Forth variables, all are 4 bytes long
%define dOFFSET 20 ; The address of the next byte KEY will read, relative to FileBuffer
%define dLATEST 24 ; The LFA of the last Forth word defined.
%define dHERE   28 ; The address of the first free byte of Forth memory.
%define dSTATE  32 ; 1 if compiling words, 0 if interpreting.

; The last two partition entries are reused as a buffer for WORD.

; The general memory map looks like this:
;  0000 -  03FF -> Real mode interrupt vector table
;  0400 -  04FF -> The BIOS data area
;  0500 -  14FF -> FORTH return stack
%define FORTHR0 0x1500
;  1500 -  7BFF -> the stack, used as the FORTH parameter stack
;  7C00 -  7DFF -> The MBR - the first part of this file
;  7E00 -  83FF -> 3 sectors loaded from the FAT filesystem - the second part of this file
;  8400 -  85FF -> A buffer for one sector of a file or directory
%define FileBuffer 0x8400
;  8600 -  87FF -> A buffer for one sector of FAT
%define FATBuffer  0x8600
;  8800 -  89FF -> A buffer for the sector with BPB
%define BPBBuffer  0x8800
;  8A00 - 7FFFF -> Unassigned.
%define FORTHMemoryStart 0x8A00
; 80000 - 9FFFF -> Mostly unassigned, but the end is used by the Extended BIOS Data Area. Its size
;                  varies, and this 128 KiB is the maximum
; A0000 - BFFFF -> Video RAM
; C0000 - FFFFF -> ROMs and memory mapped hardware

BITS 16
ORG 0x7C00

; Addresses of the values in the BPB we need to correctly parse the FAT filesystem.
%define BPBReservedSectors BPBBuffer+14
%define BPBFATCount        BPBBuffer+16
%define BPBSectorsPerFAT   BPBBuffer+36
%define BPBRootCluster     BPBBuffer+44

; Constants related to FAT directory entries
%define FATNameLength  11
%define DirAttributes  11
%define DirHighCluster 20
%define DirLowCluster  26
%define DirEntrySize   32

; Selector constants according to the layout of the GDT
%define Selector_Code32 0x08
%define Selector_Code16 0x10
%define Selector_Data   0x18

MBR:
	jmp 0:start
start:
	cli ; Disable the interrupts when setting up the stack - always a good idea
	mov bp, MBR
	mov sp, bp

	xor cx, cx
	mov ss, cx
	mov ds, cx
	mov es, cx

	dec cx ; Intentional integer underflow
	mov fs, cx ; FS is set to 0xFFFF for probing the unloved A20 gate. Unloved for a reason.

	mov byte[..@DiskNumberPatch], dl ; Self modifying code to save a few bytes.
	sti

	mov ax, 0x0003 ; Better to set the video mode explicitly...
	int 0x10

	mov eax, dword[P1LBA] ; Read the first sector of the partition, to get the BPB
	mov di, BPBBuffer ; This is the first instruction that isn't overlapping with variables
	call DiskRead

	; First FAT sector = Partition Start LBA + Reserved Sector Count

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
	; push X / jmp Y is equivalent to call Y / jmp X, but here jmp Y is a noop
	; TL;DR: call FindFileRoot / jmp LoadPartTwo

; FindFileRoot: like FindFile, but look in the root directory of a partition.
FindFileRoot:
	push di
	mov eax, dword[BPBRootCluster]
	call ReadCluster
	pop di
	; fallthrough

; FindFile: treat the currently loaded file as a directory, find the file with a specified name and
; load its first cluster
; Input:
;  DI = pointer to filename
FindFile:
	xor ecx, ecx
	mov [bp+dOFFSET], ecx
	mov cl, 16
	mov si, FileBuffer
.loop:
	mov al, byte[si]
	or al, al
	jz short .notfound
	; normally, you would check if the first byte is 0xE5 (if so, you should skip the entry),
	; but it won't match the filename anyway
	test byte[si+DirAttributes], 0x0e
	jnz short .next
	pushad
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
	popad
	mov ax, [si+DirHighCluster]
	shl eax, 16
	mov ax, [si+DirLowCluster]
	jmp short ReadCluster
.nomatch:
	popad
.next:
	add si, DirEntrySize
	loop short .loop
	push di
	call ReadNextCluster
	pop di
	jnc short FindFile
.notfound:
	mov si, di
	call PrintText
	mov si, FileNotFoundMsg
	jmp short Error

ReadNextCluster:
	mov eax, dword[bp+dCurrentCluster]
	shr eax, 7
	db 0x66, 0x05 ; add eax, imm32
..@FirstFATSectorPatch:
	dd 0

	mov di, FATBuffer
	call DiskRead
	movzx bx, byte[bp+dCurrentCluster]
	shl bl, 1
	shl bx, 1
	mov eax, dword[di+bx]
	cmp eax, 0x0ffffff8
	cmc
	jc short ..@Return

ReadCluster:
	mov dword[bp+dCurrentCluster], eax
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

ReadNextCluster_ErrorOnEOF:
	call ReadNextCluster
	jnc short ..@Return
	mov si, EOFErrorMsg
	jmp short Error

; NULL terminators in filenames are only necessary for error handling
StageZeroFilename:
	db 'STAGENOTBIN', 0

StageOneFilename:
	db 'STAGEONEFRT', 0

DiskErrorMsg:
	db ' disk error', 0

FileNotFoundMsg:
	db ' not found', 0

EOFErrorMsg:
	db 'EOF error', 0

A20ErrorMsg:
	db 'A20 error', 0

LoadPartTwo:
	mov byte[..@ReadClusterDestinationPatch], 0x7E
.loop:
	call ReadNextCluster
	jc A20
	add byte[..@ReadClusterDestinationPatch], 2
	jmp short .loop

	times 446 - ($ - $$) db 0

PartitionTable:
	times 8 db 0

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

GDT:
	dw GDT_End-GDT-1
	dd GDT
	dw 0

	dw 0xffff
	dw 0
	db 0
	db 0x9a
	db 0xcf
	db 0

	dw 0xffff
	dw 0
	db 0
	db 0x9a
	db 0x8f
	db 0

	dw 0xffff
	dw 0
	db 0
	db 0x92
	db 0xcf
	db 0
GDT_End:

KBC_SendCommand:
	in al, 0x64
	test al, 2
	jnz KBC_SendCommand
	pop si
	lodsb
	out 0x64, al
	jmp si

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
	mov eax, [esp]
	mov eax, [eax]
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

	xor eax, eax
	mov dword[ebp+dLATEST], link_FIND
	mov [ebp+dSTATE], eax
	mov ah, FORTHMemoryStart >> 8
	mov [ebp+dHERE], eax
	mov ah, FORTHR0 >> 8
	xchg edi, eax

	jmp short QUIT
DOCOL:
	sub edi, 4
	mov [edi], esi
	pop esi
	NEXT

link_QUIT:
	dw 0
	db 4, 'QUIT'
QUIT:
	call DOCOL
.loop:
	dd BRANCH, .loop

link_R0:
	dw $-link_QUIT
	db 2, 'R0'
R0:
	push dword FORTHR0
	NEXT

link_OFFSET:
	dw $-link_R0
	db 6, 'OFFSET'
OFFSET:
	lea eax, [ebp+dOFFSET]
	push eax
	NEXT

link_LATEST:
	dw $-link_OFFSET
	db 6, 'LATEST'
LATEST:
	lea eax, [ebp+dLATEST]
	push eax
	NEXT

link_HERE:
	dw $-link_LATEST
	db 4, 'HERE'
HERE:
	lea eax, [ebp+dHERE]
	push eax
	NEXT

link_EXIT:
	dw $-link_HERE
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

link_QDUP:
	dw $-link_NROT
	db 4, '?DUP'
QDUP:
	pop eax
	push eax
	or eax, eax
	jz .skip
	push eax
.skip:
	NEXT

link_INC:
	dw $-link_QDUP
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

link_EQ:
	dw $-link_DIVMOD
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

link_AND:
	dw $-link_ZGT
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
	lea edx, [ebp+dHERE]
	mov eax, [edx]
	pop dword[eax]
	add dword[edx], 4
	NEXT

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

link_CCOMMA:
	dw $-link_CFETCH
	db 2, 'C,'
CCOMMA:
	mov edx, [ebp+dHERE]
	pop eax
	mov [edx], al
	inc dword[ebp+dHERE]
	NEXT

link_CMOVE:
	dw $-link_CCOMMA
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
	movzx eax, al
	push eax
	NEXT

doKEY:
	mov ebx, [ebp+dOFFSET]
	cmp bx, 0x200
	jb .nonextcluster

	pushad
	call CallRM
	dw ReadNextCluster_ErrorOnEOF
	popad

	xor ebx, ebx
.nonextcluster:
	mov al, [FileBuffer+ebx]
	inc ebx
	mov [ebp+dOFFSET], ebx
	ret

link_WORD:
	dw $-link_KEY
	db 4, 'WORD'
_WORD:
	call doWORD
	push dword WORDBuffer
	push ecx
	NEXT

doWORD:
	call doKEY
	cmp al, ' '
	jbe _WORD
	xor ecx, ecx
.loop:
	mov [WORDBuffer+ecx], al
	inc ecx
	call doKEY
	cmp al, ' '
	ja .loop
	ret

link_NUMBER:
	dw $-link_WORD
	db 6, 'NUMBER'
NUMBER:
	pop ecx
	pop eax
	xchg esi, eax
	push eax
	call doNUMBER
	pop esi
	push eax
	push ecx
	NEXT

; Parses a number in the base specified by BASE
; Input:
;  ECX = string length
;  ESI = string buffer
; Output:
;  EAX = the number represented in the string buffer
;  ECX = the number of unparsed characters (may indicate a failure)
doNUMBER:
	mov word[.negate_patch], 0x9066 ; two byte nop - assume we don't need to negate
	xor ebx, ebx
	mul ebx ; EAX, EBX and EDX are now 0
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
	ret

link_EMIT:
	dw $-link_NUMBER
	db 4, 'EMIT'
EMIT:
	pop eax
	call CallRM
	dw PrintChar
	NEXT

link_TELL:
	dw $-link_EMIT
	db 4, 'TELL'
TELL:
	pop ecx
	pop eax
	push esi
	xchg eax, esi
.loop:
	lodsb
	call CallRM
	dw PrintChar
	loop .loop
	pop esi
	NEXT

link_CREATE:
	dw $-link_TELL
	db 6, 'CREATE'
CREATE:
	pop ecx
	pop eax
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
	rep movsb
	mov [ebp+dHERE], edi
	pop edi
	pop esi
	NEXT

link_DOCOLCOMMA:
	dw $-link_CREATE
	db 6, 'DOCOL,'
DOCOLCOMMA:
	push edi
	mov edi, [ebp+dHERE]
	mov al, 0xE8
	stosb
	mov eax, DOCOL+4
	sub eax, edi
	stosd
	mov [ebp+dHERE], edi
	pop edi
	NEXT

link_HIDDEN:
	dw $-link_DOCOLCOMMA
	db 6, 'HIDDEN'
HIDDEN:
	pop eax
	add eax, 2
	xor byte[eax], F_IMMED
	NEXT

link_FIND:
	dw $-link_HIDDEN
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
	or edx, edx
	jz .end

	mov al, [edx+2]
	and al, F_HIDDEN|F_LENMASK
	cmp al, cl
	jnz .next

	lea esi, [edx+3]
	mov edi, ebx
	push ecx
	repe cmpsb
	pop ecx
	je .end
.next:
	movzx eax, word[edx]
	sub edx, eax
	jmp .loop
.end:
	pop edi
	pop esi
	ret

link_LBRAC:
	dw $-link_FIND
	db F_IMMED|1, '['
LBRAC:
	mov byte[ebp+dSTATE], 0
	NEXT

link_RBRAC:
	dw $-link_LBRAC
	db 1, ']'
RBRAC:
	mov byte[ebp+dSTATE], 1
	NEXT

link_COLON:
	dw $-link_RBRAC
	db 1, ':'
COLON:
	call DOCOL
	dd _WORD, CREATE, DOCOLCOMMA, LATEST, FETCH, HIDDEN, RBRAC, EXIT

link_SEMICOLON:
	dw $-link_COLON
	db F_IMMED, ';'
SEMICOLON:
	call DOCOL
	dd LIT, EXIT, COMMA, LATEST, FETCH, HIDDEN, LBRAC, EXIT

	times 2048 - ($ - $$) db 0x69
