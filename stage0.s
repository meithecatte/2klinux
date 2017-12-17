; This Forth implementation is based on jonesforth - https://github.com/nornagon/jonesforth
; Any similarities are probably not accidental.

; The first bootstrap stage of this project is implemented as a bootloader. This bootsector
; implements FAT32, with the assumption that the sector and cluster sizes are both 512 bytes. Long
; file names are not supported, but their presence for files we don't care about is not harmful.
; All disk access is done using EDD, which means problems for very old PCs (like pre-Pentium old)
; or booting from a floppy. Both of these problems don't concern me, like, at all. The FAT partition
; with all the files should be the first physical partition of the drive.

; EBP is constantly loaded with the value 0x7C00 to generate shorter instructions for accessing
; some memory locations. Constants that start with a lowercase d represent the offset from 0x7C00, and therefore EBP, of some
; memory address.

; We use a part of the code section as variables after executing it:
;  7C00 -  7C0F -> The EDD disk packet
%define dDiskPacket            0
%define dDiskPacketDestOffset  4
%define dDiskPacketDestSegment 6
%define dDiskPacketLBA         8
;  7C10 -  7C13 -> The LBA of the first FAT sector
%define dFATStart 16
;  7C14 -  7C17 -> The currently loaded cluster
%define dCurrentCluster 20
;  7C18 -  7C2B -> Forth variables, all are 4 bytes long
; if any of these addresses gets modified, make sure to change the stage 1 forth file too
%define dOFFSET 24 ; The address of the next byte KEY will read, relative to the beginning of the 512 byte buffer.
%define dLATEST 28 ; The LFA of the last FORTH word defined.
%define dHERE   32 ; The address of the first free byte of memory. This is where any new FORTH definitions will be put.
%define dBASE   36 ; The number base of the digits parsed by NUMBER. Starts at 16.
%define dSTATE  40 ; 1 if compiling words, 0 if interpreting.

; The last two partition entries are reused as a buffer for WORD

; The general memory map looks like this:
;  0000 -  03FF -> Real mode interrupt vector table
;  0400 -  04FF -> The BIOS data area
;  0500 -  14FF -> FORTH return stack
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
ORG 0x7c00

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

%define Selector_Code32 0x08
%define Selector_Code16 0x10
%define Selector_Data   0x18

; Forth related constants and macros
%define F_IMMED   0x80
%define F_HIDDEN  0x20
%define F_LENMASK 0x1f

%macro NEXT 0
	lodsd
	jmp [eax]
%endmacro

MBR:
	jmp 0:start
start:
	cli                             ; Disable the interrupts when setting up the stack
	xor cx, cx
	mov bp, MBR
	mov sp, bp
	mov ss, cx
	mov ds, cx
	mov es, cx
	dec cx
	mov fs, cx                                ; FS is set to 0xFFFF for probing the A20 Gate. Yuck.
	mov byte[..@DiskReadPatch], dl; Self modifying code, because why not
	sti

	; The initialization code above is only used once. When we get here we can use that memory
	; as data. All variables except the FORTH ones are free for use.

	mov ax, 0x0003                  ; Better to set the video mode explicitly...
	int 0x10

	mov eax, dword[P1LBA]           ; Read the first sector of the partition, to get the BPB
	mov di, BPBBuffer
	call DiskRead

	movzx eax, word[BPBReservedSectors]
	add eax, dword[P1LBA]
	mov dword[bp+dFATStart], eax

	mov di, StageZeroFilename
	push word LoadPartTwo
	; fallthrough
FindFileRoot:
	push di
	mov eax, dword[BPBRootCluster]
	call ReadCluster
	pop di
FindFile:
	xor cx, cx
	mov [bp+dOFFSET], cx
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
	add eax, dword[bp+dFATStart]
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
	mov ebx, eax
	mov eax, dword[BPBSectorsPerFAT]
	movzx ecx, byte[BPBFATCount]
	mul ecx
	add eax, dword[bp+dFATStart]
	dec eax
	dec eax
	add eax, ebx

	db 0xBF, 0x00 ; mov di, FileBuffer at first, but the address is patched at runtime when loading 7E00-83FF
..@ReadClusterPatch:
	db FileBuffer>>8
	; fallthrough

DiskRead:
	mov dword[bp+dDiskPacketLBA], eax
	xor eax, eax
	mov dword[bp+dDiskPacketLBA+4], eax
	mov dword[bp+dDiskPacket], 0x10010
	mov word[bp+dDiskPacketDestOffset], di
	mov word[bp+dDiskPacketDestSegment], ax
	db 0xB2 ; mov dl, (patched at runtime)
..@DiskReadPatch:
	db 0xFF
	mov ah, 0x42
	mov si, bp
	int 0x13
	jnc short ..@Return

	mov al, ah
	call PrintByte
	mov si, DiskErrorMsg
	; fallthrough
Error:
	call PrintText
	; fallthrough
Halt:
	hlt
	jmp short Halt

PrintByte:
	push ax
	shr al, 4
	call PrintNibble
	pop ax
	and al, 0x0f
	; fallthrough
PrintNibble:
	add al, '0'
	cmp al, '9'
	jbe PrintChar
	add al, 'A' - '0' - 0x0A
	; fallthrough
PrintChar:
	pusha
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

StageZeroFilename:
	db 'STAGENOTBIN', 0

DiskErrorMsg:
	db ' disk error', 0

FileNotFoundMsg:
	db ' not found', 0

LoadPartTwo:
	mov byte[..@ReadClusterPatch], 0x7E
.loop:
	call ReadNextCluster
	jc A20
	add byte[..@ReadClusterPatch], 2
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
	cli
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

A20ErrorMsg:
	db 'A20 error', 0

GDT:
	dw 31
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

	push dword PM_Entry-2
	jmp short GoPM

BITS 32
CallRM:
	mov ebp, eax
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
	sti
	xor ax, ax
	mov ds, ax
	mov es, ax
	mov ss, ax
	mov eax, ebp
	mov bp, MBR
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

StageOneFilename:
	db 'STAGEONEFRT', 0

BITS 32
PM_Entry:
	mov di, StageOneFilename
	call CallRM
	dw FindFileRoot

	xor eax, eax
	mov [ebp+dLATEST], eax
	mov [ebp+dSTATE], eax
	mov [ebp+dBASE], eax
	mov byte[ebp+dBASE], 0x10
	mov ah, FORTHMemoryStart >> 8
	mov [ebp+dHERE], eax
	mov ah, 0x15
	xchg edi, eax

	mov eax, QUIT
	; fallthrough
DOCOL:
	sub edi, 4
	mov [edi], esi
	add eax, 4 ; skip codeword
	xchg esi, eax
	NEXT

QUIT:
link_EXIT:
	dd 0
	db 4, 'EXIT'
EXIT:
	dd code_EXIT
code_EXIT:
	mov esi, [edi]
	add edi, 4
	NEXT

link_LIT:
	dd link_EXIT
	db 3, 'LIT'
LIT:
	dd code_LIT
code_LIT:
	lodsd
	push eax
	NEXT

link_DROP:
	dd link_LIT
	db 4, 'DROP'
	dd code_DROP
code_DROP:
	pop eax
	NEXT

link_SWAP:
	dd link_DROP
	db 4, 'SWAP'
	dd code_SWAP
code_SWAP:
	pop eax
	pop ebx
	push eax
	push ebx
	NEXT

link_DUP:
	dd link_SWAP
	db 3, 'DUP'
	dd code_DUP
code_DUP:
	push dword[esp]
	NEXT

link_OVER:
	dd link_DUP
	db 4, 'OVER'
	dd code_OVER
code_OVER:
	push dword[esp+4]
	NEXT

link_ROT:
	dd link_OVER
	db 3, 'ROT'
	dd code_ROT
code_ROT:
	pop eax
	pop ebx
	pop ecx
	push ebx
	push eax
	push ecx
	NEXT

link_NROT:
	dd link_ROT
	db 4, '-ROT'
	dd code_NROT
code_NROT:
	pop eax
	pop ebx
	pop ecx
	push eax
	push ecx
	push ebx
	NEXT

link_INC:
	dd link_NROT
	db 2, '1+'
	dd code_INC
code_INC:
	inc dword[esp]
	NEXT

link_DEC:
	dd link_INC
	db 2, '1-'
	dd code_DEC
code_DEC:
	dec dword[esp]
	NEXT

link_4INC:
	dd link_DEC
	db 2, '4+'
	dd code_4INC
code_4INC:
	add dword[esp], 4
	NEXT

link_4DEC:
	dd link_4INC
	db 2, '4-'
	dd code_4DEC
code_4DEC:
	sub dword[esp], 4
	NEXT

link_ADD:
	dd link_4DEC
	db 1, '+'
	dd code_ADD
code_ADD:
	pop eax
	add dword[esp], eax
	NEXT

link_SUB:
	dd link_ADD
	db 1, '-'
	dd code_SUB
code_SUB:
	pop eax
	sub dword[esp], eax
	NEXT

link_MUL:
	dd link_SUB
	db 1, '*'
	dd code_MUL
code_MUL:
	pop ecx
	pop eax
	imul ecx
	push eax
	NEXT

link_DIVMOD:
	dd link_MUL
	db 4, '/MOD'
	dd code_DIVMOD
code_DIVMOD:
	pop ecx
	pop eax
	cdq
	idiv ecx
	push edx
	push eax
	NEXT

link_EQ:
	dd link_DIVMOD
	db 1, '='
	dd code_EQ
code_EQ:
	pop ecx
	pop ebx
	xor eax, eax
	cmp ebx, ecx
	setne al
	dec eax
	push eax
	NEXT

link_LT:
	dd link_EQ
	db 1, '<'
	dd code_LT
code_LT:
	pop ecx
	pop ebx
	xor eax, eax
	cmp ebx, ecx
	setnl al
	dec eax
	push eax
	NEXT

link_GT:
	dd link_LT
	db 1, '>'
	dd code_GT
code_GT:
	pop ecx
	pop ebx
	xor eax, eax
	cmp ebx, ecx
	setng al
	dec eax
	push eax
	NEXT

link_ZEQ:
	dd link_GT
	db 2, '0='
	dd code_ZEQ
code_ZEQ:
	pop ebx
	xor eax, eax
	test ebx, ebx
	setne al
	dec eax
	push eax
	NEXT

link_ZLT:
	dd link_ZEQ
	db 2, '0<'
	dd code_ZLT
code_ZLT:
	pop ebx
	xor eax, eax
	test ebx, ebx
	setnl al
	dec eax
	push eax
	NEXT

link_ZGT:
	dd link_ZLT
	db 2, '0>'
	dd code_ZGT
code_ZGT:
	pop ebx
	xor eax, eax
	test ebx, ebx
	setng al
	dec eax
	push eax
	NEXT

link_AND:
	dd link_ZGT
	db 3, 'AND'
	dd code_AND
code_AND:
	pop eax
	and dword[esp], eax
	NEXT

link_OR:
	dd link_AND
	db 2, 'OR'
	dd code_OR
code_OR:
	pop eax
	or dword[esp], eax
	NEXT

link_XOR:
	dd link_OR
	db 3, 'XOR'
	dd code_XOR
code_XOR:
	pop eax
	xor dword[esp], eax
	NEXT

link_INVERT:
	dd link_XOR
	db 6, 'INVERT'
	dd code_INVERT
code_INVERT:
	not dword[esp]
	NEXT

link_STORE:
	dd link_INVERT
	db 1, '!'
	dd code_STORE
code_STORE:
	pop ebx
	pop eax
	mov [ebx], eax
	NEXT

link_FETCH:
	dd link_STORE
	db 1, '@'
	dd code_FETCH
code_FETCH:
	pop eax
	mov eax, [eax]
	push eax
	NEXT

link_ADDSTORE:
	dd link_FETCH
	db 2, '+!'
	dd code_ADDSTORE
code_ADDSTORE:
	pop ebx
	pop eax
	add [ebx], eax
	NEXT

link_SUBSTORE:
	dd link_ADDSTORE
	db 2, '-!'
	dd code_SUBSTORE
code_SUBSTORE:
	pop ebx
	pop eax
	sub [ebx], eax
	NEXT

link_COMMA:
	dd link_SUBSTORE
	db 1, ','
	dd code_COMMA
code_COMMA:
	lea edx, [ebp+dHERE]
	mov eax, [edx]
	pop dword[eax]
	add dword[edx], 4
	NEXT

link_CSTORE:
	dd link_COMMA
	db 2, 'C!'
	dd code_CSTORE
code_CSTORE:
	pop ebx
	pop eax
	mov [ebx], al
	NEXT

link_CFETCH:
	dd link_CSTORE
	db 2, 'C@'
	dd code_CFETCH
code_CFETCH:
	pop eax
	movzx eax, byte[eax]
	push eax
	NEXT

link_CCOMMA:
	dd link_CFETCH
	db 2, 'C,'
	dd code_CCOMMA
code_CCOMMA:
	lea edx, [ebp+dHERE]
	mov eax, [edx]
	pop ebx
	mov [eax], bl
	inc dword[edx]
	NEXT

link_DOCOL:
	dd link_CCOMMA
	db 5, 'DOCOL'
	dd code_DOCOL
code_DOCOL:
	push dword DOCOL
	NEXT

link_TOR:
	dd link_DOCOL
	db 2, '>R'
	dd code_TOR
code_TOR:
	pop eax
	sub edi, 4
	mov [edi], eax
	NEXT

link_FROMR:
	dd link_TOR
	db 2, 'R>'
	dd code_FROMR
code_FROMR:
	push dword[edi]
	add edi, 4
	NEXT

link_RPEEK:
	dd link_FROMR
	db 2, 'R@'
	dd code_RPEEK
code_RPEEK:
	push dword[edi]
	NEXT

link_RPSTORE:
	dd link_RPEEK
	db 3, 'RP!'
	dd code_RPSTORE
code_RPSTORE:
	pop edi
	NEXT

link_RPFETCH:
	dd link_RPSTORE
	db 3, 'RP@'
	dd code_RPFETCH
code_RPFETCH:
	push edi
	NEXT

link_SPSTORE:
	dd link_RPFETCH
	db 3, 'SP!'
	dd code_SPSTORE
code_SPSTORE:
	pop esp
	NEXT

link_SPFETCH:
	dd link_SPSTORE
	db 3, 'SP@'
	dd code_SPFETCH
code_SPFETCH:
	mov eax, esp
	push eax
	NEXT

link_KEY:
	dd link_SPFETCH
	db 3, 'KEY'
	dd code_KEY
code_KEY:
	call _KEY
	push eax
	NEXT

link_EMIT:
	dd link_KEY
	db 4, 'EMIT'
	dd code_EMIT
code_EMIT:
	pop eax
	call CallRM
	dw PrintChar
	NEXT

link_CREATE:
	dd link_EMIT
	db 6, 'CREATE'
	dd code_CREATE
code_CREATE:
	pop ecx
	pop ebx

; returns a character from the currently loaded file
; Output:
;  AL = the character
; Clobbers EBX
_KEY:
	mov ebx, [ebp+dOFFSET]
	cmp bx, 0x200
	jb .gotsector
	pushad
	call CallRM
	dw ReadNextCluster
	jc .eof
	popad
	xor ebx, ebx
.gotsector:
	xor eax, eax
	mov al, [FileBuffer+ebx]
	inc ebx
	mov [ebp+dOFFSET], ebx
	ret
.eof:
	mov si, EOFErrorMsg
	call CallRM
	dw Error
EOFErrorMsg:
	db 'EOF in FORTH source', 0

; WORD is a FORTH word which reads the next full word of input.
; Output:
;  ECX = string length
;  EDX = string buffer, always equal to WORDBuffer
; Clobbers EAX and EBX
..@SkipComment:
	call _KEY
	cmp al, 10
	jne ..@SkipComment
_WORD:
	call _KEY
	cmp al, '\'
	je ..@SkipComment
	cmp al, ' '
	jbe _WORD
	xor ecx, ecx
	mov edx, WORDBuffer
.main_loop:
	mov [edx+ecx], al
	inc ecx
	call _KEY
	cmp al, ' '
	ja .main_loop
	ret

; Parses a number in the base specified by BASE
; Input:
;  ECX = string length
;  EDX = string buffer
; Output:
;  EAX = the number represented in the string buffer
;  ECX = the number of unparsed characters (may indicate a failure)
_NUMBER:
	xor eax, eax
	xor ebx, ebx
.loop:
	mov bl, [edx]
	inc edx
	sub bl, '0'
	jb .end
	cmp bl, 9
	jbe .gotdigit
	sub bl, 'A' - '0'
	jb .end
	add bl, 10
.gotdigit:
	cmp bl, [ebp+dBASE]
	jae .end
	push edx
	mul dword[ebp+dBASE]
	add eax, ebx
	pop edx
	loop .loop
.end:
..@return:
	ret

; Input:
;  ECX = name length
;  EBX = name pointer
; Output:
;  EDX = word pointer, or 0 if not found
_FIND:
	push esi
	push edi

	mov edx, [ebp+dLATEST]
.loop:
	or edx, edx
	jz ..@return

	mov al, [edx+4]
	and al, F_HIDDEN|F_LENMASK
	cmp al, cl
	jnz .next

	lea esi, [edx+5]
	mov edi, ebx
	push ecx
	repe cmpsb
	pop ecx
	je ..@return
.next:
	mov edx, [edx]
	jmp .loop

	times 2048 - ($ - $$) db 0x69
