; This Forth implementation is based on jonesforth - https://github.com/nornagon/jonesforth
; Any similarities are probably not accidental.

; The first bootstrap stage of this project is implemented as a bootloader. This bootsector
; implements FAT32, with the assumption that the sector and cluster sizes are both 512 bytes. Long
; file names are not supported, but their presence for files we don't care about is not harmful.
; All disk access is done using EDD, which means problems for very old PCs (like pre-Pentium old)
; or booting from a floppy. Both of these problems don't concern me, like, at all. The FAT partition
; with all the files should be the first physical partition of the drive.

; We use a part of the code section as variables after executing it:
;  7C00 -  7C0F -> The EDD disk packet
;  7C10 -  7C13 -> The LBA of the first FAT sector
;  7C14 -  7C17 -> The currently loaded cluster
;  7C18 -  7C1B -> OFFSET
;  7C1C -  7C1F -> LATEST
;  7C20 -  7C23 -> HERE
;  7C24 -  7C27 -> BASE
;  7C28 -  7C2B -> STATE
;  7DDE -  7DFD -> WORD's buffer

; The general memory map looks like this:
;  0000 -  03FF -> Real mode interrupt vector table
;  0400 -  04FF -> The BIOS data area
;  0500 -  14FF -> FORTH return stack
;  1500 -  7BFF -> the stack, used as the FORTH parameter stack
;  7C00 -  7DFF -> The MBR - the first part of this file
;  7E00 -  83FF -> 3 sectors loaded from the FAT filesystem - the second part of this file
;  8400 -  85FF -> A buffer for one sector of a file or directory
;  8600 -  87FF -> A buffer for one sector of FAT
;  8800 -  89FF -> A buffer for the sector with BPB
;  8A00 - 7FFFF -> Unassigned.
; 80000 - 9FFFF -> Mostly unassigned, but the end is used by the Extended BIOS Data Area. Its size
;                  varies, and this 128 KiB is the maximum
; A0000 - BFFFF -> Video RAM
; C0000 - FFFFF -> ROMs and memory mapped hardware

; Finally, EBP is constantly loaded with the value 0x7C00 to generate shorter instructions for accessing
; some memory locations.

BITS 16
ORG 0x7c00

%define FileBuffer 0x8400
%define FATBuffer  0x8600
%define BPBBuffer  0x8800

; Constants that start with a lowercase d represent the offset from 0x7C00, and therefore EBP, of some
; memory address.

; Used by disk access routines
%define dDiskPacket            0
%define dDiskPacketDestOffset  4
%define dDiskPacketDestSegment 6
%define dDiskPacketLBA         8

; Filesystem support
%define dFATStart       16
%define dCurrentCluster 20

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

; FORTH variables
%define dOFFSET 24 ; The address of the next byte KEY will read, relative to the beginning of the 512 byte buffer.
%define dLATEST 28 ; The LFA of the last FORTH word defined.
%define dHERE   32 ; The address of the first free byte of memory. This is where any new FORTH definitions will be put.
%define dBASE   36 ; The number base of the digits parsed by NUMBER. Starts at 16.
%define dSTATE  40 ; 1 if compiling words, 0 if interpreting.

%define Error_Disk         'Q'
%define Error_FileNotFound 'R'
%define Error_A20          'S'
%define Error_FileTooShort 'T'

%define Selector_Code32 0x08
%define Selector_Code16 0x10
%define Selector_Data   0x18

%define F_IMMED   0x80
%define F_HIDDEN  0x20
%define F_LENMASK 0x1f

%macro NEXT 0
	lodsd
	jmp [eax]
%endmacro

%macro RPUSH 1
	sub edi, 4
	mov [edi], %1
%endmacro

%macro RPOP 1
	mov %1, [edi]
	add edi, 4
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

	mov eax, dword[BPBRootCluster]
	call ReadCluster
	mov edi, StageZeroFilename
	push word LoadPartTwo

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
	mov al, Error_FileNotFound
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
	mov al, Error_Disk
	; fallthrough
Error:
	call PrintChar
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
	add al, 'A'
	; fallthrough
PrintChar:
	pusha
	xor bx, bx
	mov ah, 0x0e
	int 0x10
	popa
..@Return:
	ret

FileTooShortError:
	cli
	hlt
	mov al, Error_FileTooShort
..@jmpError:
	jmp short Error

StageZeroFilename:
	db 'STAGENOTBIN'

LoadPartTwo:
	mov byte[..@ReadClusterPatch], 0x7E
	mov cx, 3
.loop:
	push cx
	call ReadNextCluster
	jc short FileTooShortError
	add byte[..@ReadClusterPatch], 2
	pop cx
	loop .loop

	; try enabling the A20 with 3 different methods

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
	mov al, Error_A20
	jmp short ..@jmpError

	times 446 - ($ - $$) db 0

PartitionTable:
	times 8 db 0

P1LBA:      dd 0
P1Length:   dd 0

	times 16 db 0

WORDBuffer:

	times 32 db 0

	dw 0xaa55

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
	; assumes DS = 0
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

BITS 32
PM_Entry:

	xor eax, eax
	mov [ebp+dLATEST], eax
	mov [ebp+dSTATE], eax
	mov [ebp+dBASE], eax
	mov byte[ebp+dBASE], 0x10
	mov ah, 0x15
	mov edi, eax
	mov ah, 0x80
	mov [ebp+dHERE], eax

	cli
	hlt

StageOneFilename:
	db 'STAGEONEFRT'
LIT:
	lodsd
pushEAXdoNEXT:
	push eax
doNEXT:
	NEXT

DOCOL:
	RPUSH esi
	add eax, 4 ; skip codeword
	mov esi, eax
	jmp short doNEXT

EXIT:
	RPOP esi
	jmp short doNEXT

; returns a character from the currently loaded file
; Output:
;  AL = the character
; Clobbers EBX
_KEY:
	mov ebx, [ebp+dOFFSET]
	cmp bx, 0x200
	jb .gotsector
	pushad
	call ReadNextCluster
	popad
	xor ebx, ebx
.gotsector:
	mov al, [FileBuffer+ebx]
	inc ebx
	mov [ebp+dOFFSET], ebx
	ret

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

%macro dict 2
%strlen dictlen %1
	db dictlen, %1, %2 & 0xff
%endmacro

CompressedDictionary:
	dict 'LIT', LIT
	dict 'EXIT', EXIT

	times 2048 - ($ - $$) db 0x69
