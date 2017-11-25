; This implements FAT32, with the assumption that the sector and cluster sizes are both 512 bytes.
; Long file names are not supported, but their presence for files we don't care about is not
; harmful. We use a part of the code section as variables after executing it:
;  7C00 -  7C0F -> The EDD disk packet
;  7C10 -  7C13 -> The LBA of the first FAT sector
;  7C14 -  7C17 -> The currently loaded cluster
;  7C18 -  7C1B -> OFFSET
;  7C1C -  7C1F -> LATEST
;  7C20 -  7C23 -> HERE
; The general memory map looks like this:
;  0000 -  03FF -> Real mode interrupt vector table
;  0400 -  04FF -> The BIOS data area
;  0500 -  14FF -> FORTH return stack
;  1500 -  77FF -> the stack, used as the FORTH parameter stack
;  7800 -  79FF -> A buffer for one sector of a file or directory
;  7A00 -  7BFF -> A buffer for one sector of FAT
;  7C00 -  7DFF -> The MBR - the first part of this file
;  7E00 -  7FFF -> The VBR - the second part of this file
;  8000 -  801F -> WORD's buffer
;  8000 - 7FFFF -> Unassigned.
; 80000 - 9FFFF -> Mostly unassigned, but the end is used by the Extended BIOS Data Area. Its size
;                  varies, and this 128 KiB is the maximum
; A0000 - BFFFF -> Video RAM
; C0000 - FFFFF -> ROMs and memory mapped hardware

bits 16
org 0x7c00

%define DiskPacket                   bp
%define DiskPacketDestinationOffset  DiskPacket+4
%define DiskPacketDestinationSegment DiskPacket+6
%define DiskPacketLBA                DiskPacket+8

%define FATStart                     ebp+16
%define CurrentCluster               ebp+20
%define OFFSET                       ebp+24
%define LATEST                       ebp+28
%define HERE                         ebp+32

%define FATNameLength                11
%define DirAttributes                11
%define DirHighCluster               20
%define DirLowCluster                26
%define DirEntrySize                 32

%define FileBuffer                   0x7800
%define FATBuffer                    0x7a00

%define INT10_GetVideoMode           0x0f
%define INT10_SetVideoMode           0x00

%define Error_Disk                   'Q'
%define Error_FileNotFound           'R'
%define Error_A20                    'S'

%define Selector_Code16              16
%define Selector_Code32              8
%define Selector_Data                24

%macro NEXT 0
	lodsd
	jmp [eax]
%endmacro

MBR:
	jmp 0:start
start:
	cli                             ; Disable the interrupts when setting up the stack
	xor cx, cx
	mov bp, MBR                     ; Using bp-relative addressing produces shorter code
	mov sp, FileBuffer
	mov ss, cx
	mov ds, cx
	mov es, cx
	dec cx
	mov fs, cx
	mov cx, 0xb800
	mov gs, cx
	mov byte[..@DiskReadPatch+1], dl
	sti

	mov ah, INT10_GetVideoMode      ; Shortest way to clear the screen while preserving
	int 0x10                        ; the video mode
	mov ah, INT10_SetVideoMode
	int 0x10

	mov eax, dword[P1LBA]
	mov di, VBR
	push word A20
	; fallthrough
DiskRead:
	mov dword[DiskPacketLBA], eax
	xor eax, eax
	mov dword[DiskPacketLBA+4], eax
	mov dword[DiskPacket], 0x10010
	mov word[DiskPacketDestinationOffset], di
	mov word[DiskPacketDestinationSegment], ax
..@DiskReadPatch:
	db 0xB2, 0xFF ; mov dl, (patched at runtime)
	mov ah, 0x42
	mov si, DiskPacket
	int 0x13
	jnc short Return

	mov al, ah
	call PrintByte
	mov al, Error_Disk
Error:
	call PrintChar
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
	; fallthrough
Return:
	ret

BITS 32
PM_Entry:
	movzx eax, word[BPBReservedSectors]
	add eax, dword[P1LBA]
	mov dword[FATStart], eax

	mov eax, dword[BPBRootDirectoryCluster]
	call ReadCluster
	mov edi, .filename
	call FindFile

	mov edi, 0x1500 ; initialize the return stack
	cli
	hlt
.filename:
	db 'STAGE1  F  '

;DROP:
;	pop eax
;	jmp short doNEXT
;DUP:
;	pop eax
;	push eax
;	push eax
;	jmp short doNEXT

LIT:
	lodsd
	push eax
	jmp short doNEXT

SWAP:
	pop eax
	pop ebx
	push eax
	push ebx
	jmp short doNEXT

ROT:
	pop eax
	pop ebx
	pop ecx
	push ebx
	push eax
	push ecx
	jmp short doNEXT

ZBRANCH:
	pop eax
	or eax, eax
	jz BRANCH
	lodsd
	jmp short doNEXT
BRANCH:
	add esi, [esi]
doNEXT:
	NEXT

_KEY:
	mov bx, [OFFSET]
	cmp bx, 0x200
	jb .gotsector
	pushad
	call ReadNextCluster
	popad
	xor bx, bx
.gotsector:
	mov al, [FileBuffer+bx]
	inc bx
	mov [OFFSET], bx
	ret

FindFile:
	xor ecx, ecx
	mov cl, 16
	mov esi, FileBuffer
.loop:
	mov al, byte[esi]
	or al, al
	jz short .notfound
	; normally, you would check if the first byte is 0xE5 (if so, you should skip the entry),
	; but it won't match the filename anyway
	test byte[esi+DirAttributes], 0x0e
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
	cmp al, byte[edi]
	jne short .nomatch
	inc edi
	loop short .cmploop
	popad
	mov eax, [esi+DirHighCluster]
	shl eax, 16
	mov ax, [esi+DirLowCluster]
	jmp short ReadCluster
.nomatch:
	popad
.next:
	add esi, DirEntrySize
	loop short .loop
	push edi
	call ReadNextCluster
	pop edi
	jnc short FindFile
.notfound:
	xor eax, eax
	mov word[gs:eax], 0x073f
	hlt
ReadNextCluster:
	mov eax, dword[CurrentCluster]
	shr eax, 7
	add eax, dword[FATStart]
	mov edi, FATBuffer
	call CallRM
	dw DiskRead
	movzx ebx, byte[CurrentCluster]
	shl bl, 1
	mov eax, dword[edi+2*ebx]
	cmp eax, 0x0ffffff8
	cmc
	jnc short ReadCluster
	ret

ReadCluster:
	mov dword[CurrentCluster], eax
	mov ebx, eax
	mov eax, dword[BPBSectorsPerFAT]
	movzx ecx, byte[BPBFATCount]
	mul ecx
	add eax, dword[FATStart]
	dec eax
	dec eax
	add eax, ebx

	mov di, FileBuffer
	call CallRM
	dw DiskRead
	ret
	times 446 - ($ - $$) db 0

PartitionTable:
	times 8 db 0

P1LBA:      dd 0
P1Length:   dd 0

	times 48 db 0

	dw 0xaa55

VBR:
	jmp VBR
	nop

	times 11 db 0

BPBReservedSectors:
	dw 0
BPBFATCount:
	db 0

	times 19 db 0

BPBSectorsPerFAT:
	dd 0

	dd 0

BPBRootDirectoryCluster:
	dd 0

	times 42 db 0
BITS 16
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
	mov al, Error_A20
	jmp Error

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

	times 1022 - ($ - $$) db 0
	dw 0xaa55
