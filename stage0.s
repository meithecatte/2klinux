; This implements FAT32, with the assumption that the sector and cluster sizes are both 512 bytes.
; Long file names are not supported, but their presence for files we don't care about is not
; harmful. We use a part of the code section as variables after executing it:
;  7C00 -  7C0F -> The EDD disk packet
;  7C10 -  7C13 -> The LBA of the first FAT sector
;  7C14 -  7C17 -> The currently loaded cluster
; The general memory map looks like this:
;  0000 -  03FF -> Real mode interrupt vector table
;  0400 -  04FF -> The BIOS data area
;  0500 -  77FF -> Currently unassigned. The stack starts at 7A00, but I don't think it will need
;                  more than 256 bytes.
;  7800 -  79FF -> A buffer for one sector of a file or directory
;  7A00 -  7BFF -> A buffer for one sector of FAT
;  7C00 -  7DFF -> The MBR - the first part of this file
;  7E00 -  7FFF -> The VBR - the second part of this file
;  8000 - 7FFFF -> Unassigned.
; 80000 - 9FFFF -> Mostly unassigned, but the end is used by the Extended BIOS Data Area. Its size
;                  varies, and this 128 KiB is the maximum
; A0000 - BFFFF -> Video RAM
; C0000 - FFFFF -> ROMs and memory mapped hardware

bits 16
org 0x7c00

%define DiskPacket                   bp
%define DiskPacketSizeReserved       DiskPacket
%define DiskPacketSectorCount        DiskPacket+2
%define DiskPacketDestinationOffset  DiskPacket+4
%define DiskPacketDestinationSegment DiskPacket+6
%define DiskPacketLBA                DiskPacket+8

%define FATStart                     bp+16
%define CurrentCluster               bp+20

%define FATNameLength                11
%define DirAttributes                11
%define DirHighCluster               20
%define DirLowCluster                26
%define DirEntrySize                 32

%define FileBuffer                   0x7800
%define FATBuffer                    0x7a00

%define INT10_GetVideoMode           0x0f
%define INT10_SetVideoMode           0x00

%define Error_Disk                   0xe0
%define Error_FileNotFound           0xe1
%define Error_A20                    0xe2

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
	mov byte[..@DiskReadPatch+1], dl
	sti

	mov ah, INT10_GetVideoMode      ; Shortest way to clear the screen while preserving
	int 0x10                        ; the video mode
	mov ah, INT10_SetVideoMode
	int 0x10

	mov eax, dword[P1LBA]
	mov di, VBR
	inc cx
	call DiskReadSegment0

	movzx eax, word[BPBReservedSectors]
	add eax, dword[P1LBA]
	mov dword[FATStart], eax

	mov eax, dword[BPBRootDirectoryCluster]
	call ReadCluster
	mov di, .filename
	call FindFile
	jmp Error
.filename:
	db 'TESTING TXT'

FindFile:
	mov cx, 16
	mov si, FileBuffer
.loop:
	mov al, byte[si]
	or al, al
	jz short .notfound
	; normally, you would check if the first byte is 0xE5 (if so, you should skip the entry),
	; but it won't match the filename anyway
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
	mov ax, word[si+DirHighCluster]
	shl eax, 16
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
.notfound:
	mov al, Error_FileNotFound
	jmp short Error
ReadNextCluster:
	mov eax, dword[CurrentCluster]
	shr eax, 7
	add eax, dword[FATStart]
	mov di, FATBuffer
	call DiskReadSegment0
	mov bl, byte[CurrentCluster]
	shl bl, 1
	shl bx, 1
	mov eax, dword[bx+di]
	cmp eax, 0x0ffffff8
	cmc
	jc short Return
	; fallthrough
ReadCluster:
	mov dword[CurrentCluster], eax
	mov ebx, eax
	mov eax, dword[BPBLongSectorsPerFAT]
	movzx ecx, byte[BPBFATCount]
	mul ecx
	add eax, dword[FATStart]
	sub eax, 2
	add eax, ebx

	mov di, FileBuffer
	; fallthrough
DiskReadSegment0:
	xor bx, bx
	; fallthrough
DiskRead:
	mov word[DiskPacketSizeReserved], 0x0010
	mov word[DiskPacketSectorCount], cx
	mov word[DiskPacketDestinationOffset], di
	mov word[DiskPacketDestinationSegment], bx
	mov dword[DiskPacketLBA], eax
	xor eax, eax
	mov dword[DiskPacketLBA+4], eax
..@DiskReadPatch:
	db 0xB2, 0xFF ; mov dl, (patched at runtime)
	mov ah, 0x42
	mov si, DiskPacket
	int 0x13
	jnc short Return

	mov al, ah
	call PrintHexByte
	mov al, Error_Disk
Error:
	call PrintHexByte
.halt:
	hlt
	jmp short .halt

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
	add al, 'A' - '0' - 10
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

	times 15 db 0

BPBLongSectorCount:
	dd 0
BPBLongSectorsPerFAT:
	dd 0

	dd 0

BPBRootDirectoryCluster:
	dd 0

	times 42 db 0 ; reserved

Check_A20:
	; assumes DS = 0
	mov cx, 0xFFFF
	mov fs, cx
	mov si, 0x7dfe

.loop:
	mov al, byte[si]
	inc byte[fs:si+0x10]
	wbinvd
	cmp al, byte[si]
	jnz A20_OK
	loop .loop
	ret

KBC_WaitWrite:
	in al, 0x64
	test al, 2
	jnz KBC_WaitWrite
	ret

KBC_SendCommand:
	call KBC_WaitWrite
	pop di
	lodsb
	out 0x64, al
	jmp di

DoA20:
	call Check_A20
	mov ax, 0x2401
	int 0x15
	call Check_A20

	cli
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

	sti
	call Check_A20
	in al, 0x92
	or al, 2
	out 0x92, al
	call Check_A20
	mov al, Error_A20
	jmp Error

A20_OK:

	times 1022 - ($ - $$) db 0
	dw 0xaa55
