; This implements FAT32, with the assumption that the sector size is 512 and the cluster size is
; not larger than 32K. Long file names are not supported, but their presence for files we don't
; care about is not harmful. We use a part of the code section as variables after executing it:
;  7C00 -  7C0F -> The EDD disk packet
;  7C10 -  7C13 -> The first FAT sector
;  7C14 -  7C17 -> The LBA of cluster 0
;  7C18 -  7C1B -> The currently loaded cluster
; The general memory map looks like this:
;  0000 -  03FF -> Real mode interrupt vector table
;  0400 -  04FF -> The BIOS data area
;  0500 -  79FF -> Currently unassigned. The stack starts at 7A00, but I don't think it will need
;                  more than 256 bytes.
;  7A00 -  7BFF -> A buffer for one sector of FAT
;  7C00 -  7DFF -> The MBR - the first part of this file
;  7E00 -  7FFF -> The VBR - the second part of this file
;  8000 -  83FF -> A buffer for one cluster of a file or directory
; 10000 - 7FFFF -> Unassigned.
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
%define ClustersStart                bp+20
%define CurrentCluster               bp+24
%define FATNameLength                11
%define DirAttributes                11
%define DirHighCluster               20
%define DirLowCluster                26
%define DirEntrySize                 32

MBR:
	jmp 0:start
start:
	cli                                      ; Disable the interrupts when setting up the stack
	xor cx, cx
	mov bp, 0x7c00
	mov sp, 0x7a00
	mov ss, cx
	mov ds, cx
	mov es, cx
	mov byte[DiskReadPatch+1], dl
	sti

	mov ah, 0x0f
	int 0x10
	mov ah, 0x00
	int 0x10

	mov eax, dword[P1LBA]
	mov di, VBR
	inc cx
	call DiskReadSegment0

	movzx eax, word[BPBReservedSectors]
	add eax, dword[P1LBA]
	mov dword[FATStart], eax

	mov eax, dword[BPBLongSectorsPerFAT]
	movzx ecx, byte[BPBFATCount]
	mul ecx
	add eax, dword[FATStart]
	mov cl, byte[BPBSectorsPerCluster]
	cmp cl, 64
	jbe .clustersize_ok
	call Error
	db 'EMKFS', 0
.clustersize_ok:
	add cx, cx
	sub eax, ecx
	mov dword[ClustersStart], eax

	mov eax, dword[BPBRootDirectoryCluster]
	call ReadCluster
	mov di, .filename
	call FindFile
	mov si, 0x8000
	call PrintText
	jmp Halt
.filename:
	db 'TESTING TXT'

FindFile:
	mov cx, 16
	mov si, 0x8000
.loop:
	mov al, byte[si]
	or al, al
	jz FileNotFound
	cmp al, 0xe5
	je .next
	test byte[si+DirAttributes], 0x0e
	jnz .next
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
	jne .nomatch
	inc di
	loop .cmploop
	popa
	mov ax, word[si+DirHighCluster]
	shl eax, 16
	mov ax, word[si+DirLowCluster]
	jmp short ReadCluster
.nomatch:
	popa
.next:
	add si, DirEntrySize
	loop .loop
	push di
	call ReadNextCluster
	pop di
	jnc short FindFile
	; fallthrough
FileNotFound:
	mov si, di
	mov cx, FATNameLength
.loop:
	lodsb
	call PrintChar
	loop .loop
	call Error
	db 'ENOENT', 0
ReadNextCluster:
	mov eax, dword[CurrentCluster]
	shr eax, 7
	add eax, dword[FATStart]
	mov di, 0x7a00
	call DiskReadSegment0
	movzx bx, byte[CurrentCluster]
	shl bx, 2
	mov eax, dword[bx+di]
	cmp eax, 0x0ffffff8
	cmc
	jc short Return
	; fallthrough
ReadCluster:
	mov dword[CurrentCluster], eax
	movzx ecx, byte[BPBSectorsPerCluster]
	mul ecx
	add eax, dword[ClustersStart]
	mov di, 0x8000
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
	; fallthrough
DiskReadPatch:
	db 0xB2, 0xFF ; mov dl, (patched at runtime)
	mov ah, 0x42
	mov si, DiskPacket
	int 0x13
	jnc short Return
	; fallthrough
DiskError:
	mov al, ah
	call PrintHexByte
	call Error
.msg:
	db 'EDISK', 0

Error:
	pop si
	call PrintText
	; fallthrough
Halt:
	cli
	hlt
	jmp short Halt

PrintText:
	lodsb
	or al, al
	jz short Return
	call PrintChar
	jmp short PrintText

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
P1Active:   db 0
P1CHSStart: times 3 db 0
P1Type:     db 0
P1CHSEnd:   times 3 db 0
P1LBA:      dd 0
P1Length:   dd 0

P2Active:   db 0
P2CHSStart: times 3 db 0
P2Type:     db 0
P2CHSEnd:   times 3 db 0
P2LBA:      dd 0
P2Length:   dd 0

P3Active:   db 0
P3CHSStart: times 3 db 0
P3Type:     db 0
P3CHSEnd:   times 3 db 0
P3LBA:      dd 0
P3Length:   dd 0

P4Active:   db 0
P4CHSStart: times 3 db 0
P4Type:     db 0
P4CHSEnd:   times 3 db 0
P4LBA:      dd 0
P4Length:   dd 0

	dw 0xaa55

VBR:
	jmp VBR
	nop
BPBOEMIdentifier:
	times 8 db 0
BPBBytesPerSector:
	dw 0
BPBSectorsPerCluster:
	db 0
BPBReservedSectors:
	dw 0
BPBFATCount:
	db 0
BPBNumDirectoryEntries:
	dw 0
BPBShortSectorCount:
	dw 0
BPBMediaDescriptor:
	db 0
BPBShortSectorsPerFAT:
	dw 0
BPBSectorsPerTrack:
	dw 0
BPBHeadCount:
	dw 0
BPBHiddenSectors:
	dd 0
BPBLongSectorCount:
	dd 0
BPBLongSectorsPerFAT:
	dd 0
BPBFlags:
	dw 0
BPBFATVersion:
	dw 0
BPBRootDirectoryCluster:
	dd 0
BPBFSInfoSector:
	dw 0
BPBBackupSector:
	dw 0

	times 12 db 0 ; reserved

BPBDriveNumber:
	db 0
BPBNTFlags:
	db 0
BPBSignature:
	db 0
BPBSerial:
	dd 0
BPBLabel:
	times 11 db 0
BPBSystemIdentifier:
	times 8 db 0

	times 1022 - ($ - $$) db 0
	dw 0xaa55
