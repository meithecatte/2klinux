#!/bin/sh
# Exit on first error
set -e

# Use tput if available
if command -v tput >/dev/null; then
	TPUT=tput
else
	TPUT=:
fi

# Colored output helpers
error(){  $TPUT bold; $TPUT setaf 1; echo "$*"; $TPUT sgr0; exit 1; }
info(){   $TPUT setaf 4; echo    "$*"; $TPUT sgr0; }
infon(){  $TPUT setaf 4; echo -n "$*"; $TPUT sgr0; }
accent(){ $TPUT setaf 5; echo    "$*"; $TPUT sgr0; }

# Some utilities are put in /sbin even if they have a use for a normal user. Make sure sh can find them
PATH=$PATH:/sbin

# Choose an assembler
if command -v yasm >/dev/null; then
	ASM=yasm
elif command -v nasm >/dev/null; then
	ASM=nasm
else
	error "Can't find a suitable assembler. Install yasm or nasm and try again."
fi

infon "Assembling stage0... "
$ASM stage0.s -o stage0.bin -l stage0.lst
MBRFREE="$((0x1BE - 0x$(grep 'times 446'  stage0.lst | awk '{ print $2 }') ))"
VBRFREE="$((0x3FE - 0x$(grep 'times 1022' stage0.lst | awk '{ print $2 }') ))"
accent "$MBRFREE + $VBRFREE = $(($MBRFREE + $VBRFREE)) bytes free"

info "Creating the partition image..."
rm -f 1klinux.img fs.img
truncate -s 64M fs.img

info "Creating a FAT32 filesystem..."
mkfs.fat -F 32 -S 512 fs.img >/dev/null
mcopy -i fs.img TESTING.TXT ::

info "Creating the disk image..."
truncate -s $(($(stat -c %s fs.img) + 512)) 1klinux.img

info "Filling the partition table..."
sfdisk --quiet --no-reread --no-tell-kernel 1klinux.img <<EOF
label: dos
unit: sectors
1
EOF

info "Copying the filesystem..."
dd if=fs.img of=1klinux.img conv=notrunc status=none bs=512 seek=1

info "Installing stage0.bin..."
dd if=stage0.bin of=1klinux.img conv=notrunc status=none bs=1 count=446
dd if=stage0.bin of=1klinux.img conv=notrunc status=none bs=1 count=5 skip=510 seek=510
dd if=stage0.bin of=1klinux.img conv=notrunc status=none bs=1 count=420 skip=602 seek=604
