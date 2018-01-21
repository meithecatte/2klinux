#!/bin/bash
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
mkdir -p gen
rm -rf gen/*
$ASM stage0.s -o gen/stage0.bin -l gen/stage0.lst
MBRFREE="$(hexdump -e '"%d\n"' -s446 -n2 gen/stage0.bin)"
RESTFREE="$(hexdump -e '"%d\n"' -s448 -n2 gen/stage0.bin)"
accent "$MBRFREE + $RESTFREE = $(($MBRFREE + $RESTFREE)) bytes free"

# If you find a way to create only one 64 megabyte file without using loop devices, hit me up
info "Creating the partition image..."
truncate -s 64M gen/fs.img

info "Creating a FAT32 filesystem..."
echo "You probably used fs.img instead of 2klinux.img. Read the goddamn manual." | mkfs.fat -m - -F 32 -S 512 gen/fs.img >/dev/null
mcopy -i gen/fs.img image-files/* ::
mcopy -i gen/fs.img gen/stage0.bin ::

info "Creating the disk image..."
truncate -s $(($(stat -c %s gen/fs.img) + 512)) gen/2klinux.img

info "Filling the partition table..."
sfdisk --quiet --no-reread --no-tell-kernel gen/2klinux.img <<EOF
label: dos
unit: sectors
1
EOF

info "Copying the filesystem..."
dd if=gen/fs.img     of=gen/2klinux.img conv=notrunc status=none bs=512 seek=1

info "Installing stage0.bin..."
dd if=gen/stage0.bin of=gen/2klinux.img conv=notrunc status=none bs=1 count=446
