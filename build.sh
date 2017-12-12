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
mkdir -p gen
rm -rf gen/*
$ASM stage0.s -o gen/stage0.bin -l gen/stage0.lst
MBRFREE="$((0x01BE - 0x$(grep 'times 446'  gen/stage0.lst | awk '{ print $2 }') ))"
RESTFREE="$((0x800 - 0x$(grep 'times 1536' gen/stage0.lst | awk '{ print $2 }') ))"
accent "$MBRFREE + $RESTFREE = $(($MBRFREE + $RESTFREE)) bytes free"

info "Creating the partition image..."
truncate -s 64M gen/fs.img

info "Creating a FAT32 filesystem..."
echo "You booted the partition instead of the whole drive. I think you should fix it..." | mkfs.fat -m - -F 32 -S 512 gen/fs.img >/dev/null
#sed -e $(hexdump -s 1024 -e '/2 "%2X\n"' stage0.bin | sed = | sed -e 'N;s@\n@/@;s@^@s/!!PP!!@;s@$@/@' | paste -sd ';') stage1.ft | mcopy -i fs.img - ::\stage1.ft
mcopy -i gen/fs.img gen/stage0.bin ::\STGEZERO.BIN
mcopy -i gen/fs.img     stage1.frt ::\STAGEONE.FRT

info "Creating the disk image..."
truncate -s $(($(stat -c %s gen/fs.img) + 512)) gen/2klinux.img

info "Filling the partition table..."
sfdisk --quiet --no-reread --no-tell-kernel gen/2klinux.img <<EOF
label: dos
unit: sectors
1
EOF

info "Copying the filesystem..."
dd if=gen/fs.img     of=gen/1klinux.img conv=notrunc status=none bs=512 seek=1

info "Installing stage0.bin..."
dd if=gen/stage0.bin of=gen/2klinux.img conv=notrunc status=none bs=1 count=446
dd if=gen/stage0.bin of=gen/2klinux.img conv=notrunc status=none bs=1 count=2 skip=510 seek=510
