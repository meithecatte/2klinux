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

# Some utilities are put in /sbin even if they have a use for a normal user. Make sure sh can find them
PATH=$PATH:/sbin

info "Generating stage0.bin..."
printf "$(sed -e 's/^[^|]*|//g;s/|.*$//g;s/[^A-Fa-f0-9]//g;s/\./0/g' stage0.hxs | tr -d '\n' | sed -e 's/\(..\)/\\x\1/g')" >stage0.bin
SIZE="$(wc -c <stage0.bin)"
EXPECTED=512

if [ $SIZE -ne $EXPECTED ]; then
	if [ $SIZE -gt $EXPECTED ]; then
		CMP=large
		DIFF=$(($SIZE - $EXPECTED))
	elif [ $SIZE -lt $EXPECTED ]; then
		CMP=small
		DIFF=$(($EXPECTED - $SIZE))
	fi

	error "stage0.bin is too $CMP by $DIFF bytes"
fi

info "Creating the partition image..."
rm -f 1klinux.img fs.img
truncate -s 64M fs.img

info "Creating a FAT32 filesystem..."
mkfs.fat -F 32 -S 512 -s 1 fs.img >/dev/null
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
