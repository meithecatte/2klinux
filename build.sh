#!/bin/sh
set -e
if command -v tput >/dev/null; then
	TPUT=tput
else
	TPUT=:
fi

error(){ $TPUT bold; $TPUT setaf 1; echo "$*"; $TPUT sgr0; exit 1; }
info(){  $TPUT setaf 2; echo "$*"; $TPUT sgr0; }

info Generating stage0.bin...
printf "$(sed -e 's/^[^|]*|//g;s/|.*$//g;s/[^A-Fa-f0-9]//g' stage0.hxs | tr -d '\n' | sed -e 's/\(..\)/\\x\1/g')" >stage0.bin
SIZE="$(wc -c <stage0.bin)"
if [ $SIZE -ne 512 ]; then
	if [ $SIZE -gt 512 ]; then
		CMP=large
		DIFF=$(($SIZE - 512))
	elif [ $SIZE -lt 512 ]; then
		CMP=small
		DIFF=$((512 - $SIZE))
	fi

	error "stage0.bin is too $CMP by $DIFF bytes"
fi

info Creating 1klinux.img...
truncate -s 32M 1klinux.img

info Partitioning the image...
PATH=$PATH:/sbin
sed 's/\s*//g;s/#.*$//g' <<EOF | fdisk 1klinux.img >/dev/null
  o # create a new partition table
  n # create a new partition
  p # primary
  1 # partition number
    # default - beginning
    # default - end
  t # change type (of the only partition)
  c # W95 FAT32 (LBA)
  w # write and exit
EOF

info Writing the MBR...
dd if=stage0.bin of=1klinux.img bs=1 count=446 conv=notrunc status=none
