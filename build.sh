#!/bin/sh
printf "$(sed -e 's/^[^|]*|//g;s/|.*$//g;s/[^A-Fa-f0-9]//g' stage0.hxs | tr -d '\n' | sed -e 's/\(..\)/\\x\1/g')" >stage0.bin
SIZE="$(wc -c <stage0.bin)"
if [ "$SIZE" -gt 512 ]; then
	echo "stage0.bin too large by $(($SIZE - 512)) bytes"
elif [ "$SIZE" -lt 512 ]; then
	echo "stage0.bin too small by $((512 - $SIZE)) bytes"
fi
