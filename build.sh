#!/bin/sh
printf "$(sed -e 's/^[^|]*|//g;s/|.*$//g;s/[^A-Fa-f0-9]//g' stage0.hxs | tr -d '\n' | sed -e 's/\(..\)/\\x\1/g')" >stage0.bin
