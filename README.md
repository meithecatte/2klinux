# 2Klinux

A Linux distribution that bootstraps from four sectors worth of binaries (and a pile of source code), involving a filesystem driver
that fits in 256 bytes and a C compiler written in the awesome language known as Forth.

## Testing

You'll need `yasm` or `nasm` installed. Run the `build.sh` script to generate a disk image called `gen/2klinux.img`. You can use the
`run.sh` script to test it under QEMU.
