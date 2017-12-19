# 2Klinux

A Linux distribution that bootstraps from a 2 kilobyte binary (and a pile of source code), involving a filesystem driver
that fits in 256 bytes and a C compiler written in the awesome language known as Forth.

## Testing

You'll need:
- `yasm` or `nasm`
- `mtools`
- `bash`
- `sfdisk`
- GNU coreutils
- an emulator like QEMU or bochs if you are not brave enough to run it on hardware.

Run the `build.sh` script to generate a disk image called `gen/2klinux.img`. You can use the `run.sh` script to test it under QEMU.

## The plan

- BIOS boots the installation media
- the remaining 1.5K is loaded from a FAT partition, where all the other files are also stored
- the bootstrap binary implements Forth
- a small C compiler written in Forth compiles libtcc and a real compilation driver
- the compilation driver uses libtcc to compile the Linux kernel and an initrd or initramfs consisting of busybox, uclibc and tcc
- the kernel boot parameter structure is created and the Linux kernel boots into busybox's shell
- GNU make is compiled using the shellscript provided in the distribution
- bash is compiled since some packages use some shell features that are not available in busybox
- GCC 4.7, the last version of GCC written in C is compiled using tcc, together with an appropriate version of binutils and glibc, since these packages seem to be collaborating pretty tightly
- the newest versions of GCC, binutils and glibc are compiled using GCC 4.7

The rest is pretty hard to plan out so far into the future, but the hardest part seems to be above. The probability of any changes is directly proportional to the position on this list.
