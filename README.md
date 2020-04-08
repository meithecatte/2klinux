# 2K Linux

A Linux distribution that bootstraps from a 2 kilobyte binary (and a pile of source code), involving a filesystem driver
that fits in 256 bytes and a C compiler written in the awesome language known as Forth.

## Disappointment warning

Current status: a Forth system that reads its source from FAT32, and chunks of
a C preprocessor. After a while, I realized that the current approach is not
ideal, because while booting from source code fully automatically makes for a
nice demo, it doesn't have any introspective capabilities. I am considering
redoing this project while optimizing for explainability and experimentation.
In particular, [snaptoken-style](https://viewsourcecode.org/snaptoken/) literate
programming would be nice. However, this is definitely not a short-term plan.
Make sure to *watch* this repository on GitHub if you're interested.

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

## The Forth system

The Forth implemented in 2K Linux is not fully ANS compliant. This section aims to list all places where the implementation diverges from this standard.

 - This Forth is not interactive. All input is read from a file on the installation media.
 - Input is not read line by line, but in 512-byte sectors. The input buffer might be refilled in the middle of reading a word.
 - `HERE` does not behave like a `VALUE`, but like a `VARIABLE`.
 - `WORD` does not take a separator on the stack. Instead, words may be separated by any whitespace character, i. e. anything with an ASCII value <= 32.
 - `ABORT` is an unconditional "print message and halt". No error code is used.
 - `."` during interpretation will print a message.
 - `[COMPILE]` is only available before `POSTPONE` is implemented.
 - The following `CORE` and `CORE EXT` words are not implemented:
   - `<#`
   - `#`
   - `#S`
   - `#>`
   - `2!`
   - `2@`
   - `>BODY`
   - `>NUMBER`
   - `ABORT"`
   - `ACCEPT`
   - `ALIGN`
   - `ALIGNED`
   - `BASE`
   - `CHAR+`
   - `CHARS`
   - `DECIMAL`
   - `DOES>`
   - `ENVIRONMENT?`
   - `EVALUATE`
   - `HOLD`
   - `MOVE`
   - `QUIT`
   - `S>D`
   - `SIGN`
   - `SOURCE`
   - `.(`
   - `ACTION-OF`
   - `BUFFER:`
   - `C"`
   - `COMPILE,` (replaced by `,`)
   - `DEFER!`
   - `DEFER@`
   - `ERASE`
   - `HEX`
   - `HOLDS`
   - `MARKER`
   - `PAD`
   - `PARSE`
   - `PARSE-NAME`
   - `REFILL`
   - `RESTORE-INPUT`
   - `S\"`
   - `SAVE-INPUT`
   - `SOURCE-ID`
   - `TO`
   - `UNUSED`
   - `VALUE`
 - There aren't really any blocks to deal with, but a similar wordset is used to deal with 512 byte sectors on the installation media.
 - There is only one block buffer
 - The only word from the block wordset that carries the standard semantics is `BLK`
 - `LOAD` just updates `BLK` and loads a sector from the disk into the buffer, current input won't be restored on EOF and the data will only be interpreted as Forth only if the control is returned to the interpreter. When using `LOAD`, care must be taken to make sure `>IN` and `LENGTH` reflect the reality.
 - `FILE` is a word that takes a pointer to a filename and its length, finds the file on disk and LOADs its first sector. The currently loaded sector is used as the current directory.
 - `ROOT` loads the root directory, to be followed by `FILE`.
