# 2Klinux

A Linux distribution that bootstraps from four sectors worth of binaries
(and a pile of source code).

## Assembling

You'll need `yasm` or `nasm` installed. Run the `build.sh` script to
generate a binary called `stage0`.

## Creating a bootable drive

Here I'll show you how you can create a bootable disk image. You can then
write it to a real disk later. If you know what you're doing, you can operate
on the disk directly.

1. Create the image

    $ truncate -s 32M 1klinux.img

or

    $ dd if=/dev/zero of=1klinux.img bs=1048576 count=32
    32+0 records in
    32+0 records out
    33554432 bytes (34 MB, 32 MiB) copied, 0.0382914 s, 876 MB/s

2. Create a new primary FAT32 partition

    $ /sbin/fdisk fs.img

    Welcome to fdisk (util-linux 2.30.2).
    Changes will remain in memory only, until you decide to write them.
    Be careful before using the write command.


    Command (m for help): o
    Created a new DOS disklabel with disk identifier 0xa290ce88.

    Command (m for help): n
    Partition type
       p   primary (0 primary, 0 extended, 4 free)
       e   extended (container for logical partitions)
    Select (default p): p
    Partition number (1-4, default 1):
    First sector (2048-65535, default 2048):
    Last sector, +sectors or +size{K,M,G,T,P} (2048-65535, default 65535):

    Created a new partition 1 of type 'Linux' and of size 31 MiB.

    Command (m for help): t
    Selected partition 1
    Hex code (type L to list all codes): c
    Changed type of partition 'Linux' to 'W95 FAT32 (LBA)'.

    Command (m for help): w
    The partition table has been altered.
    Syncing disks.

3. Bind the partition to a loopback device (**Note:** replace 2048 with the
  first sector parameter from the previous step)

    $ sudo losetup -o $((512 * 2048)) /dev/loop0 1klinux.img

4. Format the partition

    $ sudo mkfs.fat /dev/loop0
    mkfs.fat 4.1 (2017-01-24)

5. Mount the partition

    $ mkdir mnt
    $ sudo mount /dev/loop0 mnt

6. Copy the files from 
... TODO ...
