# The "BootProg" Boot Sector


## What is BootProg?

BootProg is a collection of 512-byte boot sectors (for the x86 PC) capable of
loading and executing a program from a
[FAT](https://en.wikipedia.org/wiki/File_Allocation_Table)12-formatted floppy
or a FAT16/32-formatted hard disk (bootable USB sticks and CDs can also be made
with BootProg).

BootProg understands programs in the MS-DOS
[.COM](https://en.wikipedia.org/wiki/COM_file) or
[.EXE](https://en.wikipedia.org/wiki/DOS_MZ_executable) format. This makes it
possible to use existing 16-bit compilers such as Borland/Turbo C/C++,
Sybase/Open Watcom C/C++ and Smaller C and a variety of assemblers such as
[NASM](https://nasm.us/), FASM, TASM and MASM among the others.

BootProg doesn't require that the program occupy a contiguous span of sectors
or FAT clusters or reside at a specific fixed location on the disk. BootProg
faithfully parses the root directory and the chain of FAT clusters in order to
locate the program contents. The only requirement is that the program be named
"STARTUP.BIN" (without quotes). This makes updating the program easy. You just
need to update the file and you can reboot and execute it immediately.

Btw, you can flexibly boot your HDD partitions containning BootProg and some
other OSes using the [MBiRa](https://github.com/alexfru/MBiRa) boot manager.


## What can BootProg be used for?

You can make a boot loader for your OS. The program that BootProg loads can be
your 2nd stage boot loader. Or, if your OS is relatively small, STARTUP.BIN
could contain the entire OS.

You can write low-level utilities to work with your PC's hardware and load them
with BootProg without having to jump through the hoops with your Windows, Linux
or even DOS.

You can make cool graphics demos or games that run on bare hardware.


## What can't BootProg be used for?

Many things. Most importantly, if you make a DOS program that uses any MS-DOS
service functions (e.g. [int](http://www.delorie.com/djgpp/doc/rbinter/) 21h)
or data structures, it will not work when loaded by BootProg. It must use either
BIOS services (e.g. int 10h, int 16h, int 13h and such) or access hardware
directly or both.

However, it is possible to create universal/hybrid programs that would work both
in DOS and when loaded by BootProg. BootProg will set registers si, di and bp to
the values 16381, 32749 and 65521 respectively before transferring control to
your program. Your program can then check the values in these registers and use
DOS services in DOS or something else instead on bare hardware. You can also
choose to make the program run with reduced functionality if not on DOS or vice
versa.


## How does it work?

Nothing special. It just finds STARTUP.BIN, loads it, performs any relocations
necessary for the .EXE type of programs, sets the dl register to the BIOS boot
drive number (e.g. 0, 80H), sets the magic numbers 16381, 32749 and 65521 in
registers si, di and bp respectively and passes control to your program.

If BootProg can't find STARTUP.BIN, it will print "NoFile" or "NF" to the
screen. If it fails to load the file due to a read error, it will print
"ReadErr" or "RE".


## Compilation

You'll need [NASM](https://nasm.us/) 2.10 or newer.

Then just do one or more of these as appropriate:

    $ nasm boot12.asm -f bin -o boot12.bin
    $ nasm boot16.asm -f bin -o boot16.bin
    $ nasm boot16.asm -f bin -o boot16l.bin -dUSE_LBA
    $ nasm boot32.asm -f bin -o boot32.bin
    $ nasm boot32.asm -f bin -o boot32c.bin -dUSE_CHS


## How do I put BootProg on my disk?

If you have a 1.44MB 3"5 floppy, just format it regularly with FAT12 in DOS or
Windows and then write flp144.bin to the very first sector of the floppy with
whatever tools you find/have for that. After that you can copy STARTUP.BIN to
the floppy using any regular means and off you go.

If you want to create an image of a 1.44MB 3"5 floppy, it might be even easier.
Compile the mkimg144.c program contained here with your favorite C compiler and
use it:

    mkimg144 [option(s)] [file(s)]
    
    Options:
    
    -bs <file>  Specifies the boot sector to use, e.g. "-bs flp144.bin"
    
    -o <file>   Specifies the name of the output file ("floppy.img" is the
                default, if this option isn't specified)
    
    -us         Uses the current time to set the volume ID of the FAT to a unique
                value (the volume ID is used to distinguish between different
                removable disks and detect disk change more accurately)

E.g: "mkimg144 -bs flp144.bin -o flp144.img -us startup.bin".
Btw, you can rename the supplied file "demo1.com" to "startup.bin" to try it
out.

For all other cases you'll need to become a little more familiar with FAT and a
little more intimate with disk tools and BootProg's source code.

You will need to populate the
[BPB](https://en.wikipedia.org/wiki/BIOS_parameter_block)'s of boot12.asm,
boot16.asm and boot32.asm with the values appropriate to the type and size of
the file system that you already have on a disk or that you intend to create on
the disk. See the source code, the BPB variables are located between comments
like these:

    ;;;;;;;;;;;;;;;;;;;;;
    ;; BPB starts here ;;
    ;;;;;;;;;;;;;;;;;;;;;
    ...
    ;;;;;;;;;;;;;;;;;;;
    ;; BPB ends here ;;
    ;;;;;;;;;;;;;;;;;;;

Note, there are two BPB parts in FAT32.

The best is to format your disk with some standard tools (e.g. FORMAT.COM in
DOS), extract the BPB values from the FAT-formatted disk, put them into BootProg
and then write thusly adjusted BootProg over the original boot sector.

You may find a disk editor handy when manipulating BPB values and/or reading/
writing boot sectors.

### Linux

On Linux you could use the dd tool to install BootProg. If you need to install
BootProg on /dev/sda1 (it could be /dev/hda1 or some such, you need to figure
out which it is), first, save the existing VBR to the sda1_old.vbr file:

    $ sudo dd if=/dev/sda1 of=sda1_old.vbr bs=1b count=1

**BACKUP THE sda1_old.vbr FILE TO BE ABLE TO RESTORE THE ORIGINAL VBR!**

Now install BootProg as the new VBR on /dev/sda1...

If you're doing it for FAT32 LBA:

    $ sudo cp boot32.bin sda1.vbr
    $ sudo dd if=sda1_old.vbr of=sda1.vbr bs=1 skip=3 seek=3 count=87 conv=notrunc
    $ sudo dd if=sda1.vbr of=/dev/sda1 bs=1b count=1

(Use boot32c.bin for FAT32 CHS.)

If you're doing it for FAT16 CHS:

    $ sudo cp boot16.bin sda1.vbr
    $ sudo dd if=sda1_old.vbr of=sda1.vbr bs=1 skip=3 seek=3 count=59 conv=notrunc
    $ sudo dd if=sda1.vbr of=/dev/sda1 bs=1b count=1

(Use boot16l.bin for FAT16 LBA.)

Similarly, if you're doing it for FAT12:

    $ sudo cp boot12.bin sda1.vbr
    $ sudo dd if=sda1_old.vbr of=sda1.vbr bs=1 skip=3 seek=3 count=59 conv=notrunc
    $ sudo dd if=sda1.vbr of=/dev/sda1 bs=1b count=1

Essentially, this preserves the VBR's BPB while replacing the VBR's code.

To uninstall BootProg from /dev/sda1 using the previously saved file
sda1_old.vbr:

    $ sudo dd if=sda1_old.vbr of=/dev/sda1 bs=1b count=1


## Limitations and implementation details

boot12.asm (flp144.asm) and boot16.asm require an i8086/i8088 or a better CPU.
boot32.asm naturally requires an i80386 or a better CPU.

boot12.asm (flp144.asm) was primarily written for floppies but may also be used
on hard disks on FAT12 primary partitions (
[file system ID](https://en.wikipedia.org/wiki/Partition_type) 1). On HDDs its
expected use is the boot sector of the partition (AKA
[VBR](https://en.wikipedia.org/wiki/Volume_boot_record)) and not the
[MBR](https://en.wikipedia.org/wiki/Master_boot_record).

boot16.asm was written for and tested on primary FAT16 partitions (file system
IDs 4, 6 and 0Eh). Its expected use is the boot sector of the partition (AKA
VBR) and not the MBR. By default boot16.asm assembles to operate using BIOS int
13h function 2, that is,
[CHS](https://en.wikipedia.org/wiki/Cylinder-head-sector)-based sector reads,
IOW, for old systems supporting only small HDDs, smaller than 8GB (or even
smaller than 504MB).
Pass "-dUSE_LBA" to NASM to assemble boot16.asm to operate using BIOS int 13h
function 42h, that is,
[LBA](https://en.wikipedia.org/wiki/Logical_block_addressing)-based sector
reads, IOW, for newer systems supporting HDDs larger than 8GB.

boot32.asm was written for and tested on primary FAT32 partitions (file system
IDs 0Bh and 0Ch). Its expected use is the boot sector of the partition (AKA VBR)
and not the MBR. By default boot32.asm assembles to operate using BIOS int 13h
function 42h, that is, LBA-based sector reads, IOW, for newer systems supporting
HDDs larger than 8GB.
Pass "-dUSE_CHS" to NASM to assemble boot32.asm to operate using BIOS int 13h
function 2, that is, CHS-based sector reads, IOW, for old systems supporting
only small HDDs, smaller than 8GB (or even smaller than 504MB).

Note that when BootProg is assembled to operate using CHS-based reads, on HDDs
it will first request the drive parameters (AKA disk geometry) using BIOS int
13h function 8. If a drive is moved between different systems, its geometry may
differ in the eyes of the different BIOSes, more so when it's a disk image for
a VM. Using function 8 helps in the situations when the disk is moved and its
BPB values for Sectors per Track and Heads per Cylinder cease to be appropriate.

Also, when using CHS-based reads, BootProg does its best to detect whether the
necessary sectors are reachable using CHS addressing, that is, their equivalent
LBA fits into 24 bits (less than some 16 million) and doesn't result in cylinder
numbers larger than 1023. If a sector isn't reachable, a read error is reported
instead of mysterious behavior. This should help identifying mistakes such as
when the partition is too big or is too far away from the HDD's start for CHS-
based reads to work. Move and/or resize the partition or use LBA-based BootProg
if you run into this problem.

BootProg does not check the size of STARTUP.BIN and reads into memory all of its
clusters, which means that up to 32767 extra bytes may be read from the disk and
written to the memory after the last byte of STARTUP.BIN (max cluster size is
32KB). It also means that you may append data to your program and it will be
loaded. You may create oversized .COM-style STARTUP.BIN larger than ~64KB,
however, note that the stack will naturally overwrite its contents from offset
65535 of the program segment (offset 65279 of the file) downwards.

If your PC has the full 640KB of conventional/DOS memory, you should be able to
safely load program files of sizes of up to 590KB (don't forget to include .EXE
stack size!). Calculation:

    (640*1024 - 600h (reserved at memory address 0) - 2048 (BootProg's stack) -
     512 (BootProg) - 6144 (FAT12 copy) - 32768 (max cluster size)) / 1024 =
    598KB


## Resources, links

*   [Master boot record](https://en.wikipedia.org/wiki/Master_boot_record)
*   [Volume boot record](https://en.wikipedia.org/wiki/Volume_boot_record)
*   [Partition type](https://en.wikipedia.org/wiki/Partition_type)
*   [Logical block addressing](https://en.wikipedia.org/wiki/Logical_block_addressing)
*   [Cylinder-head-sector](https://en.wikipedia.org/wiki/Cylinder-head-sector)
*   [File Allocation Table](https://en.wikipedia.org/wiki/File_Allocation_Table)
*   [FAT: General Overview of On-Disk Format Version 1.03](https://download.microsoft.com/download/1/6/1/161ba512-40e2-4cc9-843a-923143f3456c/fatgen103.doc)
*   [BIOS parameter block](https://en.wikipedia.org/wiki/BIOS_parameter_block)
*   [DOS MZ executable](https://en.wikipedia.org/wiki/DOS_MZ_executable)
*   [COM file](https://en.wikipedia.org/wiki/COM_file)
*   [MBiRa](https://github.com/alexfru/MBiRa)
*   [NASM](https://nasm.us/)
*   [Ralf Brown's Interrupt List](http://www.delorie.com/djgpp/doc/rbinter/)
