The "BootProg" Boot Sector


What is BootProg?

BootProg is a collection of 512-byte boot sectors (for the x86 PC) capable of
loading and executing a program from a FAT12-formatted floppy or a FAT16/32-
formatted hard disk (bootable USB sticks and CDs can also be made with
BootProg).

BootProg understands programs in the MS-DOS .COM or .EXE format. This makes
it possible to use existing 16-bit compilers such as Borland/Turbo C/C++,
Sybase/Open Watcom C/C++ and Smaller C and a variety of assemblers such as
NASM, FASM, TASM and MASM among the others.

BootProg doesn't require that the program occupy a contiguous span of sectors
or FAT clusters or reside at a specific fixed location on the disk. BootProg
faithfully parses the root directory and the chain of FAT clusters in order to
locate the program contents. The only requirement is that the program be named
"STARTUP.BIN" (without quotes). This makes updating the program easy. You just
need to update the file and you can reboot and execute it immediately.


What can BootProg be used for?

You can make a boot loader for your OS. The program that BootProg loads can be
your 2nd stage boot loader. Or, if your OS is relatively small, STARTUP.BIN
could contain the entire OS.

You can write low-level utilities to work with your PC's hardware and load them
with BootProg without having to jump through the hoops with your Windows, Linux
or even DOS.

You can make cool graphics demos or games that run on bare hardware.


What can't BootProg be used for?

Many things. Most importantly, if you make a DOS program that uses any MS-DOS
service functions (e.g. int 21h) or data structures, it will not work when
loaded by BootProg. It must use either BIOS services (e.g. int 10h, int 16h,
int 13h and such) or access hardware directly or both.

However, it is possible to create universal/hybrid programs that would work
both in DOS and when loaded by BootProg. BootProg will set registers si, di and
bp to the values 16381, 32749 and 65521 respectively before transferring control
to your program. Your program can then check the values in these registers and
use DOS services in DOS or something else instead on bare hardware. You can also
choose to make the program run with reduced functionality if not on DOS or
vice versa.


How does it work?

Nothing special. It just finds STARTUP.BIN, loads it, performs any relocations
necessary for the .EXE type of programs, sets the magic numbers 16381, 32749
and 65521 in registers si, di and bp respectively and passes control to your
program.

If BootProg can't find STARTUP.BIN, it will print "NF" to the screen. If it
fails to load the file due to a read error, it will print "RE". This is how the
FAT12 and FAT16 versions of BootProg work. The FAT32 version has much less space
for these errors and so in both above cases it will simply print "E".


How do I put BootProg on my disk?

If you have a 1.44MB 3"5 floppy, just format it regularly with FAT12 in DOS or
Windows and then write flp144.bin to the very first sector of the floppy with
whatever tools you find/have for that. After that you can copy STARTUP.BIN to
the floppy and off you go.

If you want to create an image of a 1.44MB 3"5 floppy, it might be even easier.
Compile the mkimg144.c program contained here with your favorite C compiler
and use it:

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

For all other cases you'll need to become a little more familiar with FAT and
a little more intimate with disk tools and BootProg's source code.

You will need to populate the BPB's of boot16.asm and boot32.asm with the
values appropriate to the type and size of the file system that you already have
on a disk or that you intend to create on the disk.
See the source code, these places are marked with question marks, for example:
  bpbBytesPerSector       DW      ?               ; 0x0B

The best is to format your disk with some standard tools (e.g. FORMAT.COM in
DOS), extract the BPB values from the FAT-formatted disk, put them into BootProg
and then write thusly adjusted BootProg over the original boot sector.

You may find a disk editor handy when manipulating BPB values and/or
reading/writing boot sectors.


Limitations

boot12.asm (flp144.asm) was not written for or tested on hard disks.

boot16.asm was written for and tested on primary FAT16 partitions (file system
IDs 4 and 6). It's expected use is the boot sector of the partition and not the
MBR. The FAT16 version may allocate up to 128KB of RAM for the entire FAT16,
leaving less room for STARTUP.BIN. But ~400KB left should still be plenty of
space for its code, data and stack.

boot32.asm was written for and tested on primary FAT32 partitions (file system
IDs 0Bh and 0Ch) and for BIOSes supporting function 42h of int 13h (IOW, for
systems supporting HDDs larger than 8GB). It's expected use is the boot sector
of the partition and not the MBR.

