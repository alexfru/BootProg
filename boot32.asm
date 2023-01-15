;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                          ;;
;;         "BootProg" Loader v 1.5 by Alexey Frunze (c) 2000-2023           ;;
;;                           2-clause BSD license.                          ;;
;;                                                                          ;;
;;                                                                          ;;
;;                              How to Compile:                             ;;
;;                              ~~~~~~~~~~~~~~~                             ;;
;; nasm boot32.asm -f bin -o boot32.bin                                     ;;
;;                                                                          ;;
;;                                                                          ;;
;;                                 Features:                                ;;
;;                                 ~~~~~~~~~                                ;;
;; - FAT32 supported using BIOS int 13h function 42h (IOW, it will only     ;;
;;   work with modern BIOSes supporting HDDs bigger than 8 GB)              ;;
;;                                                                          ;;
;; - Loads a 16-bit executable file in the MS-DOS .COM or .EXE format       ;;
;;   from the root directory of a disk and transfers control to it          ;;
;;   (the "ProgramName" variable holds the name of the file to be loaded)   ;;
;;                                                                          ;;
;; - Prints an error if the file isn't found or couldn't be read            ;;
;;   (the single-letter "E" message stands for both "Read Error" and        ;;
;;    "file Not Found")                                                     ;;
;;                                                                          ;;
;;                                                                          ;;
;;                             Known Limitations:                           ;;
;;                             ~~~~~~~~~~~~~~~~~~                           ;;
;; - Works only on the 1st MBR partition which must be a PRI DOS partition  ;;
;;   with FAT32 (File System ID: 0Bh,0Ch)                                   ;;
;;                                                                          ;;
;;                                                                          ;;
;;                                Known Bugs:                               ;;
;;                                ~~~~~~~~~~~                               ;;
;; - All bugs are fixed as far as I know. The boot sector has been tested   ;;
;;   on my HDD and an 8GB USB stick.                                        ;;
;;                                                                          ;;
;;                                                                          ;;
;;                               Memory Layout:                             ;;
;;                               ~~~~~~~~~~~~~~                             ;;
;; The diagram below shows the typical memory layout. The actual location   ;;
;; of the boot sector and its stack may be lower than A0000H if the BIOS    ;;
;; reserves memory for its Extended BIOS Data Area just below A0000H and    ;;
;; reports less than 640 KB of RAM via its Int 12H function.                ;;
;;                                                                          ;;
;;                                            physical address              ;;
;;                 +------------------------+ 00000H                        ;;
;;                 | Interrupt Vector Table |                               ;;
;;                 +------------------------+ 00400H                        ;;
;;                 |     BIOS Data Area     |                               ;;
;;                 +------------------------+ 00500H                        ;;
;;                 | PrtScr Status / Unused |                               ;;
;;                 +------------------------+ 00600H                        ;;
;;                 |      Loaded Image      |                               ;;
;;                 +------------------------+ nnnnnH                        ;;
;;                 |    Available Memory    |                               ;;
;;                 +------------------------+ A0000H - 512 - 2KB            ;;
;;                 |     2KB Boot Stack     |                               ;;
;;                 +------------------------+ A0000H - 512                  ;;
;;                 |       Boot Sector      |                               ;;
;;                 +------------------------+ A0000H                        ;;
;;                 |        Video RAM       |                               ;;
;;                                                                          ;;
;;                                                                          ;;
;;                   Boot Image Startup (register values):                  ;;
;;                   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                  ;;
;;  dl = BIOS boot drive number (e.g. 80H)                                  ;;
;;  cs:ip = program entry point                                             ;;
;;  ss:sp = program stack (don't confuse with boot sector's stack)          ;;
;;  COM program defaults: cs = ds = es = ss = 50h, sp = 0, ip = 100h        ;;
;;  EXE program defaults: ds = es = 50h, other stuff depends on EXE header  ;;
;;  Magic numbers:                                                          ;;
;;    si = 16381 (prime number 2**14-3)                                     ;;
;;    di = 32749 (prime number 2**15-19)                                    ;;
;;    bp = 65521 (prime number 2**16-15)                                    ;;
;;  The magic numbers let the program know whether it has been loaded by    ;;
;;  this boot sector or by MS-DOS, which may be handy for universal, bare-  ;;
;;  metal and MS-DOS programs.                                              ;;
;;                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[BITS 16]

ImageLoadSeg            equ     60h     ; <=07Fh because of "push byte ImageLoadSeg" instructions

[SECTION .text]
[ORG 0]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Boot sector starts here ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        jmp     short   start                   ; MS-DOS/Windows checks for this jump
        nop
bsOemName               DB      "BootProg"      ; 0x03

;;;;;;;;;;;;;;;;;;;;;;
;; BPB1 starts here ;;
;;;;;;;;;;;;;;;;;;;;;;

bpbBytesPerSector       DW      0               ; 0x0B
bpbSectorsPerCluster    DB      0               ; 0x0D
bpbReservedSectors      DW      0               ; 0x0E
bpbNumberOfFATs         DB      0               ; 0x10
bpbRootEntries          DW      0               ; 0x11
bpbTotalSectors         DW      0               ; 0x13
bpbMedia                DB      0               ; 0x15
bpbSectorsPerFAT        DW      0               ; 0x16
bpbSectorsPerTrack      DW      0               ; 0x18
bpbHeadsPerCylinder     DW      0               ; 0x1A
bpbHiddenSectors        DD      0               ; 0x1C
bpbTotalSectorsBig      DD      0               ; 0x20

;;;;;;;;;;;;;;;;;;;;
;; BPB1 ends here ;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;
;; BPB2 starts here ;;
;;;;;;;;;;;;;;;;;;;;;;

bsSectorsPerFAT32               DD      0               ; 0x24
bsExtendedFlags                 DW      0               ; 0x28
bsFSVersion                     DW      0               ; 0x2A
bsRootDirectoryClusterNo        DD      0               ; 0x2C
bsFSInfoSectorNo                DW      0               ; 0x30
bsBackupBootSectorNo            DW      0               ; 0x32
bsreserved             times 12 DB      0               ; 0x34
bsDriveNumber                   DB      0               ; 0x40
bsreserved1                     DB      0               ; 0x41
bsExtendedBootSignature         DB      0               ; 0x42
bsVolumeSerialNumber            DD      0               ; 0x43
bsVolumeLabel                   DB      "NO NAME    "   ; 0x47
bsFileSystemName                DB      "FAT32   "      ; 0x52

;;;;;;;;;;;;;;;;;;;;
;; BPB2 ends here ;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Boot sector code starts here ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

start:
        cld

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; How much RAM is there? ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        int     12h             ; get conventional memory size (in KBs)
        shl     ax, 6           ; and convert it to 16-byte paragraphs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reserve memory for the boot sector and its stack ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        sub     ax, 512 / 16    ; reserve 512 bytes for the boot sector code
        mov     es, ax          ; es:0 -> top - 512

        sub     ax, 2048 / 16   ; reserve 2048 bytes for the stack
        mov     ss, ax          ; ss:0 -> top - 512 - 2048
        mov     sp, 2048        ; 2048 bytes for the stack

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copy ourselves to top of memory ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        mov     cx, 256
        mov     si, 7C00h
        xor     di, di
        mov     ds, di
        rep     movsw

;;;;;;;;;;;;;;;;;;;;;;
;; Jump to the copy ;;
;;;;;;;;;;;;;;;;;;;;;;

        push    es
        push    byte main
        retf

main:
        push    cs
        pop     ds

        mov     [bsDriveNumber], dl     ; store BIOS boot drive number

        and     byte [bsRootDirectoryClusterNo+3], 0Fh ; mask cluster value
        mov     esi, [bsRootDirectoryClusterNo] ; esi=cluster # of root dir

RootDirReadContinue:
        push    byte ImageLoadSeg
        pop     es
        xor     bx, bx
        call    ReadCluster             ; read one cluster of root dir
        push    esi                     ; save esi=next cluster # of root dir
        pushf                           ; save carry="not last cluster" flag

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Look for the COM/EXE file to load and run ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        push    byte ImageLoadSeg
        pop     es
        xor     di, di                  ; es:di -> root entries array
        mov     si, ProgramName         ; ds:si -> program name

        mov     al, [bpbSectorsPerCluster]
        cbw
        mul     word [bpbBytesPerSector]; ax = bytes per cluster
        shr     ax, 5
        mov     dx, ax                  ; dx = # of dir entries to search in

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Looks for a file/dir by its name      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input:  DS:SI -> file name (11 chars) ;;
;;         ES:DI -> root directory array ;;
;;         DX = number of root entries   ;;
;; Output: ESI = cluster number          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FindName:
        mov     cx, 11
FindNameCycle:
        cmp     byte [es:di], ch
        jne     FindNameNotEnd
        jmp     ErrFind                 ; end of root directory (NULL entry found)
FindNameNotEnd:
        pusha
        repe    cmpsb
        popa
        je      FindNameFound
        add     di, 32
        dec     dx
        jnz     FindNameCycle           ; next root entry
        popf                            ; restore carry="not last cluster" flag
        pop     esi                     ; restore esi=next cluster # of root dir
        jc      RootDirReadContinue     ; continue to the next root dir cluster
        jmp     ErrFind                 ; end of root directory (dir end reached)
FindNameFound:
        push    word [es:di+14h]
        push    word [es:di+1Ah]
        pop     esi                     ; si = cluster no.

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load the entire file ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

        push    byte ImageLoadSeg
        pop     es
        xor     bx, bx
FileReadContinue:
        call    ReadCluster             ; read one cluster of root dir
        jc      FileReadContinue

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type detection, .COM or .EXE? ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        push    byte ImageLoadSeg
        pop     ds
        mov     ax, ds                  ; ax=ds=seg the file is loaded to

        cmp     word [0], 5A4Dh         ; "MZ" signature?

        je      RelocateEXE             ; yes, it's an EXE program

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup and run a .COM program ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        sub     ax, 10h                 ; "org 100h" stuff :)
        mov     es, ax
        mov     ds, ax
        mov     ss, ax
        xor     sp, sp
        push    es
        push    word 100h
        jmp     short Run

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Relocate, setup and run a .EXE program ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

RelocateEXE:

        add     ax, [08h]               ; ax = image base
        mov     cx, [06h]               ; cx = reloc items
        mov     bx, [18h]               ; bx = reloc table pointer

        jcxz    RelocationDone

ReloCycle:
        mov     di, [bx]                ; di = item ofs
        mov     dx, [bx+2]              ; dx = item seg (rel)
        add     dx, ax                  ; dx = item seg (abs)

        push    ds
        mov     ds, dx                  ; ds = dx
        add     [di], ax                ; fixup
        pop     ds

        add     bx, 4                   ; point to next entry
        loop    ReloCycle

RelocationDone:

        mov     bx, ax
        add     bx, [0Eh]
        mov     ss, bx                  ; ss for EXE
        mov     sp, [10h]               ; sp for EXE

        add     ax, [16h]               ; cs
        push    ax
        push    word [14h]              ; ip
Run:
        mov     dl, [cs:bsDriveNumber]  ; pass the BIOS boot drive

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the magic numbers so the program knows that it   ;;
;; has been loaded by this bootsector and not by MS-DOS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        mov     si, 16381 ; prime number 2**14-3
        mov     di, 32749 ; prime number 2**15-19
        mov     bp, 65521 ; prime number 2**16-15

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All done, transfer control to the program now ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        retf

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reads a FAT32 cluster        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inout:  ES:BX -> buffer      ;;
;;           ESI = cluster no   ;;
;; Output:   ESI = next cluster ;;
;;         ES:BX -> next addr   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ReadCluster:
        mov     ax, [bpbBytesPerSector]
        shr     ax, 2                           ; ax=# of FAT32 entries per sector
        cwde
        mov     ebp, esi                        ; ebp=esi=cluster #
        xchg    eax, esi
        cdq
        div     esi                             ; eax=FAT sector #, edx=entry # in sector
        movzx   edi, word [bpbReservedSectors]
        add     edi, [bpbHiddenSectors]
        add     eax, edi

        push    dx                              ; save dx=entry # in sector on stack
        mov     cx, 1
        call    ReadSectorLBA                   ; read 1 FAT32 sector

        pop     si                              ; si=entry # in sector
        add     si, si
        add     si, si
        and     byte [es:si+3], 0Fh             ; mask cluster value
        mov     esi, [es:si]                    ; esi=next cluster #

        lea     eax, [ebp-2]
        movzx   ecx, byte [bpbSectorsPerCluster]
        mul     ecx
        mov     ebp, eax

        movzx   eax, byte [bpbNumberOfFATs]
        mul     dword [bsSectorsPerFAT32]

        add     eax, ebp
        add     eax, edi

        call    ReadSectorLBA

        mov     ax, [bpbBytesPerSector]
        shr     ax, 4                   ; ax = paragraphs per sector
        mul     cx                      ; ax = paragraphs read

        mov     cx, es
        add     cx, ax
        mov     es, cx                  ; es:bx updated

        cmp     esi, 0FFFFFF8h          ; carry=0 if last cluster, and carry=1 otherwise
        ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reads a sector using BIOS Int 13h fn 42h ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input:  EAX = LBA                        ;;
;;         CX    = sector count             ;;
;;         ES:BX -> buffer address          ;;
;; Output: CF = 1 if error                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ReadSectorLBA:
        pushad

ReadSectorLBANext:
        pusha

        push    byte 0
        push    byte 0 ; 32-bit LBA only: up to 2TB disks
        push    eax
        push    es
        push    bx
        push    byte 1 ; sector count word = 1
        push    byte 16 ; packet size byte = 16, reserved byte = 0

        mov     ah, 42h
        mov     dl, [bsDriveNumber]
        mov     si, sp
        push    ss
        pop     ds
        int     13h
        push    cs
        pop     ds

        jc      short ErrRead
        add     sp, 16 ; the two instructions are swapped so as not to overwrite carry flag

        popa
        dec     cx
        jz      ReadSectorLBADone2      ; last sector

        add     bx, [bpbBytesPerSector] ; adjust offset for next sector
        add     eax, byte 1             ; adjust LBA for next sector
        jmp     short ReadSectorLBANext

ReadSectorLBADone2:
        popad
        ret

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error Messaging Code ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

ErrRead:
ErrFind:
        mov     ax, 0E00h+'E'
        mov     bx, 7
        int     10h
        jmp     short $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fill free space with zeroes ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                times (512-13-($-$$)) db 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Name of the file to load and run ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ProgramName     db      "STARTUP BIN"   ; name and extension each must be
                                        ; padded with spaces (11 bytes total)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of the sector ID ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

                dw      0AA55h          ; BIOS checks for this ID
