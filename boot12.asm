;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                          ;;
;;         "BootProg" Loader v 1.5 by Alexey Frunze (c) 2000-2023           ;;
;;                           2-clause BSD license.                          ;;
;;                                                                          ;;
;;                                                                          ;;
;;                              How to Compile:                             ;;
;;                              ~~~~~~~~~~~~~~~                             ;;
;; nasm boot12.asm -f bin -o boot12.bin                                     ;;
;;                                                                          ;;
;;                                                                          ;;
;;                                 Features:                                ;;
;;                                 ~~~~~~~~~                                ;;
;; - FAT12 supported, best suited for floppies                              ;;
;;                                                                          ;;
;; - Loads a 16-bit executable file in the MS-DOS .COM or .EXE format       ;;
;;   from the root directory of a disk and transfers control to it          ;;
;;   (the "ProgramName" variable holds the name of the file to be loaded)   ;;
;;                                                                          ;;
;; - Prints an error if the file isn't found or couldn't be read            ;;
;;   (the "RE" message stands for "Read Error",                             ;;
;;    the "NF" message stands for "file Not Found")                         ;;
;;   and waits for a key to be pressed, then executes the Int 19h           ;;
;;   instruction and lets the BIOS continue bootstrap.                      ;;
;;                                                                          ;;
;;                                                                          ;;
;;                             Known Limitations:                           ;;
;;                             ~~~~~~~~~~~~~~~~~~                           ;;
;; - Works only on the 1st MBR partition which must be a PRI DOS partition  ;;
;;   with FAT12 (File System ID: 1)                                         ;;
;;                                                                          ;;
;;                                                                          ;;
;;                                Known Bugs:                               ;;
;;                                ~~~~~~~~~~~                               ;;
;; - All bugs are fixed as far as I know. The boot sector has been tested   ;;
;;   on the following types of diskettes:                                   ;;
;;   - 360KB 5"25                                                           ;;
;;   - 1.2MB 5"25                                                           ;;
;;   - 1.44MB 3"5                                                           ;;
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
;;  dl = BIOS boot drive number (e.g. 0, 80H)                               ;;
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

;;;;;;;;;;;;;;;;;;;;;
;; BPB starts here ;;
;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;
;; BPB ends here ;;
;;;;;;;;;;;;;;;;;;;

bsDriveNumber           DB      0               ; 0x24
bsUnused                DB      0               ; 0x25
bsExtBootSignature      DB      0               ; 0x26
bsSerialNumber          DD      0               ; 0x27
bsVolumeLabel           DB      "NO NAME    "   ; 0x2B
bsFileSystem            DB      "FAT12   "      ; 0x36

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reserve memory for the FAT12 image (6KB max) ;;
;; and load it in its entirety                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        mov     ax, [bpbBytesPerSector]
        shr     ax, 4                   ; ax = sector size in paragraphs
        mov     cx, [bpbSectorsPerFAT]  ; cx = FAT size in sectors
        mul     cx                      ; ax = FAT size in paragraphs

        mov     di, ss
        sub     di, ax
        mov     es, di
        xor     bx, bx                  ; es:bx -> buffer for the FAT

        mov     ax, [bpbHiddenSectors]
        mov     dx, [bpbHiddenSectors+2]
        add     ax, [bpbReservedSectors]
        adc     dx, bx                  ; dx:ax = LBA

        call    ReadSector

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reserve memory for the root directory ;;
;; and load it in its entirety           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        mov     bx, ax
        mov     di, dx                  ; save LBA to di:bx

        mov     ax, 32

        mov     si, [bpbRootEntries]
        mul     si
        div     word [bpbBytesPerSector]
        mov     cx, ax                  ; cx = root directory size in sectors

        mov     al, [bpbNumberOfFATs]
        cbw
        mul     word [bpbSectorsPerFAT]
        add     ax, bx
        adc     dx, di                  ; dx:ax = LBA

        push    es                      ; push FAT segment (2nd parameter)

        push    byte ImageLoadSeg
        pop     es
        xor     bx, bx                  ; es:bx -> buffer for root directory

        call    ReadSector

        add     ax, cx
        adc     dx, bx                  ; adjust LBA for cluster data

        push    dx
        push    ax                      ; push LBA for data (1st parameter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Look for the COM/EXE file to load and run ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        mov     di, bx                  ; es:di -> root entries array
        mov     dx, si                  ; dx = number of root entries
        mov     si, ProgramName         ; ds:si -> program name

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Looks for a file/dir by its name      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input:  DS:SI -> file name (11 chars) ;;
;;         ES:DI -> root directory array ;;
;;         DX = number of root entries   ;;
;; Output: SI = cluster number           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FindName:
        mov     cx, 11
FindNameCycle:
        cmp     byte [es:di], ch
        je      FindNameFailed          ; end of root directory
        pusha
        repe    cmpsb
        popa
        je      FindNameFound
        add     di, 32
        dec     dx
        jnz     FindNameCycle           ; next root entry
FindNameFailed:
        jmp     ErrFind
FindNameFound:
        mov     si, [es:di+1Ah]         ; si = cluster no.

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load the entire file ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

ReadNextCluster:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reads a FAT12 cluster      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input:  ES:BX -> buffer    ;;
;;         SI = cluster no    ;;
;; Output: SI = next cluster  ;;
;;         ES:BX -> next addr ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ReadCluster:
        mov     bp, sp

        lea     ax, [si-2]
        xor     ch, ch
        mov     cl, [bpbSectorsPerCluster]
                ; cx = sector count
        mul     cx

        add     ax, [bp]
        adc     dx, [bp+1*2]
                ; dx:ax = LBA

        call    ReadSector

        mov     ax, [bpbBytesPerSector]
        shr     ax, 4                   ; ax = paragraphs per sector
        mul     cx                      ; ax = paragraphs read

        mov     cx, es
        add     cx, ax
        mov     es, cx                  ; es:bx updated

        mov     ax, 3
        mul     si
        shr     ax, 1
        xchg    ax, si                  ; si = cluster * 3 / 2

        push    ds
        mov     ds, [bp+2*2]            ; ds = FAT segment
        mov     si, [si]                ; si = next cluster
        pop     ds

        jnc     ReadClusterEven

        shr     si, 4

ReadClusterEven:
        and     si, 0FFFh               ; mask cluster value
ReadClusterDone:

        cmp     si, 0FF8h
        jc      ReadNextCluster         ; if not End Of File

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reads a sector using BIOS Int 13h fn 2 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input:  DX:AX = LBA                    ;;
;;         CX    = sector count           ;;
;;         ES:BX -> buffer address        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ReadSector:
        pusha

ReadSectorNext:
        mov     di, 5                   ; attempts to read

ReadSectorRetry:
        pusha

        div     word [bpbSectorsPerTrack]
                ; ax = LBA / SPT
                ; dx = LBA % SPT         = sector - 1

        mov     cx, dx
        inc     cx
                ; cx = sector no.

        xor     dx, dx
        div     word [bpbHeadsPerCylinder]
                ; ax = (LBA / SPT) / HPC = cylinder
                ; dx = (LBA / SPT) % HPC = head

        mov     ch, al
                ; ch = LSB 0...7 of cylinder no.
        shl     ah, 6
        or      cl, ah
                ; cl = MSB 8...9 of cylinder no. + sector no.

        mov     dh, dl
                ; dh = head no.

        mov     dl, [bsDriveNumber]
                ; dl = drive no.

        mov     ax, 201h
                                        ; al = sector count = 1
                                        ; ah = 2 = read function no.

        int     13h                     ; read sectors
        jnc     ReadSectorDone          ; CF = 0 if no error

        xor     ah, ah                  ; ah = 0 = reset function
        int     13h                     ; reset drive

        popa
        dec     di
        jnz     ReadSectorRetry         ; extra attempt
        jmp     short ErrRead

ReadSectorDone:
        popa
        dec     cx
        jz      ReadSectorDone2         ; last sector

        add     bx, [bpbBytesPerSector] ; adjust offset for next sector
        add     ax, 1
        adc     dx, 0                   ; adjust LBA for next sector
        jmp     short ReadSectorNext

ReadSectorDone2:
        popa
        ret

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error Messaging Code ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

ErrRead:
        mov     si, MsgErrRead
        jmp     short Error
ErrFind:
        mov     si, MsgErrFind
Error:
        mov     ah, 0Eh
        mov     bx, 7

        lodsb
        int     10h                     ; 1st char
        lodsb
        int     10h                     ; 2nd char

        xor     ah, ah
        int     16h                     ; wait for a key...
        mov     dl, [bsDriveNumber]     ; restore BIOS boot drive number
        int     19h                     ; bootstrap

;;;;;;;;;;;;;;;;;;;;;;
;; String constants ;;
;;;;;;;;;;;;;;;;;;;;;;

MsgErrRead      db      "RE"
MsgErrFind      db      "NF"

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
