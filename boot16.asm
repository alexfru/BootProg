;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                          ;;
;;         "BootProg" Loader v 2.0 by Alexey Frunze (c) 2000-2023           ;;
;;                           2-clause BSD license.                          ;;
;;                                                                          ;;
;;                                                                          ;;
;;                              How to Compile:                             ;;
;;                              ~~~~~~~~~~~~~~~                             ;;
;; nasm boot16.asm -f bin -o boot16.bin                                     ;;
;; nasm boot16.asm -f bin -o boot16l.bin -dUSE_LBA                          ;;
;;                                                                          ;;
;;                                                                          ;;
;;                                 Features:                                ;;
;;                                 ~~~~~~~~~                                ;;
;; - Loads a 16-bit executable file in the MS-DOS .COM or .EXE format       ;;
;;   from the root directory of a disk and transfers control to it          ;;
;;   (the "ProgramName" variable holds the name of the file to be loaded)   ;;
;;                                                                          ;;
;; - Prints an error if the file isn't found or couldn't be read            ;;
;;   (the "RE" and "ReadErr" messages stand for "Read Error",               ;;
;;    the "NF" and "NoFile" messages stand for "No File"/"file Not Found")  ;;
;;                                                                          ;;
;; - FAT16 supported, to be used on hard drives                             ;;
;;                                                                          ;;
;; - FAT16 reads supported using BIOS int 13h function 2 (AKA CHS-based     ;;
;;   reads; IOW, this will work with older BIOSes supporting HDDs smaller   ;;
;;   than 8 GB) or using BIOS int 13h function 42h (AKA LBA-based reads;    ;;
;;   IOW, this will work with modern BIOSes supporting HDDs bigger than     ;;
;;   8 GB).                                                                 ;;
;;                                                                          ;;
;; - The code can be assembled for either CHS or LBA operation. CHS is the  ;;
;;   default. For LBA pass "-dUSE_LBA" (without the quotation marks) to     ;;
;;   NASM.                                                                  ;;
;;                                                                          ;;
;; - Supports i8086/i8088 CPUs.                                             ;;
;;                                                                          ;;
;;                                                                          ;;
;;                             Known Limitations:                           ;;
;;                             ~~~~~~~~~~~~~~~~~~                           ;;
;; - Works only on the 1st MBR partition which must be a PRImary DOS        ;;
;;   partition with FAT16 (File System ID: 4, 6, 0Eh)                       ;;
;;                                                                          ;;
;;                                                                          ;;
;;                                Known Bugs:                               ;;
;;                                ~~~~~~~~~~~                               ;;
;; - All bugs are fixed as far as I know. The boot sector has been tested   ;;
;;   on my HDD.                                                             ;;
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
;;                 +------------------------+ 00600H <- ImageLoadSeg * 16   ;;
;;                 |      Loaded Image      |                               ;;
;;                 +------------------------+ nnnnnH                        ;;
;;                 |    Available Memory    |                               ;;
;;                 +------------------------+ A0000H - 2KB - 512            ;;
;;                 |       Boot Sector      |                               ;;
;;                 +------------------------+ A0000H - 2KB                  ;;
;;                 |     2KB Boot Stack     |                               ;;
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
;;  EXE program defaults: ds = 60h, other stuff depends on EXE header       ;;
;;  Magic numbers:                                                          ;;
;;    si = 16381 (prime number 2**14-3)                                     ;;
;;    di = 32749 (prime number 2**15-19)                                    ;;
;;    bp = 65521 (prime number 2**16-15)                                    ;;
;;  The magic numbers let the program know whether it has been loaded by    ;;
;;  this boot sector or by MS-DOS, which may be handy for universal, bare-  ;;
;;  metal and MS-DOS programs.                                              ;;
;;                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%ifndef USE_LBA
  %define USE_CHS
%elifdef USE_CHS
  %error "Can't have USE_CHS and USE_LBA both defined"
%endif

BITS 16

CPU 8086

%ifndef LOADSEG
  %define LOADSEG 60h
%endif
%ifnum LOADSEG
%else
  %error "LOADSEG isn't a number"
%endif
ImageLoadSeg            equ     LOADSEG

%ifndef STACKSIZE
  %define STACKSIZE 2048
%endif
%ifnum STACKSIZE
%else
  %error "STACKSIZE isn't a number"
%endif
StackSize               equ     STACKSIZE

%ifndef PROGNAME
  %define PROGNAME "STARTUP BIN"
%endif
%ifstr PROGNAME
  %strlen PROGNAMELEN PROGNAME
  %if PROGNAMELEN != 11
    %error "PROGNAME isn't an 11-char string"
  %endif
%else
  %error "PROGNAME isn't a string"
%endif

ORG 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Boot sector starts here ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Base:
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
bsFileSystem            DB      "FAT16   "      ; 0x36

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Boot sector code starts here ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

start:
        cld

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; How much RAM is there? ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        int     12h             ; get conventional memory size (in KBs)
        mov     cl, 6
        shl     ax, cl          ; and convert it to 16-byte paragraphs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reserve memory for the boot sector and its stack ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        sub     ax, (512 + StackSize) / 16 ; reserve space for code and stack
        mov     es, ax          ; intending (cs=ds=ss):0 -> top - 512 - StackSize
        cli                             ; ss:sp change protection for 8088
        mov     ss, ax
        mov     sp, 512 + StackSize
        sti

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
        mov     cl, main
        push    cx
        retf

main:
        push    cs
        pop     ds

        mov     [bsDriveNumber], dl     ; store BIOS boot drive number

%ifdef USE_LBA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check for int 13h extensions for LBA reads ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        mov     ah, 41h                 ; clobbers AX,BX,CX,DH
        mov     bx, 55AAh
        int     13h
        jc      NoExtensions
        sub     bx, 0AA55h
        jnz     NoExtensions
        shr     cx, 1
        jc      GotExtensions

NoExtensions:
        jmp     ReadError

GotExtensions:
                ; bx=0 from above:
                ; buffer offset; also for shorter encoding of memory operands
%endif

%ifdef USE_CHS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get drive parameters.                            ;;
;; If a drive is moved between different systems,   ;;
;; its geometry may differ in the eyes of the       ;;
;; different BIOSes, more so when it's a disk image ;;
;; for a VM.                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        mov     ah, 8                   ; clobbers AX,BL,CX,DX,ES:DI
        int     13h
        mov     bx, 0   ; buffer offset; also for shorter encoding of memory operands
        jc      NoNewGeometry           ; use the above BPB if no BIOS info

        and     cx, 63
        mov     [bx+(bpbSectorsPerTrack-Base)], cx

        mov     cl, dh
        inc     cx
        mov     [bx+(bpbHeadsPerCylinder-Base)], cx

NoNewGeometry:
%endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load the root directory in its entirety ;;
;; (16KB max)                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        mov     ax, 32
        mul     word [bx+(bpbRootEntries-Base)]
        div     word [bx+(bpbBytesPerSector-Base)]
        xchg    cx, ax                  ; cx = root directory size in sectors

        mov     al, [bx+(bpbNumberOfFATs-Base)]
        cbw
        mul     word [bx+(bpbSectorsPerFAT-Base)]
                                        ; dx:ax = FATs size in sectors =
                                        ; LBA of root dir (relative to 1st FAT start)

        mov     di, ImageLoadSeg
        mov     es, di                  ; es:bx -> buffer for root dir & file

        push    es
        call    ReadSector              ; cx = 0
        pop     es

        push    dx
        push    ax                      ; FATs size + root dir size in sectors (1st stack parameter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Look for the COM/EXE file to load and run ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        mov     di, bx                  ; es:di -> root entries array
        mov     dx, [bx+(bpbRootEntries-Base)]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Looks for a file/dir by its name      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input:  ES:DI -> root directory array ;;
;;         DX = number of root entries   ;;
;;         CH = 0                        ;;
;; Output: DI = cluster number           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FindName:
        mov     si, ProgramName         ; ds:si -> program name
        mov     cl, 11                  ; length of ProgramName
        cmp     byte [es:di], ch
        je      FindNameFailed          ; end of root directory
        push    di
        repe    cmpsb
        pop     di
        je      FindNameFound
        add     di, 32
        dec     dx
        jnz     FindName                ; next root entry
FindNameFailed:
        call    Error
%ifndef SHORTERR
        db      "NoFile", 0
%else
        db      "NF"
%endif
FindNameFound:
        mov     di, [es:di+1Ah]         ; di = cluster no.

        push    es                      ; ImageLoadSeg

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load the entire file ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reads a FAT16 cluster                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input:  ES:BX -> buffer address                               ;;
;;         BX = 0                                                ;;
;;         DI = cluster no                                       ;;
;;         dword [SP+2] = FATs size + root dir size (in sectors) ;;
;; Output: DI = next cluster                                     ;;
;;         ES:BX -> next addr (new ES:old BX)                    ;;
;;         BX = 0                                                ;;
;; Clobbers: AX, CX, DX, SI, BP                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ReadCluster:
        mov     bp, sp

        push    di                      ; save di=cluster # on stack

        mov     ax, [bx+(bpbBytesPerSector-Base)]
        shr     ax, 1                   ; ax=# of FAT16 entries per sector
        xchg    ax, di
        xor     dx, dx
        div     di                      ; ax=FAT sector #, dx=entry # in sector

        push    dx                      ; save dx=entry # in sector on stack

        xor     dx, dx                  ; dx:ax = LBA of a FAT16 sector
        mov     cx, 1
        push    es
        call    ReadSector              ; read 1 FAT16 sector
        pop     es

        pop     di                      ; di=entry # in sector
        add     di, di
        mov     cx, [es:bx+di]          ; cx=next cluster #

        pop     di                      ; di=prev cluster #
        lea     ax, [di-2]
        mov     di, cx                  ; di=next cluster #
        xor     ch, ch
        mov     cl, [bx+(bpbSectorsPerCluster-Base)]
                ; cx = sector count
        mul     cx

        add     ax, [bp+2]
        adc     dx, [bp+4]
                ; dx:ax = cluster LBA

        call    ReadSector              ; read 1 cluster

        cmp     di, 0FFF8h
        jc      ReadCluster             ; if not End Of File

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type detection, .COM or .EXE? ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        cli                             ; ss:sp change protection for 8088

        mov     dl, [bx+(bsDriveNumber-Base)] ; pass the BIOS boot drive

        pop     ax                      ; ImageLoadSeg
        mov     ds, ax                  ; ax=ds=seg the file is loaded to
        cmp     word [bx], 5A4Dh        ; "MZ" signature?

        je      RelocateEXE             ; yes, it's an EXE program

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup and run a .COM program ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        sub     ax, 10h                 ; "org 100h" stuff :)
        mov     es, ax
        mov     ds, ax
        mov     ss, ax
        xor     sp, sp
        mov     bh, 1                   ; ax:bx = cs:ip of entry point
        jmp     short Run

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Relocate, setup and run a .EXE program ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

RelocateEXE:
        add     ax, [bx+08h]            ; ax = image base
        mov     cx, [bx+06h]            ; cx = reloc items
        mov     si, [bx+18h]            ; si = reloc table pointer

        jcxz    RelocationDone

ReloLoop:
        add     [si+2], ax              ; item seg (rel):ofs -> item seg (abs):ofs
        les     di, [si]                ; es:di = item seg (abs):ofs

        add     [es:di], ax             ; fixup item

        add     si, 4                   ; point to next entry
        loop    ReloLoop

RelocationDone:

        mov     cx, ax
        add     cx, [bx+0Eh]
        mov     ss, cx                  ; ss for EXE
        mov     sp, [bx+10h]            ; sp for EXE

        add     ax, [bx+16h]
        mov     bx, [bx+14h]            ; ax:bx = cs:ip of entry point

Run:
        push    ax
        push    bx
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
        sti
        retf

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reads a sector using BIOS Int 13h fn 2 or 42h   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input:  DX:AX = LBA (relative to 1st FAT start) ;;
;;         CX    = sector count                    ;;
;;         ES:BX -> buffer address                 ;;
;;         BX    = 0                               ;;
;; Output: DX:AX = next LBA                        ;;
;;         CX    = 0                               ;;
;;         ES:BX -> next addr (new ES:old BX)      ;;
;;         BX    = 0                               ;;
;; Clobbers: SI                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ReadSector:
        push    di

ReadSectorNext:
        mov     di, 5                   ; attempts to read

ReadSectorRetry:
        push    ax
        push    dx
        push    cx

        add     ax, [bx+(bpbReservedSectors-Base)]
        adc     dx, bx ; Can't overflow (bpbTotalSectors(Big) includes bpbReservedSectors).

%ifdef USE_LBA
        xor     cx, cx
        add     ax, [bx+(bpbHiddenSectors-Base)]
        adc     dx, [bx+(bpbHiddenSectors-Base)+2]
        adc     cx, bx ; Support FAT16 volumes at 2TB offset from HDD start.

        push    bx ; LBA 48...63: 0
        push    cx ; LBA 32...47: 0 or 1
        push    dx ; LBA 16...31
        push    ax ; LBA  0...15
        push    es
        push    bx
        mov     cl, 1 ; sector count word = 1
        push    cx
        mov     cl, 16 ; packet size byte = 16, reserved byte = 0
        push    cx

        mov     si, sp                  ; ds:si -> packet for fn 42h

        mov     ah, 42h                 ; clobbers AX
                                        ; ah = 42h = read function no.
        mov     dl, [bx+(bsDriveNumber-Base)]
                ; dl = drive no.

        int     13h                     ; read sectors

        lea     sp, [si+16]             ; remove packet from stack
%endif

%ifdef USE_CHS
        add     ax, [bx+(bpbHiddenSectors-Base)]
        adc     dx, [bx+(bpbHiddenSectors-Base)+2]

        ; Limit the LBA to 24 bits, which should avoid most #DEs
        ; (but probably not all) in the following divisions in case
        ; this FAT16 volume isn't fully CHS-addressable.
        jc      ReadError
        test    dh, dh
        jnz     ReadError

        ; Divide 32-bit LBA in dx:ax by 16-bit bpbSectorsPerTrack
        ; with 32-bit quotient and 16-bit remainder.
        ; This avoids division overflows with large LBAs and
        ; supports disks up to 8 GB in size (the BIOS int 13h read
        ; function (ah = 2) takes cylinder:head:sector (AKA CHS)
        ; that's at most 24-bit, IOW, the function takes LBAs up to
        ; ~16 million, which with 512-byte sectors gives 8GB).
        xchg    ax, cx                  ; save LBA's low word in cx
        xchg    ax, dx                  ; will first divide LBA's hi word

        cwd
        div     word [bx+(bpbSectorsPerTrack-Base)]
                ; ax = (LBA / 65536) / SPT = (LBA / SPT) / 65536
                ; dx = (LBA / 65536) % SPT
        xchg    ax, cx                  ; will next divide LBA's low word
        div     word [bx+(bpbSectorsPerTrack-Base)]
                ; cx:ax = LBA / SPT
                ; dx = LBA % SPT         = sector - 1

        xchg    cx, dx
        inc     cx
                ; cx = sector no.

        div     word [bx+(bpbHeadsPerCylinder-Base)]
                ; ax = (LBA / SPT) / HPC = cylinder
                ; dx = (LBA / SPT) % HPC = head

        mov     ch, al
                ; ch = LSB 0...7 of cylinder no.

        cmp     ah, 4
        jae     ReadError       ; limit cylinder no. to less than 1024

        ror     ah, 1
        ror     ah, 1 ; excess bits of cylinder no. must be 0 anyway
        or      cl, ah
                ; cl = MSB 8...9 of cylinder no. + sector no.

        mov     dh, dl
                ; dh = head no.

        mov     ax, 201h                ; clobbers AX
                                        ; al = sector count = 1
                                        ; ah = 2 = read function no.

        mov     dl, [bx+(bsDriveNumber-Base)]
                ; dl = drive no.

        int     13h                     ; read sectors
%endif

%ifdef INJ_ERROR
        stc
%endif
%ifdef INJ_RETRY
        call    InjectReadRetry
%endif
        jnc     ReadSectorDone          ; CF = 0 if no error

        mov     ah, 0                   ; clobbers AX
                                        ; ah = 0 = reset function
        int     13h                     ; reset drive

        pop     cx
        pop     dx
        pop     ax

        dec     di
        jnz     short ReadSectorRetry   ; extra attempt

ReadError:
        call    Error
%ifndef SHORTERR
        db      "ReadErr", 0
%else
        db      "RE"
%endif

ReadSectorDone:
        mov     dx, [bx+(bpbBytesPerSector-Base)]
        mov     cl, 4
        shr     dx, cl                  ; dx = sector size in paragraphs
        mov     ax, es
        add     ax, dx
        mov     es, ax                  ; es updated

        pop     cx
        pop     dx
        pop     ax

        inc     ax
        jnz     ReadSectorIncd
        inc     dx
ReadSectorIncd:                         ; adjust LBA for next sector

        loop    ReadSectorNext          ; if not last sector

        pop     di
        ret

%ifdef INJ_RETRY
;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Read retry injection ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
InjectReadRetry:
        jc      InjectReadRetryDone
        cmp     di, 2                   ; carry=1 IFF di<2
        cmc                             ; carry=1 IFF di>=2
InjectReadRetryDone:
        ret
%endif

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error Messaging Code ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

Error:
        pop     si
        mov     ah, 0Eh
        mov     bx, 7

%ifndef SHORTERR
ErrorNext:
        lodsb
Halt:
        hlt
        test    al, al
        jz      Halt
        int     10h
        jmp     short ErrorNext
%else
        lodsb
        int     10h                     ; 1st char
        lodsb
        int     10h                     ; 2nd/last char
Halt:
        hlt
        jmp     short Halt
%endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fill free space with zeroes ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                times (512-13-($-$$)) db 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Name of the file to load and run ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ProgramName     db      PROGNAME        ; name and extension each must be
                                        ; padded with spaces (11 bytes total)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of the sector ID ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

                dw      0AA55h          ; BIOS checks for this ID
