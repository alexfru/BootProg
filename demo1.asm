; "Universal" test application that can either run in DOS or be
; loaded by the "BootProg" boot sector.
;
; Assemble with NASM for DOS:
;   nasm -f bin demo1.asm -o demo1.com
;
; Assemble with NASM for loading from the "BootProg" boot sector:
;   nasm -f bin demo1.asm -o startup.bin
;   copy startup.bin to the root directory of the disk with the
;     "BootProg" boot sector installed or create a bootable floppy
;     image with both:
;       mkimg144 -bs flp144.bin -o flp144.img -us startup.bin

bits 16
org 0x100

    ; check the magic numbers set by the boot sector
    xor     si, 16381
    xor     di, 32749
    xor     bp, 65521
    or      si, di
    or      si, bp
    jnz     dos ; if no match, it's DOS

    ; booted directly from the boot sector
    mov si, msghi
    call prints
halt:
    hlt
    jmp halt

    ; executed from DOS
dos:
    mov si, msghidos
    call prints
    ret

prints:
    cld
    mov     ah, 0xE
    mov     bx, 7
.1:
    lodsb
    test    al, al
    jz      .2
    int     0x10
    jmp     .1
.2:
    ret

msghi db "Hello, World! (booted)", 13, 10, 0
msghidos db "Hello, World! (DOS)", 13, 10, 0
