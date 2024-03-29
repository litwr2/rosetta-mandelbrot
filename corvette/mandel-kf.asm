;for pasmo assembler
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;Thanks to reddie for some help with optimization
;
;512x256 (fullscreen) Mandelbrot for the Corvette, 8 colors (color write mode)

BDOS equ 5

RGBASE2 EQU     0FA00H  ;ROMB1, ODOSA, NDOS, BASIC
RGBASE3 EQU     0FF00H  ;DOSA, DOSG1

SYSREG  EQU     7FH
DOSG1   EQU     3CH
ODOSA   EQU     1CH

NCREG   EQU     0BFH  ;color reg

INTRV EQU 0F7D0H

HSize equ 512

org #100

sqrtab macro
    ld a,l
    and $fe
    ld l,a
    ld a,h
    add a,high(sqrbase)
    ld h,a
endm

start
    ld de,msg
    ld c,9
    call BDOS
    call waitk

    ld hl,$fafb   ;palette
    ld a,$80
    ld c,16
lo3 ld (hl),a
    add a,$11
    dec c
    jp nz,lo3

    ld de,curoff
    ld c,9
    call BDOS
    call clscursor

    ld hl,(INTRV)
    ld (KL+1),hl    ;prepare the timer handler

    ld hl,sqrbase
    push hl
    ld bc,0
    ld d,b
    ld e,c
sqrloop:
    pop hl
    ld (hl),c
    inc l
    ld (hl),b
    inc hl
    push hl
    inc e
    push de
    ld h,d
    ld l,e
    add hl,hl
    ld d,l
    ld e,h
    ld a,e
r0l:
    ld hl,0
    add hl,de
    ld (r0l+1),hl
    adc a,c
    ld c,a
    ld a,0
    adc a,b
    ld b,a
r4l:
    ld hl,sqrbase   ;the sqrbase lower/minus part
    dec hl
    ld (hl),b
    dec l
    ld (hl),c
    ld (r4l+1),hl
    pop de
    jp c,mandel0

    inc de
    jp sqrloop

mandel0:
    pop hl
mandel:
    ld hl,(dataindex)
    ld a,(hl)
    ld (dx),a
    inc hl
    ld a,(hl)
    ld (dy),a
    inc hl
    ld a,(hl)
    ld (x0),a
    inc hl
    ld a,(hl)
    ld (x0+1),a
    inc hl
    ld a,(hl)
    ld (niter),a
    inc a
    inc a
    ld (hl),a
    inc hl
    ld a,low(data+5*12)
    cp l
    jp nz,le1

    ld a,high(data+5*12)
    cp h
    jp nz,le1

    ld hl,data
le1 ld (dataindex),hl
    ld hl,0
    ld (tilo),hl
    ld (tihi),hl
    ld hl,KINTR
    ld (INTRV),hl   ;start timer
    ld hl,$403f  ;scrtop
    push hl
    xor a   ;sets C=0
    ld a,(dy)
    rra    ;C=0
    ld h,a
    ld a,0
    rra
    ld l,a       ;dy*128
    ld (r5),hl
loop0:
x0 equ $+1
    ld hl,0
    ld (r4),hl
loop2:
dx equ $+1
    ld hl,0ff00h
    ex de,hl
    ld hl,(r4)
    add hl,de
    ld (r4),hl  ;r4 += dx
    ex de,hl    ;de = r0
    ld hl,(r5)  ;hl = r1
niter equ $+1
    ld a,0
loc1:
    ld (ixhmem),a
    push hl
    sqrtab
    ld c,(hl)
    inc l
    ld b,(hl)   ;bc = r3 = sqr(r1)
    pop hl
    add hl,de   ;r1 += r0
    ex de,hl    ;de - r1, hl - r0, bc - r3
    sqrtab
    ld a,(hl)
    inc l
    ld h,(hl)
    ld l,a       ;r0 = sqr(r0)
    add hl,bc    ;r0 += r3
    ld a,h
    and $f8      ;sets C=0
    jp nz,loc2

    ex de,hl    ;de - r0, hl - r1
    sqrtab
    ld a,(hl)
    inc l
    ld h,(hl)
    ld l,a       ;r1 = sqr(r1)
    ld a,l
    sub e
    ld l,a
    ld a,h
    sbc a,d
    ld h,a      ;r1 -= r0
    ex de,hl    ;de - r1, hl - r0

    dec bc
    ld a,c
    cpl
    ld c,a
    ld a,b
    cpl
    ld b,a
    add hl,bc  ;r0 -= r3
    add hl,bc  ;r0 -= r3
    
r4 equ $+1
    ld bc,0
    add hl,bc   ;r0 += r4
    ex de,hl    ;de - r0, hl - r1
r5 equ $+1
    ld bc,0
    add hl,bc    ;r1 += r5
ixhmem equ $+1
    ld a,0
    dec a
    jp nz,loc1
    jp loc2x
loc2:
    ld a,(ixhmem)   ;color
loc2x:
    and 7
    rlca
    or $80
    ld b,a

    pop de
    ld a,$c0
    xor e
    ld c,a    ;save BC gives an invisible speed gain, and needs more bytes
         ld hl,RGBASE2+SYSREG
         di
         ld (hl),DOSG1
    ld hl,RGBASE3+NCREG
    ld (hl),b
    ld a,$3f
    xor d
    ld b,a
rcolor equ $+1
    ld a,1
    ld (de),a
    ld (bc),a
    ;rlca
    ;ld (bc),a
    ;ld (de),a
         ld hl,RGBASE3+SYSREG
         ld (hl),ODOSA
         ei
    rlca
    ld (rcolor),a
    push de
    jp nc,loop2

    pop de
    ld a,e
    dec de
    push de
    and $3f
    jp nz,loop2

    ld de,128
    pop hl  ;scrtop
    add hl,de
    push hl

dy equ $+1
    ld hl,0
    ld a,(r5)
    sub l
    ld l,a
    ld (r5),a
    ld a,(r5+1)
    sbc a,h
    ld (r5+1),a
    or l
    jp nz,loop0

    ld hl,(KL+1)
    ld (INTRV),hl   ;stop timer
    ld hl,iter
    inc (hl)
wk: call waitk
    and 0dfh
    cp 'Q'
    jp nz,noq
exit:
    rst 0

noq:cp 'C'
    jp nz,noc

    ld a,(xi1)
    xor 2
    ld (xi1),a
    xor 2
    ld (xi2),a
    ld de,invon
    ld c,9
    call BDOS
xi1 equ $+2
    ld hl,$fe00
    ld bc,512
    ld a,' '
lo1 ld (hl),a
    inc hl
    dec c
    jp nz,lo1

    dec b
    jp nz,lo1

    ld de,invoff
    ld c,9
    call BDOS
xi2 equ $+2
    ld hl,$fc00
    ld bc,512
    ld a,' '
lo4 ld (hl),a
    inc hl
    dec c
    jp nz,lo4

    dec b
    jp nz,lo4
    jp wk
noc:cp 'T'
    jp nz,mandel

    ld de,$4000
    ld b,16
    ld hl,RGBASE2+SYSREG
    di
    ld (hl),DOSG1
    ld hl,RGBASE3+NCREG
    ld (hl),$80
lt2:ld a,$ff
    ld c,10
lt1:ld (de),a
    inc e
    dec c
    jp nz,lt1

    ld a,e
    add a,54
    ld e,a
    ld a,d
    adc a,c
    ld d,a
    dec b
    jp nz,lt2

    ld hl,RGBASE3+SYSREG
    ld (hl),ODOSA
    ei

    ld a,(iter)
    ld l,a
    ld h,0
    call PR000
    ld e," "
    ld c,2
    call BDOS
    ld hl,(tilo)
    ex de,hl
    ld hl,(tihi)
    ld bc,50
    call div32x16r
	PUSH HL
	EX DE,HL
	call PR000
	LD e,'.'
    ld c,2
    call BDOS
	POP hl
    add hl,hl  ;*2
	call PR00
    call waitk
    and 0dfh
    cp 'Q'
    jp z,exit

    call clscursor
    jp mandel

div0 macro
     local t1,t2
     ex de,hl
     add hl,hl
     ex de,hl
     ld a,l
     adc a,l
     ld l,a
     ld a,h
     adc a,h
     ld h,a
     jp c,t1

     LD    A,L
     ADD   A,C
     LD    A,H
     ADC   A,B
     jp nc,t2
t1
     ADD   HL,BC
     inc e
t2
endm

div32x16r proc
     local t,t0,t1,t2,t3
     call t
     ld bc,0
     ret
t
     DEC   BC
     LD    A, B
     CPL
     LD    B, A
     LD    A, C
     CPL
     LD    C, A
     call t0
t0
     call t1
t1
     call t2
t2
     call t3
t3
     div0
     RET
     endp

;PR0000  ld de,-1000
;	CALL PR0
PR000	ld de,-100
	CALL PR0
PR00	ld de,-10
	CALL PR0
	ld A,L
PRD	add a,$30
    push hl
    ld e,a
    ld c,2
    call BDOS
    pop hl
    ret

PR0	ld A,$FF
	ld B,H
	ld C,L
	inc A
	add HL,DE
	jp C,$-4

	ld H,B
	ld L,C
	jp PRD

KINTR
     push af
     push hl
tilo equ $+1
     ld hl,0
     inc hl
     ld (tilo),hl
     ld a,l
     or h
     jp nz,kq

tihi equ $+1
     ld hl,0
     inc hl
     ld (tihi),hl
kq   pop hl
     pop af
KL   jp 0

waitk:
    ld c,6  ;direct console i/o
    ld e,$ff
    call BDOS
    or a
    jp z,waitk
    ret

clscursor:
    ld e,31  ;cls
    ld c,2
    jp BDOS

curoff db 27,";$"
;curon  db 27,":$"
;curpos db 1,33,65,"$"
invon db 27,"6$"
invoff db 27,"7$"

mentry macro dx,dy,ni
     db -dx, dy
     dw dx*HSize/2-384   ;dx, dy, x0 = dx*HSize/2, niter
     db ni
endm

iter db 0
data mentry 9, 18, 7   ;1
     mentry 7, 15, 8   ;2
     mentry 6, 13, 9   ;3
     mentry 5, 11, 10  ;4
     mentry 4, 10, 11  ;5
     mentry 4,  8, 12  ;6
     mentry 4,  6, 13  ;7
     mentry 3,  5, 14  ;8
     mentry 3,  5, 15  ;9
     mentry 3,  5, 16  ;10
     mentry 3,  5, 25  ;11
     mentry 3,  5, 37  ;12
dataindex dw data

msg     db "*****************************************",13,10
        db "*    Superfast Mandelbrot generator     *",13,10
        db "* 8/16 colors, fullscreen (512x256), v7 *",13,10
        db "*****************************************",13,10
        db "This Corvette code was created by Litwr, 2022-24.",13,10
        db "It is based on code published for",13,10
        db "the ",226,"K0011 in 2021 by Stanislav Maslovski.",13,10
        db "The T-key gives us timings. Press C-key to get more colors.",13,10
        db "Use the Q-key to quit$"
sqrbase equ (msg + $16b0 + $ff) and $ff00
   end start

