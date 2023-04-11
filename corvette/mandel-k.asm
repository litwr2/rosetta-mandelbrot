;for pasmo assembler
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;Thanks to reddie for some help with optimization
;
;128x256 Mandelbrot for the Corvette, 4 colors (planar write mode), simulates 8 colors using textures

BDOS equ 5

RGBASE2 EQU     0FA00H  ;ROMB1, ODOSA, NDOS, BASIC
RGBASE3 EQU     0FF00H  ;DOSA, DOSG1

SYSREG  EQU     7FH
DOSG1   EQU     3CH
ODOSA   EQU     1CH

NCREG   EQU     0BFH  ;color reg

WSEL0   EQU     01111100B       ;write to plane 0
WSEL1   EQU     01111010B       ;1
WSEL2   EQU     01110110B       ;2

WBIT    EQU     00000001B       ;write 1

NOCALC equ 0

initer	equ	7
idx	equ	-36       ;-.0703125
idy	equ	18        ;.03515625
ix0	equ	-62*idx
imx	equ	10*idx		; x move
sf4	equ	436/4		; sf/4

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
    and 0dfh
    ld (benchmark),a
    ld de,curoff
    ld c,9
    call BDOS
    call clscursor

    ld hl,(0xf7f1)
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
    ld a,16
    ld (bcount),a
    ld hl,0
    ld (tilo),hl
    ld (tihi),hl
    ld hl,KINTR
    ld (0xf7f1),hl   ;start timer
mandel1:
    ld hl,$401f  ;scrtop
    push hl
    ld hl,(dy)
    xor a   ;sets C=0
    ld a,l
    rra    ;C=0
    ld l,a
    ld a,h
    rra
    ld h,l
    ld l,a       ;dy*128
    ld (r5),hl
loop0:
if NOCALC=0
x0 equ $+1
    ld hl,ix0
    ld (r4),hl
endif
loop2:
if NOCALC=0
    ld hl,(dx)
    ex de,hl
    ld hl,(r4)
    add hl,de
    ld (r4),hl
    ex de,hl    ;mov	r4, r0
endif
niter equ $+1
    ld a,initer
if NOCALC=0
    ld (ixhmem),a
    ld hl,(r5)  ;mov	r5, r1	
loc1:
    push hl
    sqrtab
    ld c,(hl)
    inc l
    ld b,(hl)   ;mov	sqr(r1), r3
    pop hl
    add hl,de   ;add	r0, r1
    ex de,hl    ;de - r1, hl - r0, bc - r3
    sqrtab
    ld a,(hl)
    inc l
    ld h,(hl)
    ld l,a       ;mov	sqr(r0), r0
    add hl,bc    ;add	r3, r0
    ld a,h
    and $f8
    jp nz,loc2

    push hl
    ld a,l
    sub c
    ld l,a
    ld a,h
    sbc a,b
    ld h,a      ;x^2  ;set C=0
    ld a,l
    sub c
    ld l,a
    ld a,h
    sbc a,b
    ld h,a      ;x^2-y^2
r4 equ $+1
    ld bc,0
    add hl,bc   ;x^2-y^2+x0
    ex de,hl    ;de - r0, hl - r1
    sqrtab
    ld a,(hl)
    inc l
    ld h,(hl)
    ld l,a       ;(x+y)^2
endif
r5 equ $+1
    ld bc,0
if NOCALC=0
    add hl,bc
    pop bc   ;r0
    ld a,l
    sub c
    ld l,a
    ld a,h
    sbc a,b
    ld h,a    ;2xy+y0
ixhmem equ $+1
    ld a,0
    dec a
    ld (ixhmem),a     
    jp nz,loc1   ;sob r2,1$
loc2:
    ld a,(ixhmem)   ;color
endif
    and 7
patx equ $+1
    ld hl,pat0
    add a,l
    ld l,a
    ld c,(hl)
tcolor1 equ $+1
    ld a,0
    rrca
    rrca
    or c
    ld (tcolor1),a
    ld a,l
    add a,8
    ld l,a
    ld c,(hl)
tcolor2 equ $+1
    ld a,0
    rrca
    rrca
    or c
    ld (tcolor2),a
    ld a,l
    add a,8
    ld l,a
    ld c,(hl)
bcolor1 equ $+1
    ld a,0
    rrca
    rrca
    or c
    ld (bcolor1),a
    ld a,l
    add a,8
    ld l,a
    ld c,(hl)
bcolor2 equ $+1
    ld a,0
    rrca
    rra
    jp c,.l8

    or c
    ld (bcolor2),a
    jp loop2

.l8:
    or c
    ld (bcolor2),a
    pop de
    ld a,$3f
    xor d
    ld b,a
    ld a,$c0
    xor e
    ld c,a    ;save BC gives an invisible speed gain, and needs more bytes
         ld hl,RGBASE2+SYSREG
         di
         ld (hl),DOSG1
    ld hl,RGBASE3+NCREG
    ld (hl),WSEL1&WSEL2
    ld a,$ff
    ld (de),a
    ld (bc),a
    ld (hl),WSEL1+WBIT
    ld a,(tcolor1)
    ld (de),a
    ld a,(bcolor1)
    ld (bc),a
    ld (hl),WSEL2+WBIT
    ld a,(tcolor2)
    ld (de),a
    ld a,(bcolor2)
    ld (bc),a
         ld hl,RGBASE3+SYSREG
         ld (hl),ODOSA
         ei
    ld a,$80
    ld (bcolor2),a
    xor a
    ld (bcolor1),a
    ld (tcolor2),a
    ld (tcolor1),a
    ld a,e
    dec de
    push de
    and $1f
    jp nz,loop2

    ld de,96
    pop hl  ;scrtop
    add hl,de
    push hl
    ld c,low(pat0)
    ld a,(patx)
    cp c    ;sets C=0
    jp nz,lx8

    ld c,low(pat1)
lx8:
    ld a,c
    ld (patx),a

    ld hl,(dy)
    ld a,(r5)
    sub l
    ld l,a
    ld (r5),a
    ld a,(r5+1)
    sbc a,h
    ld (r5+1),a
    or l
    jp nz,loop0
if NOCALC=0
    ld hl,(mx)
    ex de,hl
    ld hl,(x0)
    add hl,de
    ld (x0),hl   ;x0 += mx
    ld hl,niter
    inc (hl)     ;iter++
    ld hl,dx
    push hl
lx5:
    pop hl
    ld a,l
    cp low(mx)+2
    jp z,lx2

    ld (dx1p),a
    ld (dx2p),a
    inc l
    inc l
    push hl
    ld de,-sf4
dx1p equ $+1
    ld hl,(dx)
    push hl
    add hl,de
    sqrtab
    ld c,(hl)
    inc l
    ld b,(hl)
    ld de,sf4
    pop hl
    add hl,de
    sqrtab
    ld a,(hl)
    inc l
    ld h,(hl)
    ld l,a
    ld a,l
    sub c
    ld l,a
    ld a,h
    sbc a,b
    ld h,a
dx2p equ $+1
    ld (dx),hl
    jp lx5

lx2:pop hl
endif
    ld a,(benchmark)
    cp 'B'
    jp nz,loc3

    ld hl,bcount
    dec (hl)
    jp nz,mandel1
loc3:
    ld hl,(KL+1)
    ld (0xf7f1),hl   ;stop timer
    cp 'B'
    jp z,loc4

    call waitk
    and 0dfh
    cp 'Q'
    jp nz,noq
exit:
    rst 0

noq:cp 'T'
    jp nz,mandel
loc4:
    ld a,(niter)
    sub 7
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

dx:  	dw idx
dy:	    dw idy
mx:     dw imx
  if (dx and $ff00) != ((mx+2) and $ff00)
ERROR ERROR2
  endif

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

PR0000  ld de,-1000
	CALL PR0
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

benchmark db 0
bcount db 0

          ;0,    1,    2,    3,    4,    5,    6,    7
pat0:	db 0, 0x80, 0x00, 0x80, 0xC0, 0x40, 0x00, 0xC0
        db 0, 0x00, 0x80, 0x80, 0x00, 0xC0, 0xC0, 0xC0
          ;B    gB,   rB,   yB,    g,   ry,    r,    y
pat1:	db 0, 0x40, 0x00, 0x40, 0xC0, 0x40, 0x00, 0xC0
        db 0, 0x00, 0x40, 0x40, 0x00, 0x00, 0xC0, 0xC0
          ;B    Bg,   Br,   By,    g,   Bg,    r,    y
pat0c:	db 0, 0x80, 0x00, 0x80, 0xC0, 0x40, 0x00, 0xC0
        db 0, 0x00, 0x80, 0x80, 0x00, 0xC0, 0xC0, 0xC0

  if (pat0 and $ff00) != ((pat0+47) and $ff00)
ERROR ERROR
  endif

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
   if NOCALC=1
   ds 2
   endif

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

msg     db "**********************************",13,10
        db "* Superfast Mandelbrot generator *",13,10
        db "*     4 colors + textures, v5    *",13,10
        db "**********************************",13,10
        db "The original version was published for",13,10
        db "the ",226,"K0011 in 2021 by Stanislav",13,10
        db "Maslovski.",13,10
        db "This Corvette port was created by",13,10
        db "Litwr, 2022-23.",13,10
        db "The T-key gives us timings.",13,10
        db "Use the Q-key to quit.",13,10
        db "Press B to enter benchmark mode$"

sqrbase equ (msg + $16b0 + $ff) and $ff00
   end start

