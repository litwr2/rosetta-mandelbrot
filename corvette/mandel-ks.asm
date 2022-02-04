;for pasmo assembler
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;The next code was made by litwr in 2021
;Thanks to reddie for some help with optimization
;
;512x256 (fullscreen) Mandelbrot for the Corvette,
;8 colors (color write mode), simulates 16 colors using textures

BDOS equ 5

RGBASE2 EQU     0FA00H  ;ROMB1, ODOSA, NDOS, BASIC
RGBASE3 EQU     0FF00H  ;DOSA, DOSG1

SYSREG  EQU     7FH 
DOSG1   EQU     3CH
ODOSA   EQU     1CH

NCREG   EQU     0BFH  ;color reg

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
    ld de,curoff
    ld c,9
    call BDOS
    call clscursor

    ld hl,(0xf7f1)
    ld (KL+1),hl    ;prepare the timer handler

    ld hl,sqrbase
    push hl
    ld bc,0
    ld de,0
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
    ld (dx+1),a
    inc hl
    ld a,(hl)
    ld (dy),a
    inc hl
    ld a,(hl)
    ld (dy+1),a
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
    ld a,low(data+7*12)
    cp l
    jp nz,le1

    ld a,high(data+7*12)
    cp h
    jp nz,le1

    ld hl,data
le1 ld (dataindex),hl
    ld hl,0
    ld (tilo),hl
    ld (tihi),hl
    ld hl,KINTR
    ld (0xf7f1),hl   ;start timer
    ld hl,$403f  ;scrtop
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
x0 equ $+1
    ld hl,0
    ld (r4),hl
loop2:
dx equ $+1
    ld hl,0
    ex de,hl
    ld hl,(r4)
    add hl,de
    ld (r4),hl
    ld d,h
    ld e,l      ;mov	r4, r0
niter equ $+1
    ld a,0
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
r5 equ $+1
    ld bc,0
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
    and 15
patx equ $+1
    ld hl,pat0
    add a,l
    ld l,a
    ld a,(hl)
    ld (tcolor1),a
    ld a,l
    add a,16
    ld l,a
    ld b,(hl)
    ;ld (tcolor2),a

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
    rlca
    ld (bc),a
tcolor1 equ $+1
    ld (hl),0
    rrca
    ld (bc),a
    rlca
    ld (de),a
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
    ld c,low(pat0)
    ld a,(patx)
    cp c    ;sets C=0
    jp nz,lx8

    ld c,low(pat1)
lx8:
    ld a,c
    ld (patx),a

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

lx2:ld hl,(KL+1)
    ld (0xf7f1),hl   ;stop timer
    ld hl,iter
    inc (hl)
    call waitk
    and 0dfh
    cp 'Q'
    jp nz,noq
    rst 0

noq:cp 'T'
    jp nz,mandel

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

             ;0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15
pat0:	db 0x80,0x82,0x88,0x84,0x84,0x88,0x8a,0x8e,0x8c,0x8c,0x82,0x86,0x8e,0x8a,0x86,0x8c
        db 0x80,0x80,0x88,0x80,0x84,0x80,0x80,0x8e,0x80,0x84,0x82,0x86,0x80,0x8a,0x80,0x8c
             ;B,  bB,   r,  gB,   g,  rB,  mB,   w,  yB,  yg,   b,   c,  wB,   m,  cB,   y   
pat1:	db 0x80,0x80,0x88,0x80,0x84,0x80,0x80,0x8e,0x80,0x84,0x82,0x86,0x80,0x8a,0x80,0x8c
        db 0x80,0x82,0x88,0x84,0x84,0x88,0x8a,0x8e,0x8c,0x8c,0x82,0x86,0x8e,0x8a,0x86,0x8c
                                                              
  if (pat0 and $ff00) != ((pat0+64) and $ff00)
ERROR ERROR1
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
    call BDOS
    ld de,curpos
    ld c,9
    jp BDOS

curoff db 27,";$"
;curon  db 27,":$"
;curpos db 27,"Y",33,65,"$"
curpos db 1,38,57,"$"

iter db 0
data dw -18, 18, 2232   ;dx, dy, x0, niter - dx, dy might be bytes
     db 7   ;1
     dw -15, 15, 1841
     db 8   ;2
     dw -13, 13, 1714
     db 9   ;3
     dw -11, 11, 1430
     db 10  ;4
     dw  -9, 10, 1200
     db 11  ;5
     dw  -9,  8, 1120
     db 12  ;6
     dw  -8,  6, 1000
     db 13  ;7
     dw  -7,  5,  700
     db 14  ;8
     dw  -6,  5,  500
     db 15  ;9
     dw  -5,  5,  320
     db 16  ;10
     dw  -5,  5,  300
     db 25  ;11
     dw  -5,  5,  270
     db 37  ;12
dataindex dw data

msg     db "**********************************",13,10
        db "* Superfast Mandelbrot generator *",13,10
        db "*       8 colors + textures      *",13,10
        db "*   fullscreen (512x256) , v1    *",13,10
        db "**********************************",13,10
        db "This Corvette code was created by Litwr, 2022.",13,10
        db "It is based on code published for",13,10
        db "the ",226,"K0011 in 2021 by Stanislav",13,10
        db "Maslovski.",13,10
        db "The T-key gives us timings.",13,10
        db "Use the Q-key to quit$"
sqrbase equ (msg + $16b0 + $ff) and $ff00
   end start

