;for pasmo assembler
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;Thanks to reddie for some help with optimization
;
;512x256 Mandelbrot for the Commodore 128 (VDC) under CP/M, 2 colors are used to form 4x1 bricks to simulate 8 colors

APORT equ $D600
;DPORT equ $D601
CIA1TOD equ $DC08

BDOS equ 5

NOCALC equ 0

initer	equ	7
idx	equ	-36       ;-.0703125
idy	equ	18        ;.03515625
ix0	equ	-62*idx
imx	equ	10*idx		; x move
sf4	equ	436/4		; sf/4

sqrtab macro
    res 0,l
    ld a,h
    add a,high(sqrbase)
    ld h,a
endm

mgetr macro  ;d - port, e - data
     local l1
     ld bc,APORT
     out (c),d
l1:  in a,(c)
     or a
     jp p,l1

     inc c
     in e,(c)
  endm

msetr macro  ;d - port, e - data
     local l1
     out (c),d
l1:  in a,(c)
     or a
     jp p,l1

     inc c
     out (c),e
  endm

         org $100
start    ld de,msg
         ld c,9
         call BDOS
         call wait_char
         and 0dfh
         ld (benchmark),a

         ld de,$1987 ;VDC 512x256 bw graphics
         call setr

         ld de,$1af0
         call setr

         ld de,$c00
         call setr

         ld de,$d00
         call setr

         ld de,$140
         call setr

         ld de,$25e
         call setr

         ld de,$427
         call setr

         ld de,$620
         call setr

         ld de,$724
         call setr

         ld de,$c00
         call setr

         ld de,$d00
         call setr

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
    jr c,mandel0

    inc de
    jr sqrloop

mandel0:
    pop hl
mandel:
    ld a,16
    ld (bcount),a

    call fast
    ld bc,CIA1TOD+3
    ld hl,ticks+3
    in a,(c)
    and $7f
    ld (hl),a
    dec hl
    dec bc
    in a,(c)
    ld (hl),a
    dec hl
    dec bc
    in a,(c)
    ld (hl),a
    dec hl
    dec bc
    in a,(c)
    ld (hl),a

mandel1:
    ld hl,$3fc0 ;bottom
    push hl
    ld hl,0
    push hl     ;top
    ld hl,(dy)
    ld a,h
    ld h,l
    srl h
    rra
    ld l,a       ;dy*128
    ld (r5),hl
    ld h,high(pat1)
    ld b,2
    exx
loop0:
if NOCALC=0
x0 equ $+1
    ld hl,ix0
    ld (r4),hl
endif
    exx
    ld de,lineb1+63  ;x
    exx
loop2:
niter equ $+2
    ld ixh,initer   ;ixh = r2
if NOCALC=0
    ld hl,(r4)
    ld de,(dx)
    add hl,de
    ld (r4),hl  ;r4 += dx
    ex de,hl    ;de = r0
    ld hl,(r5)  ;hl = r1
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
    ld l,a       ;r0 = sqr(r0)
    add hl,bc    ;r0 += r3
    ld a,h
    and $f8      ;sets C=0
    jr nz,loc2    ;jp?

    ex de,hl    ;de - r0, hl - r1
    sqrtab
    ld a,(hl)
    inc l
    ld h,(hl)
    ld l,a     ;r1 = sqr(r1)

    sbc hl,de  ;r1 -= r0
    ex de,hl   ;de - r1, hl - r0

    xor a
    sbc hl,bc  ;r0 -= r3
    sbc hl,bc  ;r0 -= r3
r4 equ $+1
    ld bc,0
    add hl,bc   ;r0 += r4
    ex de,hl    ;de - r0, hl - r1
endif
r5 equ $+1
    ld bc,0
if NOCALC=0
    add hl,bc   ;r1 += r5
    dec ixh
    jp nz,loc1
loc2:
    ld a,ixh   ;color
endif
    and 7
patx equ $+1
    add a,low(pat1)
    exx
    ld l,a
    ld c,(hl)
    xor 8
    ld l,a
    ld a,(hl)
    dec b
    jr z,loc8

    ld iyl,a  ;bottom
    ld iyh,c  ;top
    exx
    jp loop2
loc8
    rla
    rla
    rla
    rla
    or iyl
    ld (de),a
    ld a,c
    rla
    rla
    rla
    rla
    or iyh
    ld hl,64
    add hl,de  ;lineb2
    ld (hl),a
    ld b,2
    dec e
    exx
    jp p,loop2

    pop hl
    ld d,18
    ld e,h
    ld bc,APORT
    msetr
    inc d
    ld e,l
    dec c
    msetr
    ld (m3+1),hl

    ld hl,lineb1
    ld d,31
l7  ld e,(hl)
    dec c
    msetr
    inc l
    ld a,l
    cp low(lineb1+64)
    jp nz,l7

    pop hl
    ld d,18
    ld e,h
    dec c
    msetr
    inc d
    ld e,l
    dec c
    msetr
    ld (m2+1),hl

    ld hl,lineb2
    ld d,31   
l8  ld e,(hl)
    dec c
    msetr
    inc l
    ld a,l
    cp low(lineb2+64)
    jp nz,l8

m2  ld hl,0
    ld de,-64
    add hl,de
    push hl
m3  ld hl,0
    ld de,64
    add hl,de
    push hl

    ld a,(patx)
    xor 8     ;sets C=0
    ld (patx),a

    ld de,(dy)
    ld hl,(r5)
    ;or a   ;sets C=0
    sbc hl,de
    ld (r5),hl
    jp nz,loop0

    pop hl
    pop hl
if NOCALC=0
    ld hl,(x0)
    ld de,(mx)
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
    or a ;sets C=0
    sbc hl,bc  ;C=0
dx2p equ $+1
    ld (dx),hl
    jp lx5

lx2:
endif
    ld a,(benchmark)
    cp 'B'
    jr nz,loc3

    ld hl,bcount
    dec (hl)
    jp nz,mandel1
loc3:
    ld bc,CIA1TOD+3
    ld hl,msg+3
    in a,(c)
    and $7f
    ld (hl),a
    dec hl
    dec bc
    in a,(c)
    ld (hl),a
    dec hl
    dec bc
    in a,(c)
    ld (hl),a
    dec bc
    in a,(c)

    ld bc,msg
    ld hl,ticks
    sub (hl)
    daa
    push af
    and $f
    ld (bc),a
    pop af

    inc hl
    inc bc
    ld a,(bc)
    sbc a,(hl)
    daa
    push af
    jr nc,$+4
    sub $40
    ld (bc),a
    pop af

    inc hl
    inc bc
    ld a,(bc)
    sbc a,(hl)
    daa
    push af
    jr nc,$+4
    sub $40
    ld (bc),a
    pop af

    inc hl
    inc bc
    ld a,(bc)
    sbc a,(hl)
    daa
    jr nc,$+5
    add a,$12
    daa
    ;ld (bc),a
    ld h,0
    ld l,a
    push bc
    ld bc,4500       ;36000/8
    call mul16
    ex de,hl
    pop bc
    dec bc

    push hl
    ld a,(bc)
    push bc
    ld bc,375        ;6000/16
    and $f0
    rrca
    rrca
    rrca
    ld h,0
    ld l,a
    call mul16
    pop bc
    pop hl
    add hl,de

    push hl
    ld a,(bc)
    push bc
    ld bc,75       ;600/8
    and $f
    ld h,0
    ld l,a
    call mul16
    pop bc
    pop hl
    add hl,de
    dec bc

    push hl
    ld a,(bc)
    push bc
    ld bc,25        ;100/4
    and $f0
    rrca
    rrca
    ld h,0
    ld l,a
    call mul16
    pop bc

    push de
    ld a,(bc)
    push bc
    ld bc,10
    and $f
    ld h,0
    ld l,a
    call mul16
    pop bc
    pop hl
    add hl,de
    dec bc

    ld a,(bc)
    ld d,0
    ld e,a
    add hl,de
    ld de,0
    ld (ticks),de
    ld (ticks+2),hl
    ld b,h
    ld c,l
    pop hl
    add hl,hl
    rl e
    add hl,hl
    rl e
    add hl,hl
    rl e
    add hl,bc
    jr nc,$+3
    inc de
    ld (ticks),hl
    ld (ticks+2),de
    call slow

    ld a,(benchmark)
    cp 'B'
    jr z,loc4

    call wait_char
    and 0dfh
    cp 'Q'
    jr nz,noq
exit:
    ld c,0
    jp BDOS

noq:cp 'T'
    jp nz,mandel
loc4:
    ld e,$d
    ld c,2
    call BDOS
    ld e,$a
    ld c,2
    call BDOS
    ld a,(niter)
    sub 7
    ld l,a
    ld h,0
    call PR000
    ld e," "
    ld c,2
    call BDOS
    ld hl,(ticks)
    ex de,hl
    ld a,(ticks+2)
    ld l,a
    ld h,0
    ld bc,10
    call div32x16r
	PUSH HL
	EX DE,HL
	call PR000
	LD e,'.'
    ld c,2
    call BDOS
	POP hl
	call PR0E

    call wait_char
    and 0dfh
    cp 'Q'
    jp z,exit
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

div32x16r proc   ;HL:DE/BC -> HL - rem, DE - quo
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
PR0E   	ld A,L
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

dx:  	dw idx
dy:	    dw idy
mx:     dw imx
  if (dx and $ff00) != ((mx+2) and $ff00)
.ERROR wrong alignment for dx,dy,mx
  endif

wait_char
        ld c,6   ;direct console i/o
        ld e,0ffh
        call BDOS
        or a
        jr z,wait_char
        ret

setr:
     ld bc,APORT
     msetr
     ret


slow: ;ld bc,$d030
      ;xor a
      ;out (c),a
      ld bc,$d011
      in a,(c)
      and $7f
      or $10
      out (c),a
      ei
      ret

ticks db 0,0,0,0
benchmark db 0
bcount db 0

        org ($ + 255) & $ff00
lineb1 ds 64
lineb2 ds 64

pat1: db	0,$e,$d,$c,$a,$5,$1,$f  ;pat1 & pat2 must be on the same page
pat2: db	0,$b,$6,$3,$1,$a,$e,$f

fast: di
      ld bc,$d011
      in a,(c)
      and #$6f
      out (c),a
      ;ld bc,$d030
      ;ld a,1
      ;out (c),1
      ret
 
;getr:
;     mgetr
;     ret

      include "mul16.s"

msg     db "****************************",13,10
        db "*   Superfast Mandelbrot   *",13,10
        db "* generator, C128 VDC 16KB *",13,10
        db "*             v5           *",13,10
        db "*RUN IT ON VIC-II DISPLAY!!*",13,10
        db "****************************",13,10
        db "The original version was",13,10
        db "published for the BK0011 in",13,10
        db "2021 by Stanislav Maslovski.",13,10
        db "This C128-Z80 port was created",13,10
        db "by Litwr, 2023-24.",13,10
        db "The T-key gives us timings.",13,10
        db "Use the Q-key to quit",13,10
        db "Press B to enter benchmark mode$"

sqrbase equ (msg + $16b0 + $ff) and $ff00
   end start

