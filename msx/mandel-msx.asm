;for pasmo assembler
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;The next code was made by litwr in 2021
;Thanks to reddie for some help with optimization
;
;128x256 Mandelbrot for the MSX2, 16 colors, interlaced (256x384 raster)

CHGET equ #009F
CHPUT equ #00A2
CHGMOD equ #005F
CHGCLR equ #0062
GRPPRT equ #008D
;WRTVDP equ #0047
;TOTEXT equ #00D2
;FILVRM equ #0056
;SNSMAT equ #0141
;KILBUF equ #0156

RG0SAV	equ #F3DF
RG1SAV	equ #F3E0
RG9SAV	equ #FFE8  ;reg #8!
RG10SAV	equ #FFE9  ;reg #9!
BDRCLR	equ #F3EB
GRPACX equ #FCB7
GRPACY equ #FCB9

NOCALC equ 0
SA equ $8100  ;start address

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

         org SA-7
db $fe,low(SA),high(SA),low(ec),high(ec),low(SA),high(SA)
 
start    ld hl,msg
l2       ld a,(hl)
         or a
         jr z,l1

         call CHPUT
         inc hl
         jr l2

l1       call CHGET
         ld a,1
         ld c,$90
         call wrreg
         ld a,$77
         out ($9a),a
         ld a,0
         out ($9a),a   ;sets color 1

         ld a,5
         call CHGMOD
         xor a
         ld (BDRCLR),a
         call CHGCLR
if 0
         ld a,(RG0SAV)
         and $f1
         or 6
         ld c,$80
         call wrreg
         ld a,(RG1SAV)
         and $e7
         or $20
         ld c,$81
         call wrreg    ;graphic 4, vertical retrace interrupts
endif
         ;ld a,(RG9SAV)
         ;or $22     ;disable sprites and make color 0 normal
         ;ld c,$88
         ;ld (RG9SAV),a
         ;call wrreg ;any access to this register break interlaced mode on openMSX 0.15

         ld a,(RG10SAV)
         and $73
         or $c    ;192 lines, interlaced
         ld c,$89
         call wrreg
         ld a,$3f    ;$1f - page 0, $3f - page 1
         ld c,$82
         call wrreg

         ld hl,timer     ;prepare the timer handler
         ld ($fd9b),hl

         xor a       ;clean screen
         ld c,$11
         ld hl,0
         call wvmem
         ld hl,16384
l4:      ld a,c
         out ($98),a
         dec hl
         ld a,l
         or h
         jr nz,l4

         ld a,1
         ;ld c,$11
         ld hl,0
         call wvmem
         ld hl,8192
l5:      ld a,c
         out ($98),a
         dec hl
         ld a,l
         or h
         jr nz,l5

         ld a,2
         ;ld c,$11
         ld hl,0
         call wvmem
         ld hl,16384
l3:      ld a,c
         out ($98),a
         dec hl
         ld a,l
         or h
         jr nz,l3

         ld a,3
         ;ld c,$11
         ld hl,0
         call wvmem
         ld hl,8192
l7:      ld a,c
         out ($98),a
         dec hl
         ld a,l
         or h
         jr nz,l7

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
    jr c,mandel0

    inc de
    jr sqrloop

mandel0:
    pop hl
mandel:
    ld hl,ticks
    xor a
    ld (hl),a
    inc hl
    ld (hl),a
    inc hl
    ld (hl),a
    ld a,$c3     ;opcode for CALL
    ld ($fd9a),a   ;start timer

    ld ixl,1   ;dot even/odd
    ld iyl,0   ;line even/odd
    ld hl,0  ;scrbase
    push hl

    ld hl,(dy)
    ld a,h
    ld h,l
    srl h
    rra
    ld l,a       ;dy*128
    ld (r5),hl
loop0:
    ld hl,$40  ;scridx
    push hl
if NOCALC=0
x0 equ $+1
    ld hl,ix0
    ld (r4),hl
endif
loop2:
if NOCALC=0
    ld hl,(r4)
    ld de,(dx)
    add hl,de
    ld (r4),hl
    ld d,h
    ld e,l      ;mov	r4, r0
endif
niter equ $+2
    ld ixh,initer
if NOCALC=0
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
    jr nz,loc2    ;jp?

    push hl
    sbc hl,bc   ;x^2  ;set C=0
    sbc hl,bc   ;x^2-y^2
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
    add hl,bc    ;sets C=0
    pop bc   ;r0
    sbc hl,bc    ;2xy+y0
    dec ixh     
    jp nz,loc1   ;sob r2,1$
loc2:
    ld a,ixh   ;color
endif
    and 15
    dec ixl
    jp nz,lx1

    ld (tcolor),a
    jp loop2
lx1
    ld ixl,1
    rlca
    rlca
    rlca
    rlca
    ld b,a
tcolor equ $+1
    ld a,0
    or b
    pop hl
    dec l
    push hl
    ld b,l
    ld de,buf
    add hl,de         
    ld (hl),a
    ld a,b
    or a
    jp nz,loop2

    ld a,iyl
    xor 1
    ld iyl,a
    pop hl  ;scridx
    pop hl  ;scrbas
    push hl
    jr z,oddli

    xor a
    call wvmem
    ld bc,$4098
    ld hl,buf
    otir
    pop hl
    push hl
    ld a,h
    xor $3f
    ld h,a
    ld a,l
    xor $80
    ld l,a
    xor a
    call wvmem
    ld bc,$4098
    ld hl,buf
    otir
    jp endli
oddli
    ld a,2
    call wvmem
    ld bc,$4098
    ld hl,buf
    otir
    pop hl
    push hl
    ld a,h
    xor $3f
    ld h,a
    ld a,l
    xor $80
    ld l,a
    ld a,2
    call wvmem
    ld bc,$4098
    ld hl,buf
    otir
    pop hl
    ld de,128
    add hl,de
    push hl
endli
    ld de,(dy)
    ld hl,(r5)
    or a   ;sets C=0
    sbc hl,de
    ld (r5),hl
    jp nz,loop0

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

lx2:pop hl
endif
    ld a,$c9   ;opcode for RET
    ld ($fd9a),a   ;stop timer
    call CHGET
    and 0dfh
    cp 'Q'
    jr nz,noq

    xor a
    jp CHGMOD

noq:cp 'T'
    jp nz,mandel

         ld a,1
         ld (GRPACX),a
         ld (GRPACY),a
    ld a,(niter)
    sub 7
    ld l,a
    ld h,0
    call PR000
    ld a," "
    call GRPPRT
    ld hl,(ticks)
    ex de,hl
    ld a,(ticks+2)
    ld l,a
    ld h,0
    ld bc,60
    ld a,(RG10SAV)
    and 2
    jr z,ntsc

    ld c,50
    call div32x16r
	PUSH HL
	EX DE,HL
	call PR000
	LD a,'.'
    call GRPPRT
	POP hl
    add hl,hl  ;*2
    jr lminus
ntsc
    call div32x16r
	PUSH HL
	EX DE,HL
	call PR000
	LD a,'.'
    call GRPPRT
	POP hl
    push hl
    add hl,hl
    add hl,hl  ;*2
    pop de
    add hl,de
    ld de,0
    ex de,hl
    ld bc,3
    call div32x16r
    ex de,hl
    dec e
    dec e
    jp m,lminus

    inc hl
lminus
	call PR00
    call CHGET
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

dx:  	dw idx
dy:	    dw idy
mx:     dw imx
  if (dx and $ff00) != ((mx+2) and $ff00)
ERROR ERROR2
  endif

PR0000  ld de,-1000
	CALL PR0
PR000	ld de,-100
	CALL PR0
PR00	ld de,-10
	CALL PR0
	ld A,L
PRD	add a,$30
    jp GRPPRT

PR0	ld A,$FF
	ld B,H
	ld C,L
	inc A
	add HL,DE
	jp C,$-4

	ld H,B
	ld L,C
	jp PRD

wvmem:   ;a - bank, hl - addr
    di
    out (#99),a
    ld a,14 + 128
    out (#99),a
    ld a,l
    out (#99),a
    ld a,h
    or 64
    ei
    out (#99),a
    ret

wrreg   ;a - value, c - reg+128
    di
    out (#99),a
    ld a,c
    ei
    out (#99),a
    ret

timer:
    push af
    push hl
    ld hl,ticks
    ld a,(hl)
    add a,1
    ld (hl),a
    inc hl

    ld a,(hl)
    adc a,0
    ld (hl),a
    inc hl

    ld a,(hl)
    adc a,0
    ld (hl),a
    pop hl
    pop af
    ret

ticks db 0,0,0
buf ds 64   ;optimize?

msg     db "****************************",13,10
        db "*   Superfast Mandelbrot   *",13,10
        db "*        generator         *",13,10
        db "*     interlaced, v1       *",13,10
        db "****************************",13,10
        db "The original version was",13,10
        db "published for the BK0011 in",13,10
        db "2021 by Stanislav Maslovski.",13,10
        db "This MSX2 port was created",13,10
        db "by Litwr, 2022.",13,10
        db "The T-key gives us timings.",13,10
        db "Use the Q-key to quit",0
sqrbase equ (msg + $16b0 + $ff) and $ff00
ec:
   end start

