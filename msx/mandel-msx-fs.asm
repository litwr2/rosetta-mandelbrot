;for pasmo assembler
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;Thanks to reddie for some help with optimization
;
;Fullscreen (512x212) Mandelbrot for the MSX2, 16 colors

CHGET equ #009F
CHPUT equ #00A2
CHGMOD equ #005F
CHGCLR equ #0062
GRPPRT equ #008D
;WRTVDP equ #0047
;TOTEXT equ #00D2

RG0SAV	equ #F3DF
RG1SAV	equ #F3E0
RG2SAV	equ #F3E0
RG9SAV	equ #FFE8  ;reg #8!
RG10SAV	equ #FFE9  ;reg #9!
BDRCLR	equ #F3EB
GRPACX equ #FCB7
GRPACY equ #FCB9

SA equ $8500  ;start address
VDP equ 1  ;faster and lesser

HSize equ 512

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

         ld a,7
         call CHGMOD
         xor a
         ld (BDRCLR),a
         call CHGCLR
if 0
         ld a,(RG0SAV)
         and $f1
         or 6
         ld c,$80
         ld (RG0SAV),a
         call wrreg
         ld a,(RG1SAV)
         and $e7
         or $20
         ld c,$81
         ld (RG1SAV),a
         call wrreg    ;graphic 4, vertical retrace interrupts
endif
         ;ld a,(RG9SAV)
         ;or $22     ;disable sprites and make color 0 normal
         ;ld c,$88
         ;ld (RG9SAV),a
         ;call wrreg ;any access to this register break interlaced mode on openMSX 0.15

         ;ld a,(RG10SAV)
         ;and $73
         ;or $8c    ;212 lines, interlaced
         ;ld c,$89
         ;ld (RG10SAV),a
         ;call wrreg
         ;ld a,$1f    ;$1f - page 0, $3f - page 1
         ;ld c,$82
         ;ld (RG2SAV),a
         ;call wrreg

         ld hl,timer     ;prepare the timer handler
         ld ($fd9b),hl

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
    ld a,(dataindex)
    ld l,a
    ld h,0
    push hl
    add hl,hl
    add hl,hl
    pop de
    add hl,de
    ld de,data
    add hl,de
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
    add a,2
    ld (hl),a

    ld hl,ticks
    xor a
    ld (hl),a
    inc hl
    ld (hl),a
    inc hl
    ld (hl),a
    ld a,$c3     ;opcode for CALL
    ld ($fd9a),a   ;start timer

    ld hl,0  ;scrbase
    push hl
dy equ $+1
    ld hl,0
    add hl,hl
    push hl
    add hl,hl
    add hl,hl
    push hl
    add hl,hl
    add hl,hl
    push hl
    add hl,hl
    pop de
    add hl,de
    pop de
    add hl,de
    pop de
    add hl,de      ;dy*106
    ld (r5),hl
    exx
x0 equ $+1
    ld hl,0
    ld b,high(buf)
    ld e,1   ;dot even/odd
    exx
loop0:
    exx
    ld c,0  ;scridx
    ld (r4),hl
    exx
loop2
niter equ $+2
    ld ixh,0    ;ixh = r2
    ld hl,(r4)
dx equ $+1
    ld de,$ff00
    add hl,de
    ld (r4),hl  ;r4 += dx
    ex de,hl    ;de = r0
    ld hl,(r5)  ;mov	r5, r1	
loc1:
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
r5 equ $+1
    ld bc,0
    add hl,bc   ;r1 += r5
    dec ixh
    jp nz,loc1
loc2:
    ld a,ixh   ;color
    and 15
    exx
    dec e
    jp nz,lx1

    ld d,a
    exx
    jp loop2
lx1
    ld e,1
    rlca
    rlca
    rlca
    rlca
    or d
    dec c
    ld (bc),a
    ld a,c
    exx
    or a
    jp nz,loop2

    pop hl  ;scrbas
    push hl
    ld c,$98
    xor a
    call wvmem
    ld hl,buf
    ;otir    ;unroll?
rept $100
    outi
endm
    pop hl
    push hl
    ld a,$d3
    sub h
if VDP=0
    ld h,a
    xor a
    call wvmem
    ld hl,buf
    ;ld b,$40
    ;otir     ;unroll?
rept $100
    outi
endm
else
    ld d,l
    ld b,a
    ld a,34
    inc l
    ld c,#9B
    di
    out (#99),a
    ld a,17 + 128
    out (#99),a
    out (c),h   ;origin Y
    out (c),d
    out (c),d   ;destination X
    out (c),d
    out (c),b   ;destination Y
    out (c),d
    out (c),d   ;size X
    inc l
    out (c),l
    dec l
    out (c),l   ;size Y
    out (c),d
    out (c),d   ;0
    out (c),d
    ld a,$e0   ;YMMM
    ei
    out ($9b),a
endif
    pop hl
    inc h
    push hl
    ld de,(dy)
    ld hl,(r5)
    or a   ;sets C=0
    sbc hl,de
    ld (r5),hl
    jp nz,loop0

    pop hl

    ld a,$c9   ;opcode for RET
    ld ($fd9a),a   ;stop timer

    ld hl,counter
    inc (hl)
    ld a,(dataindex)
    inc a
    cp dataentries
    jr nz,lx2

    xor a
lx2 ld (dataindex),a

    call CHGET
    and 0dfh
    cp 'Q'
    jr nz,noq
exit:
    xor a
    jp CHGMOD

noq:cp 'T'
    jp nz,mandel

         ld a,1
         ld (GRPACX),a
         ld (GRPACY),a
    ld a,(counter)
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
    and 0dfh
    cp 'Q'
    jp nz,mandel
    jr exit

div0 macro
     local t1,t2
     sla e
     rl d
     ADC   HL, HL
     jr c,t1

     LD    A,L
     ADD   A,C
     LD    A,H
     ADC   A,B
     JR    NC,t2
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
	ld A,L
PRD	add a,$30
    jp GRPPRT

PR0	ld A,$FF
	ld B,H
	ld C,L
	inc A
	add HL,DE
	jr C,$-4

	ld H,B
	ld L,C
	JR PRD

wvmem:   ;a - bank, hl - addr
    rlc h
    rla
    rlc h
    rla
    srl h
    srl h
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

mentry macro dx,dy,ni
     db -dx, dy
     dw dx*HSize/2-384   ;dx, dy, x0 = dx*HSize, niter
     db ni
endm

;x-min = (x0+dx*HSize)/512, x-max = x0/512, y-max = dy*VSize/1024
dataentries equ 12
counter db 0
dataindex db 0
data mentry 9, 18, 7 ;1
     mentry 7, 15, 8 ;2
     mentry 6, 13, 9 ;3
     mentry 5, 11, 10 ;4
     mentry 4, 10, 11 ;5
     mentry 4,  8, 12 ;6
     mentry 4,  6, 13 ;7
     mentry 3,  5, 14 ;8
     mentry 3,  5, 15 ;9
     mentry 3,  5, 16 ;10
     mentry 3,  5, 25 ;11
     mentry 3,  6, 37 ;12
ticks db 0,0,0

msg     db "****************************",13,10
        db "*   Superfast Mandelbrot   *",13,10
        db "*   fullscreen generator   *",13,10
        db "*        512x212, v6       *",13,10
        db "****************************",13,10
        db "This MSX2 code was created",13,10
        db "by Litwr, 2022-24. It is based",13,10
        db "on code published for the",13,10
        db "BK0011 in 2021 by Stanislav",13,10
        db "Maslovski.",13,10
        db "The T-key gives us timings.",13,10
        db "Use the Q-key to quit",0
ec:
sqrbase equ (msg + $16b0 + $ff) and $ff00
buf equ sqrbase + $1700
   end start

