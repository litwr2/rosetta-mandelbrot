;for pasmo assembler
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;Thanks to reddie for some help with optimization
;
;128x256 Mandelbrot for the Amstrad CPC, 4 color mode, simulates 8 colors using textures

SCR_SET_MODE            EQU #BC0E
SCR_SET_INK             EQU #BC32
TXT_OUTPUT              EQU #BB5A
KM_WAIT_CHAR		EQU #BB06
KL_TIME_PLEASE          EQU #BD0D

sqrbase equ $8000 ;must be fixed here!
initer	equ	7
idx	equ	-36       ;-.0703125
idy	equ	18        ;.03515625
ix0	equ	-62*idx
imx	equ	10*idx		; x move
sf4	equ	436/4		; sf/4

NOCALC equ 0

org #9700

start
    ld hl,msg
char:
    ld a,(hl)
    or a
    jr z,ni

    call TXT_OUTPUT
    inc hl
    jr char

ni: call KM_WAIT_CHAR
    and 0dfh
    ld (benchmark),a
    call setvmode
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
    ld hl,0   ;the sqrbase top part
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
    call KL_TIME_PLEASE
    ld (ti),hl
    ld (ti+2),de
mandel1:
    ld ixl,2
    ld hl,$4040  ;scrtop
    push hl
    ld hl,(dy)
    ld a,h
    ld h,l
    srl h
    rra
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
    ld hl,(r4)
    ld de,(dx)
    add hl,de
    ld (r4),hl  ;r4 += dx
    ex de,hl    ;de = r0
endif
niter equ $+2
    ld ixh,initer   ;ixh = r2
if NOCALC=0
    ld hl,(r5)  ;hl = r1
loc1:
    ;push hl
    ld b,h
    ld a,l
    res 0,l
    set 7,h
    ld c,(hl)
    inc l
    ld l,(hl)   ;bc = r3 = sqr(r1)
    ;pop hl
    ld h,b
    ld b,l
    ld l,a
    add hl,de   ;r1 += r0
    ex de,hl    ;de - r1, hl - r0, bc - r3
    res 0,l
    set 7,h
    ld a,(hl)
    inc l
    ld h,(hl)
    ld l,a       ;r0 = sqr(r0)
    add hl,bc    ;r0 += r3
    ld a,h
    and $f8      ;sets C=0
    jr nz,loc2

    ex de,hl    ;de - r0, hl - r1
    set 7,h
    res 0,l
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
    jr nz,loc1   ;sob r2,1$
endif
loc2:
    ld a,ixh   ;color
    and 7
patx equ $+1
    ld hl,pat0
    add a,l
    ld l,a
    ld c,(hl) ;bottom
  xor 8
  ld l,a
  ld a,(hl)  ;top
    dec ixl
    jr z,lx1

    rrca
    rrca
    ld iyl,a
    ld a,c
    rrca
    rrca
    ld iyh,a
    jp loop2
lx1
    ld ixl,2
    or iyl
    pop hl  ;scrtop
    dec hl
    ld (hl),a
    push hl
    ld a,$3f
    xor h
    ld h,a
    ld a,$c0
    xor l
    ld l,a
    ld a,iyh
    or c
    ld (hl),a
    ld a,l
    and $3f
    jp nz,loop2

    ld de,$840
    pop hl  ;scrtop
    add hl,de
    ld a,h
    rlca
    jr nc,lx10

    ld de,$c040
    add hl,de
lx10:
    push hl
    ld c,low(pat0)
    ld a,(patx)
    cp c    ;sets C=0
    jr nz,lx8

    ld c,low(pat1)
lx8:
    ld a,c
    ld (patx),a
    ld de,(dy)
    ld hl,(r5)
    ;or a   ;C=0 is already set
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
    res 0,l
    set 7,h
    ld c,(hl)
    inc l
    ld b,(hl)
    ld de,sf4
    pop hl
    add hl,de
    res 0,l
    set 7,h
    ld a,(hl)
    inc l
    ld h,(hl)
    ld l,a
    or a ;sets C=0
    sbc hl,bc  ;C=0
dx2p equ $+1
    ld (dx),hl
    jr lx5

lx2:pop hl
endif
    ld a,(benchmark)
    cp 'B'
    jr nz,loc3

    ld hl,bcount
    dec (hl)
    jp nz,mandel1
loc3:
    call KL_TIME_PLEASE
    xor a
    ld bc,(ti)
    sbc hl,bc
    ld (ti),hl
    ex de,hl
    ld bc,(ti+2)
    sbc hl,bc
    ld (ti+2),hl
    ld a,(benchmark)
    cp 'B'
    jr z,loc4

    call KM_WAIT_CHAR
    and 0dfh
    cp 'Q'
    jr nz,noq
    rst 0
noq:cp 'T'
    jp nz,mandel
loc4:
    ld a,30  ;home cursor
    call TXT_OUTPUT
    ld a,(niter)
    sub 7
    ld l,a
    ld h,0
    call PR000
    ld a," "
    call TXT_OUTPUT
    ld hl,(ti+2)
    ld de,(ti)
    ld bc,300
    call div32x16r
	PUSH HL
	EX DE,HL
	call PR000
	LD a,'.'
    call TXT_OUTPUT
	POP hl
        push hl     ;*100/3
        add hl,hl
        add hl,hl
        pop de
        add hl,de
        push hl
        add hl,hl
        add hl,hl
        pop de
        add hl,de
        add hl,hl
        add hl,hl
        ex de,hl
        ld hl,0
        ld bc,3
        call div32x16r
        ld a,l
        cp 2
        jr c,$+3
        inc de
        ex de,hl
	call PR0000
    call KM_WAIT_CHAR
    and 0dfh
    cp 'Q'
    jp nz,mandel
    rst 0

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
PR000
	ld de,-100
	CALL PR0
	ld de,-10
	CALL PR0
	ld A,L
PRD	add a,$30
    jp TXT_OUTPUT

PR0	ld A,$FF
	ld B,H
	ld C,L
	inc A
	add HL,DE
	jr C,$-4

	ld H,B
	ld L,C
	JR PRD

setvmode
    ld a,1
    call SCR_SET_MODE
    ld a,15
    ld c,11
    ld b,c
    call SCR_SET_INK
    ld a,$40
    ld ($b7c6),a    ;screen base for system text output

; Wait for THE BEGINNING of a VSYNC signal
wait_vbl_safe
ld b,&f5
wait_vbl_end
in a,(c)
rra
jp c,wait_vbl_end
; Wait for VSYNC signal (don't care if we're already
; there)
wait_vbl
ld b,&f5
in a,(c)
rra
jp nc,wait_vbl

; Wait for first interrupt (~2 scanlines later)
halt

; Write "horizontal" CRTC registers
ld hl,inithvideocfg
ld c,2
call write_CRTC

; Wait for the third interrupt
halt
halt

; Write vertical CRTC registers for a smooth transition
ld hl,initvvideocfg
ld c,4
jp write_CRTC

; Write register/value pairs to CRTC
; INPUT
; HL - Point to the list of register/value pairs
; C  - Number of pairs in the list
write_CRTC
ld b,&bd
writeCRTCloop
outi
inc b
inc b
outi
dec c
jr nz,writeCRTCloop
ret

inithvideocfg
db 1,32,2,42
initvvideocfg
db 6,32,7,35,&c,16,&d,0

ti     dw 0,0

        org ($ + 15)&$fff0
pat0	db 0, 0x8, 0x80, 0x88, 0xc4, 0xc, 0xc0, 0xcc
pat1	db 0, 0x4, 0x40, 0x44, 0x4, 0xc, 0xc0, 0xcc
; 0 - black, 1 - blue-black, 2 - green-black, 3 - red-black, 14 - green-red, 5 - blue, 10 - green, 15 - red
; 0 - black, 4 - black-blue, 8 - black-green, 12 - black-red, 4 - black-blue, 5 - blue, 10 - green, 15 - red

benchmark db 0
bcount db 0

dx  	dw idx
dy	    dw idy
mx      dw imx
if (dx and $ff00) != ((mx+2) and $ff00)
.ERROR ERROR2
endif

msg     db "**********************************",13,10
        db "* Superfast Mandelbrot generator *",13,10
        db "*    4 colors + textures, v10    *",13,10
        db "**********************************",13,10
        db "The original version was published for",13,10
        db "the BK0011 in 2021 by Stanislav",13,10
        db "Maslovski.",13,10
        db "This Amstrad CPC port was created by",13,10
        db "Litwr, 2021-24.",13,10
        db "The T-key gives us timings.",13,10
        db "Use the Q-key to quit",13,10
        db "Press B to enter benchmark mode",0
   end start

