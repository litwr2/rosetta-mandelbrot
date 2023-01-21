;for pasmo assembler
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;Thanks to reddie for some help with optimization
;
;Horizontal Overscan Version
;HMAXxVMAX Mandelbrot for the Amstrad CPC, 16 color mode
;HMAX = 160, 176, 192
;VMAX = range 192-280 was tested, it must be a multiple of 8

SCR_SET_MODE            EQU #BC0E
SCR_SET_INK             EQU #BC32
TXT_OUTPUT              EQU #BB5A
KM_WAIT_CHAR		EQU #BB06
KL_TIME_PLEASE          EQU #BD0D

HMAX equ 192
VMAX equ 280

sqrbase equ $8000 ;do not change!

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

    call KL_TIME_PLEASE
    ld (ti),hl
    ld (ti+2),de
    ld ixl,2
    ld iyh,VMAX/2
    ld hl,HMAX/2+$40  ;scrtop, $40 - offset on zp
    push hl
    ld hl,HMAX/2*VMAX/8+$3840
    push hl    ;scrbot
dy equ $+1
    ld hl,0
    ld a,VMAX/2
    call mul16
    ld (r5),de
loop0:
x0 equ $+1
    ld hl,0
    ld (r4),hl
    ld iyl,HMAX/2
loop2
    ld hl,(r4)
dx equ $+1
    ld de,$ff00
    add hl,de
    ld (r4),hl
    ex de,hl      ;mov	r4, r0
niter equ $+2
    ld ixh,0
    ld hl,(r5)  ;mov	r5, r1	
loc1:
    push hl
    res 0,l
    set 7,h
    ld c,(hl)
    inc l
    ld b,(hl)   ;mov	sqr(r1), r3
    pop hl
    add hl,de   ;add	r0, r1
    ex de,hl    ;de - r1, hl - r0, bc - r3
    res 0,l
    set 7,h
    ld a,(hl)
    inc l
    ld h,(hl)
    ld l,a       ;mov	sqr(r0), r0
    add hl,bc    ;add	r3, r0
    ld a,h
    and $f8
    jr nz,loc2

    push hl
    sbc hl,bc   ;x^2  ;set C=0
    sbc hl,bc   ;x^2-y^2
r4 equ $+1
    ld bc,0
    add hl,bc   ;x^2-y^2+x0
    ex de,hl    ;de - r0, hl - r1
    set 7,h
    res 0,l
    ld a,(hl)
    inc l
    ld h,(hl)
    ld l,a       ;(x+y)^2
r5 equ $+1
    ld bc,0
    add hl,bc    ;sets C=0
    pop bc   ;r0
    sbc hl,bc    ;2xy+y0
    dec ixh
    jr nz,loc1   ;sob r2,1$
loc2:
    ld a,ixh   ;color
    and 15   ;16 colors
    ld hl,c8t
    add a,l
    ld l,a
    ld c,(hl)
    dec ixl
    jp z,lx1

    ld a,c
    rrca
    ld (tcolor),a
    jp loop2
lx1
    ld ixl,2
tcolor equ $+1
    ld a,0
    or c
    pop de  ;scrbot
    pop hl  ;scrtop
    dec hl
    ld (hl),a
    ld c,a
    push hl
    ex de,hl
    dec hl
    push hl
    ld a,iyh
    cp ($7C0*2/HMAX)*8-VMAX/2+1
    jr c,lv1

    cp ($7C0*2/HMAX)*8-VMAX/2+9
    jr nc,lv2

    bit 2,h
    jr nz,lv1

lv2 ld a,$38
    add a,h
    ld h,a
lv1 ld (hl),c
    dec iyl
    jp nz,loop2

    dec iyh
    pop de  ;scrbot
    pop hl  ;scrtop
    ld bc,$800+HMAX/2
    add hl,bc
    push hl
    ex de,hl
    ld bc,HMAX/2-$800
    add hl,bc
    push hl
    ld a,iyh
if (VMAX & 8) != 0
    xor 4
endif
    and 7      ;sets C=0 
    jr nz,lv3

    pop de  ;scrbot
    pop hl  ;scrtop
    ld bc,$4000-HMAX/2
    sbc hl,bc
    push hl
    ex de,hl
    add hl,bc
    push hl
lv3 ld de,(dy)
    ld hl,(r5)
    or a   ;sets C=0
    sbc hl,de
    ld (r5),hl
    jp nz,loop0

    pop hl  ;??
    pop hl  ;??
    ld hl,counter
    inc (hl)
    ld a,(dataindex)
    inc a
    cp dataentries
    jr nz,lx2

    xor a
lx2 ld (dataindex),a
    call KL_TIME_PLEASE
    xor a
    ld bc,(ti)
    sbc hl,bc
    ld (ti),hl
    ex de,hl
    ld bc,(ti+2)
    sbc hl,bc
    ld (ti+2),hl
    call KM_WAIT_CHAR
    and 0dfh
    cp 'Q'
    jr nz,noq
    rst 0

noq:cp 'T'
    jp nz,mandel

    ld a,30  ;home cursor
    call TXT_OUTPUT
    ld a,(counter)
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

ti     dw 0,0

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

c8t:    db 0, 8, $22, $88, $2a, $28, 2, $8a
        db $80, $20, $a0, $a8, $82, $a2, $aa, $a

PR0000  ld de,-1000
	CALL PR0
PR000	ld de,-100
	CALL PR0
PR00	ld de,-10
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

mul16 proc   ;multiply HL by A -> DE
    local t3,t4

    ld de,0
t3  srl a
    jr nc,t4

    ex de,hl
    add hl,de
    ex de,hl
t4  add hl,hl
    or a
    jr nz,t3
    ret
    endp

setvmode
    xor a
    call SCR_SET_MODE
    ld a,15
    ld c,11
    ld b,c
    call SCR_SET_INK
if VMAX>239
if HMAX=160
    ld a,$41  ;$29;$21;$1c;$19;$11;9;1;$39;$41;$42
endif
if HMAX=176
    ld a,$43  ;1;2;3;$b;$3a;$41;$42;$43
endif
if HMAX=192
    ld a,$42  ;$42;$44
endif
else
    ld a,1
endif
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
ld c,6
call write_CRTC

; Wait for the third interrupt
halt
halt

; Write vertical CRTC registers for a smooth transition
ld hl,initvvideocfg
ld c,2
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
db 1,HMAX/4,2
if HMAX=160 or HMAX=176
db HMAX/4+(64-14-HMAX/4)/2+1,&3,&8e
endif
if HMAX=192
db HMAX/4+2,3,&8a
endif
db 12,&c,13,&20
initvvideocfg
db 6,VMAX/8,7,35

mentry macro dx,dy,ni
     db -dx*176/HMAX, dy*256/VMAX
     dw (dx*176/HMAX)*HMAX/2-384   ;dx, dy, x0 = dx*HMAX/2, niter
     db ni
endm

dataentries equ 12
counter db 0
dataindex db 0
data  ;     dx, dy, x0, niter - to convert to real values divide by 512
     mentry 18, 18, 7   ;1
     mentry 15, 15, 8   ;2
     mentry 13, 13, 9   ;3
     mentry 11, 11, 10  ;4
     mentry 9, 10, 11  ;5
     mentry 9,  8, 12  ;6
     mentry 8,  6, 13  ;7
     mentry 7,  5, 14  ;8
     mentry 6,  5, 15  ;9
     mentry 5,  5, 16  ;10
     mentry 5,  5, 25  ;11
     mentry 8,  5, 37  ;12

msg     db "**********************************",13,10
        db "* Superfast Mandelbrot generator *",13,10
        db "*     "
        db HMAX/100+48
        db (HMAX-(HMAX/100)*100)/10+48
        db HMAX % 10+48
        db "x"
        db VMAX/100+48
        db (VMAX-(VMAX/100)*100)/10+48
        db VMAX % 10+48
        db ", 16 colors, v6     *",13,10
        db "**********************************",13,10
        db "This Amstrad CPC code was created by",13,10
        db "Litwr in 2022. It is based on code",13,10
        db "published for the BK0011 in 2021 by",13,10
        db "Stanislav Maslovski.",13,10
        db "The T-key gives us timings.",13,10
        db "Use the Q-key to quit",0
   end start

