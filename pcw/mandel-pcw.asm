;for pasmo assembler
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;Thanks to reddie for some help with optimization
;
;128x256 Mandelbrot for the Amstrad PCW8xxx/9xxx/10 under CP/M, monochrome
;it builds 512x256 pictures using 4x1 texture bricks to simulate 8 colors
;it works only for PAL machines

NOCALC equ 0

ROLLPAGE equ 2
ROLLBASE equ $B600   ;these values correspond to value $5b in port $f5, I suppose it is used always, at least under CP/M
                     ;page 2 is used to access ROLLBASE memory

sqrstart equ $1250
sqrbase equ sqrstart + $16b0   ;must be a multiple of $100
linebuft equ $1180
linebufb equ $1100

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

BDOS equ 5
INTR_VECTOR equ $38

org #100

start
    ld de,msg
    ld c,9
    call BDOS
    call wait_char
         and 0dfh
         ld (benchmark),a
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
	LD	HL,(INTR_VECTOR + 1)    ;interrupt mode 1
	LD	(intr_save + 1),hl
    ld de,cursoroff
    ld c,9
    call BDOS
mandel:
    ld a,16
    ld (bcount),a
	LD	HL,timer_intr
	LD	(INTR_VECTOR + 1),HL
    ld hl,0
    ld (time),hl
    ld (time+2),hl
mandel1:
    ld hl,0     ;scrtop, y*2
    push hl
    ld hl,(dy)
    ld a,h
    ld h,l
    srl h
    rra
    ld l,a       ;dy*128
    ld (r5),hl
loop0:
    ld hl,63  ;scrtop, x
    push hl
if NOCALC=0
x0 equ $+1
    ld hl,ix0
    ld (r4),hl
endif
loop1:
    ld ixl,2
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
    jr nz,loc2

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
    jr nz,loc1   ;sob r2,1$
endif
loc2:
    ld a,ixh
    and 7
patx equ $+1
    ld hl,pat0
    add a,l
    ld l,a
    ld c,(hl)
  xor 8
  ld l,a
  ld a,(hl)
    dec ixl
    jr z,lx1

    ld iyl,a  ;bottom
    ld iyh,c  ;top
    jp loop2
lx1
    pop de
    rla
    rla
    rla
    rla
    or iyl
    ld hl,linebufb
    add hl,de
    ld (hl),a
    ld a,c
    rla
    rla
    rla
    rla
    or iyh
    ld bc,$80
    add hl,bc  ;linebuft
    ld (hl),a
    dec e
    push de
    jp p,loop1

   pop de
   ld hl,ROLLBASE
   pop de    ;scrtop Y
   add hl,de   ;sets C=0
   ld a,$80+ROLLPAGE
   di
   out ($f2),a
   ld c,(hl)
   inc l
   ld b,(hl)
   ld hl,510+ROLLBASE
   sbc hl,de
   inc e
   inc de
   push de
   ld e,(hl)
   inc l
   ld d,(hl)
   ei
   ld a,d
   and $e0
   rlca
   rlca
   rlca
   or $80
   ld iyl,a  ;bank bottom
   ld a,d
   and $1f
   or $40  ;page 2
   ld d,a
   ld a,e
   and 7
   ld l,a
   ld a,e
   and $f8
   rla
   rl d
   or l
   ld e,a   ;de - addr bottom
   push de

   ld a,b
   and $e0
   rlca
   rlca
   rlca
   or $80
   ld iyh,a  ;bank top
   ld a,b
   and $1f
   or $40  ;page 2
   ld h,a
   ld a,c
   and 7
   ld l,a
   ld a,c
   and $f8
   rla
   rl h
   or l
   ld l,a    ;hl - addr top

   ld de,linebuft
   ld bc,8

   rept 7,ll
   ld a,iyh
   di
   out ($f2),a
   ld a,(de)
   ld (hl),a

   rept 7
   inc e
   add hl,bc
   ld a,(de)
   ld (hl),a
   endm

   ei
;   bit 6,h   ;BECAUSE WE USE ONLY THE FIRST 512 PIXELS!
;   jr z,l##ll
;   res 6,h
;   inc iyh
;l##ll
   inc e
   add hl,bc
   endm

   ld a,iyh
   di
   out ($f2),a
   ld a,(de)
   ld (hl),a

   rept 7
   inc e
   add hl,bc
   ld a,(de)
   ld (hl),a
   endm
   ei

   pop hl
   ld de,linebufb
   rept 7
   ld a,iyl
   di
   out ($f2),a
   ld a,(de)
   ld (hl),a

   rept 7
   inc e
   add hl,bc
   ld a,(de)
   ld (hl),a
   endm

   ei
   inc e
   add hl,bc
   endm

   ld a,iyl
   di
   out ($f2),a
   ld a,(de)
   ld (hl),a

   rept 7
   inc e
   add hl,bc
   ld a,(de)
   ld (hl),a
   endm
   ei

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
    sbc hl,de  ;C=0 here
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
    LD	hl,(intr_save + 1)
	LD	(INTR_VECTOR + 1),HL
    ;ld a,(benchmark)
    cp 'B'
    jr z,loc4

    call wait_char
    and 0dfh
    cp 'Q'
    jr z,exit

    cp 'T'
    jp nz,mandel
loc4:
    ld de,home  ;home cursor
    ld c,9
    call BDOS
    ld a,(niter)
    sub 7
    ld l,a
    ld h,0
    call PR000
    ld e," "
    ld c,2
    call BDOS
    ld hl,(time+2)
    ld de,(time)
    ld bc,300
    call div32x16r
	PUSH HL
	EX DE,HL
	call PR000
	LD e,'.'
    ld c,2
    call BDOS
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
    call wait_char
    and 0dfh
    cp 'Q'
    jp nz,mandel
exit:
    ld de,cursoron
    ld c,9
    call BDOS
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
	jr C,$-4

	ld H,B
	ld L,C
	JR PRD

wait_char
        ld c,6   ;direct console i/o
        ld e,0ffh
        call BDOS
        or a
        jr z,wait_char
        ret

timer_intr
      push af
      ld a,(time)
      inc a
      ld (time),a
      jp nz,exit_intr

      ld a,(time+1)
      inc a
      ld (time+1),a
      jp nz,exit_intr

      ld a,(time+2)
      inc a
      ld (time+2),a
exit_intr
      pop af
intr_save
      jp 0

home db 27,"H$"
cursoroff db 27,"f$"
cursoron db 27,"e",27,"E$"

        org ($ + 15)&$fff0
;pat0 db	15,1,2, 3, 5,10,14,0   ;inv
;pat1 db	15,4,9,12,14, 5, 1,0
pat0 db	0,14,13,12,10, 5, 1,15
pat1 db	0,11, 6, 3, 1,10,14,15

time dw 0,0
dx:  	dw idx
dy:	    dw idy
mx:     dw imx
  if (dx and $ff00) != ((mx+2) and $ff00)
ERROR ERROR2
  endif

benchmark db 0
bcount db 0

msg     db "**********************************",13,10
        db "* Superfast Mandelbrot generator *",13,10
        db "*         4x1 textures, v3       *",13,10
        db "**********************************",13,10
        db "The original version was published for",13,10
        db "the BK0011 in 2021 by Stanislav",13,10
        db "Maslovski.",13,10
        db "This Amstrad PCW port was created by",13,10
        db "Litwr, 2022-23.",13,10
        db "The T-key gives us timings.",13,10
        db "Use the Q-key to quit",13,10
        db "Press B to enter benchmark mode$"
   end start
