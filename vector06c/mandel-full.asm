;for pasmo assembler
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;Thanks to reddie for some help with optimization
;
;256x256 (fullscreen) Mandelbrot for the  Vector-06C, 16 colors

BDOS equ 5

HSize equ 256   ;don't change!

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
    call clscursor
    ld de,curpos
    ld c,9
    call BDOS

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
    halt
    ;di
  		LD  c,15
		LD HL,palette+15
PalLoop
		LD  a,c
		OUT (2),A
		LD  a,(HL)
		OUT (0Ch),A
		ex (sp),hl
		ex (sp),hl
		DEC HL
		DEC c
		OUT (0Ch),A
		JP P,PalLoop

    ld hl,0
    ld (tilo),hl
    ld hl,($39)
    ld (irqs),hl
    ld hl,irqp
    ld ($39),hl   ;start timer
    ;ei
mandel1:
    ld hl,$ffff  ;scrtop
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
loop1:
    ld hl,$8080
    push hl
    push hl
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
    ld (ixhmem),a   ;ixhmem = r2
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
    ;and 15
    pop hl
    rra
    ld b,a
    ld a,l
    rra
    ld l,a
    ld a,b
    rra
    ld b,a
    ld a,h
    rra
    ld h,a
    pop de
    ld a,b
    rra
    ld b,a
    ld a,e
    rra
    ld e,a
    ld a,b
    rra
    ld a,d
    rra
    ld d,a    
    jp c,lv1

    push de
    push hl
    jp loop2

lv1 ld b,h
    ld c,l
    pop hl
    ld (hl),c
    ld a,l
    cpl
    ld l,a
    ld (hl),c
    ld a,h
    sub $20
    ld h,a
    ld (hl),b
    ld a,l
    cpl
    ld l,a
    ld (hl),b
    ld a,h
    sub $20
    ld h,a
    ld (hl),e
    ld a,l
    cpl
    ld l,a
    ld (hl),e
    ld a,h
    sub $20
    ld h,a
    ld (hl),d
    ld a,l
    cpl
    ld l,a
    ld (hl),d
    dec h
    jp p,lv2

    ld a,h
    add a,$60
    ld h,a
    push hl
    jp loop1
lv2
    ld a,h
    add a,$80
    ld h,a
    dec l
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

    ld hl,irqe
    ld ($39),hl   ;stop timer
    ld hl,iter
    inc (hl)
    cp 'B'
    jp z,loc4

    call fwaitk
    and 0dfh
    cp 'Q'
    jp nz,noq
exit:
    rst 0

noq:cp 'T'
    jp nz,mandel
loc4:
	call fwaitkr
    ld hl,(irqs)
    ld ($39),hl  ;restore system irq
    ld de,curpos
    ld c,9
    call BDOS
    ld a,(iter)
    ld l,a
    ld h,0
    call PR000
    ld e," "
    ld c,2
    call BDOS
    ld hl,(tilo)
    ex de,hl
    ld hl,0
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
    push af
    ld de,curpos
    ld c,9
    call BDOS
    ld de,curclr
    ld c,9
    call BDOS
    pop af
    and 0dfh
    cp 'Q'
    jp z,exit

;    call clscursor
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

irqp push hl
tilo equ $+1
     ld hl,0
     inc hl
     ld (tilo),hl
     pop hl
irqe ei
     ret
irqs dw 0

waitk
    ld c,6  ;direct console i/o
    ld e,$ff
    call BDOS
    or a
    jp z,waitk
    ret

fwaitk
    ld a,$8a
    out (0),a
lf1 xor a
    out (3),a
    in a,(2)
    inc a
    jp z,lf1

    ld b,'Q'
    ld a,$bf
    out (3),a
    in a,(2)
    ld c,a
    and 2
    jp z,lf2
    
    ld b,'T'
    ld a,c
    and 16
    jp z,lf2

    ld b,a
lf2 ld a,$88
    out (0),a
    ld a,b
    ret

fwaitkr
    ld a,$8a
    out (0),a
lf3 xor a
    out (3),a
    in a,(2)
    inc a
    jp nz,lf3
    jp lf2

clscursor
    ld e,31  ;cls
    ld c,2
    jp BDOS

curpos db 27,$59,$21,$52,"$"
curclr db "          $"

palette db 0   ;RGB
		db 0+6*8+0*64  ;1
		db 0+4*8+0*64  ;2
		db 0+2*8+0*64  ;3
		db 0+0*8+3*64  ;4
		db 6+0*8+0*64  ;5
		db 4+0*8+0*64  ;6
		db 2+0*8+0*64  ;7
		db 6+6*8+0*64  ;8
		db 4+4*8+0*64  ;9
		db 2+2*8+0*64  ;10
		db 0+4*8+3*64  ;11
		db 4+0*8+3*64  ;12
		db 2+2*8+3*64  ;13
		db 4+7*8+2*64  ;14
		db 7+7*8+3*64  ;15

;x-min = (x0+dx*HSize)/512, x-max = x0/512, y-max = dy*VSize/1024
mentry macro dx,dy,ni
     db -dx, dy
     dw dx*HSize/2-384   ;dx, dy, x0 = dx*HSize/2, niter
     db ni
endm

iter db 0
data mentry 15, 18, 7  ;1
     mentry 13, 15, 8  ;2
     mentry 11, 13, 9  ;3
     mentry 9, 11, 10  ;4
     mentry 7, 10, 11  ;5
     mentry 7,  8, 12  ;6
     mentry 7,  6, 13  ;7
     mentry 6,  5, 14  ;8
     mentry 6,  5, 15  ;9
     mentry 6,  5, 16  ;10
     mentry 6,  5, 25  ;11
     mentry 6,  5, 37  ;12
dataindex dw data

msg     db $f,$d,$a
        db "***************************************",13,10
        db "*   Superfast Mandelbrot generator    *",13,10
        db "* 16 colors, fullscreen (256x256), v3 *",13,10
        db "***************************************",13,10
        db "This Be",203,212,"op-06",227," code was created by Litwr, 2024.",13,10
        db "It is based on code published for",13,10
        db "the ",226,"K0011 in 2021 by Stanislav Maslovski.",13,10
        db "The T-key gives us timings.",13,10
        db "Use the Q-key to quit$"

sqrbase equ ($ + $16b0 + $ff) and $ff00
   end start

