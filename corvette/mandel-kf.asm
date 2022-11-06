;for pasmo assembler
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;The next code was made by litwr in 2021
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
    ld (0xf7f1),hl   ;start timer
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

iter db 0
data db -9, 18
     dw 2232   ;dx, dy, x0, niter
     db 7   ;1
     db -7, 15
     dw 1841
     db 8   ;2
     db -6, 13
     dw 1714
     db 9   ;3
     db -5, 11
     dw 1430
     db 10  ;4
     db -4, 10
     dw 1000
     db 11  ;5
     db -4,  8
     dw 800
     db 12  ;6
     db -4,  6
     dw 700
     db 13  ;7
     db -3,  5
     dw 480
     db 14  ;8
     db -3,  5
     dw 410
     db 15  ;9
     db -3,  5
     dw 340
     db 16  ;10
     db -3,  5
     dw 340
     db 25  ;11
     db -3,  5
     dw 340
     db 37  ;12
dataindex dw data

msg     db "**************************************",13,10
        db "*   Superfast Mandelbrot generator   *",13,10
        db "* 8 colors, fullscreen (512x256), v2 *",13,10
        db "**************************************",13,10
        db "This Corvette code was created by Litwr, 2022.",13,10
        db "It is based on code published for",13,10
        db "the ",226,"K0011 in 2021 by Stanislav",13,10
        db "Maslovski.",13,10
        db "The T-key gives us timings.",13,10
        db "Use the Q-key to quit$"
sqrbase equ (msg + $16b0 + $ff) and $ff00
   end start

