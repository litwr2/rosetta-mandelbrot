;for pasmo assembler
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;Thanks to reddie for some help with optimization
;
;720x256 Mandelbrot for the Amstrad PCW8xxx/9xxx/10 under CP/M, 2 colors
;it works only for PAL machines

HSize equ 720
VSize equ 256

ROLLPAGE equ 2
ROLLBASE equ $7600   ;these values correspond to value $5b in port $f5, I suppose it is used always, at least under CP/M
                     ;page 1 is used to access ROLLBASE memory

sqrbase equ $8000 ;must be fixed here!
linebuf equ $1180
psp equ $1000

BDOS equ 5
INTR_VECTOR equ $38

sqrtab macro
    res 0,l
    set 7,h
endm

org #100

start
    ld de,msg
    ld c,9
    call BDOS
    call wait_char

    ;ld hl,0
    ;add hl,sp
    ;ld (ssp),hl
    ld sp,psp
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
	LD HL,(INTR_VECTOR + 1)    ;interrupt mode 1
	LD (intr_save),hl
    ld de,cursoroff
    ld c,9
    call BDOS
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

	LD HL,timer_intr
	LD (INTR_VECTOR + 1),HL
    ld a,$85
    out ($f3),a
    ld hl,0
    ld (time+2),hl
    ld (time),hl

    ld hl,0     ;scrtop, y*2
    push hl
dy equ $+1
    ld hl,0
    ld a,h
    ld h,l
    srl h
    rra
    ld l,a       ;dy*128
    ld (r5),hl
loop0:
x0 equ $+1
    ld hl,0
    ld (r4),hl
    ld de,89  ;scrtop, x
    exx
loop1:
    ld ixl,$80
loop2:
    ld hl,(r4)
dx equ $+1
    ld de,$ff00
    add hl,de
    ld (r4),hl  ;r4 += dx
    ex de,hl    ;de = r0
niter equ $+2
    ld ixh,0    ;ixh = r2
    ld hl,(r5)  ;hl = r1
loc1:
    ;push hl
    ld b,h
    ld a,l
    sqrtab
    ld c,(hl)
    inc l
    ld l,(hl)   ;bc = r3 = sqr(r1)
    ;pop hl
    ld h,b
    ld b,l
    ld l,a
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
    jr nz,loc2

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
    jr nz,loc1   ;sob r2,1$
loc2:
    ld a,ixh
    rrca
    ld a,ixl
    rra
    ld ixl,a
    jr nc,loop2
    
    exx
    ld hl,linebuf
    add hl,de
    ld (hl),a
    dec e
    exx
    jp p,loop1

   ld hl,ROLLBASE
   pop de    ;scrtop Y
   add hl,de   ;sets C=0
   ld a,$80+ROLLPAGE
;   di
   out ($f1),a
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
;   ei
   ld a,d
   and $e0
   rlca
   rlca
   rlca
   or $80
   ld iyl,a  ;bank bottom
   ld a,d
   and $1f
   or $20  ;page 1
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
   or $20  ;page 1
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

   ld de,linebuf
   ld bc,8

   rept 8
   ld a,iyh
;   di
   out ($f1),a
   ld a,(de)
   ld (hl),a

   rept 7
   inc e
   add hl,bc
   ld a,(de)
   ld (hl),a
   endm

;   ei
   inc e
   add hl,bc
   endm

   ld a,iyh
;   di
   out ($f1),a
   ld a,(de)
   ld (hl),a

   rept 7
   inc e
   add hl,bc
   ld a,(de)
   ld (hl),a
   endm

;   ei
   inc e
   add hl,bc
   bit 7,h    ;this position depends on ROLLER-RAM values
   jr z,lm0

   res 7,h
   set 6,h
   inc iyh
lm0

   rept 2
   ld a,iyh
;   di
   out ($f1),a
   ld a,(de)
   ld (hl),a

   rept 7
   inc e
   add hl,bc
   ld a,(de)
   ld (hl),a
   endm

;   ei
   inc e
   add hl,bc
   endm

   ld a,iyh
;   di
   out ($f1),a
   ld a,(de)
   ld (hl),a

   inc e
   add hl,bc
   ld a,(de)
   ld (hl),a
   ei

   pop hl
   ld de,linebuf
   rept 11
   ld a,iyl
;   di
   out ($f1),a
   ld a,(de)
   ld (hl),a

   rept 7
   inc e
   add hl,bc
   ld a,(de)
   ld (hl),a
   endm

;   ei
   inc e
   add hl,bc
   endm

   ld a,iyl
;   di
   out ($f1),a
   ld a,(de)
   ld (hl),a

   inc e
   add hl,bc   ;sets C=0
   ld a,(de)
   ld (hl),a
;   ei

    ld de,(dy)
    ld hl,(r5)
    sbc hl,de  ;C=0 here
    ld (r5),hl
    jp nz,loop0

    pop hl
    ld hl,counter
    inc (hl)
    ld a,(dataindex)
    inc a
    cp dataentries
    jr nz,lx2

    xor a
lx2 ld (dataindex),a
    ld a,$87
    out ($f3),a
    LD hl,(intr_save)
    LD (INTR_VECTOR + 1),HL
    call wait_char
    and 0dfh
    cp 'Q'
    jr z,exit

    cp 'T'
    jp nz,mandel

    ld de,home  ;home cursor
    ld c,9
    call BDOS
    ld a,(counter)
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
      push hl
      ld hl,time
      in a,($f4)
      and 15
      add a,(hl)
      ld (hl),a
      jr nc,exit_intr

      inc hl
      inc (hl)
      jr nz,exit_intr

      inc hl
      inc (hl)
exit_intr
      pop hl
      pop af
      ei
      ret

;ssp dw 0
intr_save dw 0
time dw 0,0
home db 27,"H$"
cursoroff db 27,"f$"
cursoron db 27,"e",27,"E$"

mentry macro dx,dy,ni
     db -dx, dy
     dw dx*HSize/2-384   ;dx, dy, x0 = dx*HMAX/2, niter
     db ni
endm

dataentries equ 12
counter db 0
dataindex db 0
data  ;     dx, dy, x0, niter - to convert to real values divide by 512
     mentry 5, 18, 7   ;1
     mentry 4, 15, 8   ;2
     mentry 4, 13, 9   ;3
     mentry 3, 11, 10  ;4
     mentry 3, 10, 11  ;5
     mentry 3,  8, 12  ;6
     mentry 3,  6, 13  ;7
     mentry 3,  5, 14  ;8
     mentry 2,  5, 15  ;9
     mentry 2,  5, 16  ;10
     mentry 1,  5, 25  ;11
     mentry 2,  5, 37  ;12

msg     db "**********************************",13,10
        db "* Superfast Mandelbrot generator *",13,10
        db "*     720x256, 2 colors, v5      *",13,10
        db "**********************************",13,10
        db "This Amstrad PCW code was created by",13,10
        db "Litwr in 2023-24. It is based on code",13,10
        db "published for the BK0011 in 2021 by",13,10
        db "Stanislav Maslovski.",13,10
        db "The T-key gives us timings.",13,10
        db "Use the Q-key to quit$"
   end start
