;for VASM assembler, Motorola syntax
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;
;320x200 Fullscreen Mandelbrot for the Atari ST (the 68000 code), 16 colors

timer = $4ba

div32x16 macro    ;D7=D6/D4, D6=D6%D4
     moveq.l #0,d7
     divu d4,d6
     bvc .div32no\@

     swap d6
     move d6,d7
     divu d4,d7
     swap d7
     move d7,d6
     swap d6
     divu d4,d6
.div32no\@
     move d6,d7
     clr d6
     swap d6
endm

    basereg SOD,a3
start:
    lea.l SOD(pc),a3

         move.l #msg,-(sp)
         move #9,-(sp)    ;print line
         trap #1
         addq.l #6,sp
         bsr getchar

   	clr	d0		;clr r0; 7 lower bits in high byte
	clr	d1		;clr r1; higher 11+1 bits
	clr	d2		;clr r2; operand-index
	lea.l sqr0+$16b0(pc),a4	;mov	#sqr, r4; for lower half-table
	movea.l a4,a5		;mov	r4, r5; for upper half-table
fillsqr:
	move d1,(a5)+   ;mov r1, (r5)+; to upper half tbl
	addq #1,d2		;inc r2; R2 = x + 2^-9
	movea d2,a0         ;mov	r2, -(r6)
    ror #7,d2        ; R2 = 2*x + 2^-8 ; LLLLLL00 00HHHHHH
    move.b d2,d3    
    ext d3          ;movb	r2, r3		; 00000000 00HHHHHH
	add d2,d0       ;add	r2, r0		; add up lower bits
	                ;adc	r1		; add carry to r1
	addx d3,d1       ;add	r3, r1		; R1:R0 = x^2 + 2^-8*x + 2^-16
    bcs mandel0

	move d1,-(a4)   ;mov	r1, -(r4)	; to lower half tbl
	move a0,d2   ;mov	(r6)+, r2
	addq #1,d2      ;inc	r2
	bra.s fillsqr

mandel0:
    move d1,-(a4)   ;mov	r1, -(r4)	; to lower half tbl

    move #4,-(sp)
    trap #14
    move d0,screenres(a3)
    move #2,-(sp)   ;get the screen base
    trap #14
    move.l d0,screenbase(a3)   ;graph base

    move #0,-(sp)   ;320x200 16 colors mode
    move.l d0,-(sp)
    move.l d0,-(sp)
    move #5,-(sp)   ;setscreen
    trap #14

    pea palette(pc)
    move #6,-(sp)   ;set palette
    trap #14
    adda #22,sp
mandel:
    move dataindex(pc),d0
    lea.l data(pc),a0
    move.b (a0,d0.w),dx+1(a3)
    move.b 1(a0,d0.w),dy+1(a3)
    move 2(a0,d0.w),x0(a3)
    move.b 4(a0,d0.w),niter+1(a3)
    addq.b #2,4(a0,d0.w)

         clr.l -(sp)
	     move #32,-(sp)    ;super
	     trap #1
	     addq.l #6,sp
	     move.l d0,ssp(a3)
    move.l timer,time(a3)
    movea #$800,a1
    moveq #-2,d6   ;-2=$fe
    movea.l screenbase(pc),a5
    lea.l 160*199+160(a5),a6	;screen bottom
    lea.l 160(a5),a5	;screen top
    lea.l sqr0+$16b0(pc),a4
	move dy(pc),d5
    mulu #100,d5
loop0:
    suba.l a0,a0      ;line counter
	move x0(pc),d4
loop2:
	add dx(pc),d4   ;add	@#dxa, r4		; update a
	move niter(pc),d2	; max iter. count
	move d4,d0		; r0 = x = a
	move d5,d1		; r1 = y = b
loc1:
    move d1,d7
    and.b d6,d7    ;??
	move (a4,d7.w),d3 ;mov	sqr(r1), r3	; r3 = y^2
	add d0,d1       ;add	r0, r1		; r1 = x+y
    move d0,d7
    and.b d6,d7
	move (a4,d7.w),d0    ;mov	sqr(r0), r0	; r0 = x^2
	add d3,d0       ;add	r3, r0		; r0 = x^2+y^2
	cmp a1,d0      ;cmp	r0, r6		; if r0 >= 4.0 then
    ;cmp #$800,d0
	bcc	loc2		; overflow

    move d1,d7
    and.b d6,d7
	move (a4,d7.w),d1 ;mov	sqr(r1), r1	; r1 = (x+y)^2
	sub d0,d1       ;sub	r0, r1		; r1 = (x+y)^2-x^2-y^2 = 2*x*y
	add d5,d1       ;add	r5, r1		; r1 = 2*x*y+b, updated y
	sub d3,d0       ;sub	r3, r0		; r0 = x^2
	sub d3,d0       ;sub	r3, r0		; r0 = x^2-y^2
	add d4,d0       ;add	r4, r0		; r0 = x^2-y^2+a, updated x
    subi #1,d2
	bne loc1        ;sob	r2, 1$		; to the next iteration  ??dbra
loc2:
    lea tcolor1(a3),a2
    movem (a2)+,d0/d1/d3/d7
    lsr d2
    roxr d1
    lsr d2
    roxr d0
    lsr d2
    roxr d3
    lsr d2
    roxr d7
    bcs.s loc3

    movem d0/d1/d3/d7,-(a2)
    bra loop2
loc3:
    move d1,-(a5)   ;??movem
    move d1,-(a6)
    move d0,-(a5)
    move d0,-(a6)
    move d3,-(a5)
    move d3,-(a6)
    move d7,-(a5)
    move d7,-(a6)
    move #$8000,tcolor4(a3)
    addq #1,a0
    cmpa #20,a0
    bne loop2

    lea.l 320(a5),a5
	sub dy(pc),d5          ;sub	@#dya, r5
	bne loop0

	addq #1,niter(a3)     ;inc	@#nitera	; increase the iteration count

    move.l timer,d6
	addq #1,iter(a3)      ;increase the iteration count
    move dataindex(pc),d0
    add #6,d0
    cmpi #12*6,d0
    bne loc7

    moveq #0,d0
loc7:
    move d0,dataindex(a3)

         move.l	ssp(pc),-(sp)
         move.w	#32,-(sp)     ;super
	     trap #1
	     addq.l #6,sp
    bsr getchar
    andi.b #$df,d0
    cmpi.b #"Q",d0
    beq exit

    cmpi.b #"T",d0
    bne mandel

         move #27,-(sp)  ;ESC+H = home cursor
         move #2,-(sp)    ;conout
         trap #1
         move #'H',-(sp)
         move #2,-(sp)    ;conout
         trap #1
         addq.l #8,sp

    clr.l d5
    move iter(pc),d5
    bsr PR000

         move #32,-(sp)  ;space
         move #2,-(sp)    ;conout
         trap #1
         addq.l #4,sp

    sub.l time(pc),d6
    lsr.l d6        ;200 MHz
    move.l d6,d5

.l8      lea msg(pc),a5
         moveq.l #10,d4
         ;move.l d5,d6
         div32x16
         move.b d6,(a5)+
         divu d4,d7
         swap d7
         move.b d7,(a5)+
         clr d7
         swap d7
         move.b #'.'-'0',(a5)+
.l12     tst d7
         beq .l11

         divu d4,d7
         swap d7
         move.b d7,(a5)+
         clr d7
         swap d7
         bra .l12

.l11     move #'0',d0
         add.b -(a5),d0
         move d0,-(sp)
         move #2,-(sp)    ;conout
         trap #1
         addq.l #4,sp
         cmp.l #msg,a5
         bne .l11

    bsr getchar
    andi.b #$df,d0
    cmpi.b #"Q",d0
    bne mandel

exit
    move screenres(pc),-(sp)
    move.l screenbase(pc),d0
    move.l d0,-(sp)
    move.l d0,-(sp)
    move #5,-(sp)   ;setscreen
    trap #14
    add.l #12,sp
         clr -(sp)     ;term
         trap #1

PR000     ;prints d5
       lea string(pc),a0
       bsr .l2
       move.l #string,-(sp)
       move   #9,-(sp)    ;print line
       trap   #1
       addq.l #6,sp
       rts

.l2    divu #100,d5
       bsr .l0
       clr d5
       swap d5

       divu #10,d5
       bsr .l0
       swap d5

.l0    eori.b #'0',d5
       move.b d5,(a0)+
       rts

getchar:move #7,-(sp)  ;return char in D0
        trap #1        ;conin without echo
        addq.l #2,sp
        tst.b d0
        beq.s getchar
        rts
SOD:
dx	dc.w	-1
dy	dc.w	0
x0     dc.w   0
niter  dc.w   0

ssp dc.l 0
time dc.l 0
screenbase dc.l 0
screenres dc.w 0
palette
	DC.W	$000,$770,$070,$777  ;black, green, yellow, white
	DC.W	$700,$707,$077,$007  ;red, magenta, cyan, blue
	DC.W	$400,$404,$044,$004  ;red, magenta, cyan, blue
	DC.W	$040,$440,$333,$555  ;darkgreen, yellow, darkgray, gray
;    dc.w $777,$700,$070,$000,$007,$707,$077,$555  ;original
;    dc.w $333,$733,$373,$773,$337,$737,$377,$000

tcolor1 dc.w 0
tcolor2 dc.w 0
tcolor3 dc.w 0
tcolor4 dc.w $8000

  macro mentry
     dc.b -\1, \2
     dc.w \1*320/2-384   ;dx, dy, x0 = dx*HSize, niter
     dc.b \3,0
  endm

dataindex dc.w 0
iter dc.w 0
data mentry 9, 14, 15 ;1
     mentry 7, 11, 16 ;2
     mentry 6,  9, 18 ;3
     mentry 5,  8, 20 ;4
     mentry 4,  7, 21 ;5
     mentry 4,  6, 22 ;6
     mentry 4,  5, 23 ;7
     mentry 3,  4, 24 ;8
     mentry 3,  4, 25 ;9
     mentry 3,  4, 26 ;10
     mentry 3,  4, 27 ;11
     mentry 4,  6, 37 ;12

msg     dc.b "  **********************************",13,10
        dc.b "  * Superfast Mandelbrot generator *",13,10
        dc.b "  *    fullscreen, 16 colors, v1   *",13,10
        dc.b "  **********************************",13,10
        dc.b "This code for the Atari ST was created by",13,10
        dc.b "Litwr in 2023. It is based on code",13,10
        dc.b "published for the BK0011 in 2021 by",13,10
        dc.b "Stanislav Maslovski.",13,10
        dc.b "The T-key gives us timings.",13,10
        dc.b "Use the Q-key to quit.",13,10
        dc.b "Press a "
string  dc.b "key",0

        align 1
sqr0

