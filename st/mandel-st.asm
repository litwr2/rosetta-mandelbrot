;for VASM assembler, Motorola syntax
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;
;256x128 Mandelbrot for the Atari ST (the 68000 code), 16 colors, rotated
;on the 320x256 16 colors screen

NOCALC = 0

timer = $4ba

initer	= 7
idx	=	-36       ;-.0703125
idy	=	18        ;.03515625, 1 = 1/512
ix0	=	-62*idx
imx	=	10*idx		; x move
sf4	=	436/4		; sf/4

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
    movea.l a3,a4
    move.l #idx*65536+idy,(a4)+
    move.l #imx*65536+ix0,(a4)+
    move #initer,(a4)

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

    move #2,-(sp)   ;get the screen base
    trap #14
    addq.l #2,sp
    move.l d0,screenbase(a3)   ;graph base

    move #0,-(sp)   ;320x200 16 colors mode
    move.l d0,-(sp)
    move.l d0,-(sp)
    move #5,-(sp)   ;setscreen
    trap #14
    add.l #12,sp

mandel:
         clr.l -(sp)
	     move #32,-(sp)    ;super
	     trap #1
	     addq.l #6,sp
	     move.l d0,ssp(a3)
    move.l timer,time
;    move #$800,a6
    moveq #-2,d6   ;-2=$fe
    movea.l screenbase(pc),a5
    lea.l 160*127+8(a5),a6	;screen bottom - actually the bottom left edge
    lea.l 160*127+128(a5),a5	;screen top - actually the bottom right edge
    lea.l sqr0+$16b0(pc),a4
	move dy(pc),d5
	asl #7,d5		; r5 = 128*dy
    suba.l a2,a2   ;the color shift
loop0:
    suba.l a0,a0      ;line counter
  if NOCALC=0
	move x0(pc),d4
  endif
loop2:
  if NOCALC=0
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
	;cmp a6,d0      ;cmp	r0, r6		; if r0 >= 4.0 then
    cmp #$800,d0
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
  endif
    move a2,d0
    bne.s lx2

    clr d3
    lsr d2
    roxl d3
    move d3,d1
    ror d1
    move d3,-(a5)
    move d1,-(a6)

    clr d3
    lsr d2
    roxl d3
    move d3,d1
    ror d1
    move d3,-(a5)
    move d1,-(a6)

    clr d3
    lsr d2
    roxl d3
    move d3,d1
    ror d1
    move d3,-(a5)
    move d1,-(a6)

    clr d3
    lsr d2
    roxl d3
    move d3,d1
    ror d1
    move d3,-(a5)
    move d1,-(a6)
    bra lx3

lx2
    clr d3
    lsr d2
    roxl d3
    move d3,d1
    ror d1
    lsr d0,d1
    lsl d0,d3
    or d3,-(a5)
    or d1,-(a6)

    clr d3
    lsr d2
    roxl d3
    move d3,d1
    ror d1
    lsr d0,d1
    lsl d0,d3
    or d3,-(a5)
    or d1,-(a6)

    clr d3
    lsr d2
    roxl d3
    move d3,d1
    ror d1
    lsr d0,d1
    lsl d0,d3
    or d3,-(a5)
    or d1,-(a6)

    clr d3
    lsr d2
    roxl d3
    move d3,d1
    ror d1
    lsr d0,d1
    lsl d0,d3
    or d3,-(a5)
    or d1,-(a6)
lx3
    lea.l -152(a5),a5
    lea.l -152(a6),a6
    addq #1,a0
    cmpa #128,a0
    bne loop2

    lea.l 160*128(a5),a5
    lea.l 160*128(a6),a6
    addq #1,a2
    cmpa #16,a2
    bne .lx1

    suba a2,a2
    lea.l -8(a5),a5
    lea.l 8(a6),a6
.lx1
	sub dy(pc),d5          ;sub	@#dya, r5
	bne loop0
  if NOCALC=0
	move mx(pc),d0
    add d0,x0(a3)          ;add @#mxa, @#x0a	; shift x0

	; scale the params
	move #2,d0         ;mov	#3, r0
	lea.l dx(pc),a1     ;mov	#dxa, r1
loc4:
	move (a1),d2        ;mov	(r1), r2		; x
    move d2,d3
    add #sf4,d2
    and.b #$fe,d2
	move (a4,d2.w),(a1) ;mov	sqr+sf4(r2), (r1)	; (x + sf/4)^2
    sub #sf4,d3
    and.b #$fe,d3
    move (a4,d3.w),d1
	sub d1,(a1)+          ;sub	sqr-sf4(r2), (r1)+ 	; (x + sf/4)^2 - (x - sf/4)^2 = x*sf
	dbra d0,loc4          ;sob	r0, 4$
  endif
	addq #1,niter(a3)     ;inc	@#nitera	; increase the iteration count

    move.l timer,d6
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

         move #27,-(sp)  ;ESC+H = clrscr
         move #2,-(sp)    ;conout
         trap #1
         move #'H',-(sp)
         move #2,-(sp)    ;conout
         trap #1
         addq.l #8,sp

    move niter(pc),d5
    subq #7,d5
    bsr PR000

         move #32,-(sp)  ;space
         move #2,-(sp)    ;conout
         trap #1
         addq.l #4,sp

    sub.l time(pc),d6
    lsr.l d6        ;200 MHz
    move.l d6,d5

.l8      lea string(pc),a5
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
         cmp.l #string,a5
         bne .l11

    bsr getchar
    andi.b #$df,d0
    cmpi.b #"Q",d0
    bne mandel

exit:    clr -(sp)     ;term
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
dx	dc.w	idx
dy	dc.w	idy
mx	dc.w	imx
x0     dc.w   ix0
niter  dc.w    initer

ssp dc.l 0
time dc.l 0
screenbase dc.l 0

msg     dc.b "  **********************************",13,10
        dc.b "  * Superfast Mandelbrot generator *",13,10
        dc.b "  *          16 colors, v1         *",13,10
        dc.b "  **********************************",13,10
        dc.b "The original version was published for",13,10
        dc.b "the BK0011 in 2021 by Stanislav Maslovski.",13,10
        dc.b "This Atari ST port was created by Litwr, 2023.",13,10
        dc.b "The T-key gives us timings.",13,10
        dc.b "Use the Q-key to quit.",13,10
        dc.b "Press a "
string  dc.b "key",0

        align 1
sqr0

