;for VASM assembler, Motorola syntax
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;
;640x400 Mandelbrot for the Atari ST (the 68000 code), 2 colors

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
    
         pea msg(pc)
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
    move.l d0,screenbase(a3)   ;graph base
    addq #2,sp
mandel:
    move dataindex(pc),d0
    lea.l data(pc),a0
    move.b (a0,d0.w),dx+1(a3)
    move.b 1(a0,d0.w),dy+1(a3)
    move 2(a0,d0.w),x0(a3)
    move.b 4(a0,d0.w),niter+1(a3)
    addq.b #2,4(a0,d0.w)

         pea discursor(pc)
         move #9,-(sp)    ;print line
         trap #1
         addq.l #6,sp

         clr.l -(sp)
	     move #32,-(sp)    ;super
	     trap #1
	     addq.l #6,sp
	     move.l d0,ssp(a3)
	     
	     clr $ff8240  ;invert colors
	     
    move.l timer,time(a3)
mandel1:
    movea #$800,a1
    moveq #-2,d6   ;-2=$fe
    movea.l screenbase(pc),a5
    lea.l 80*400(a5),a6	;screen bottom
    lea.l 80(a5),a5	;screen top
    lea.l sqr0+$16b0(pc),a4
	move dy(pc),d5
	mulu #200,d5
loop0:
    move.b #20,linecount(a3)      ;line counter
	move x0(pc),d4
loop1:
        movea.l #$80000000,a0
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
    move.l a0,d3
    roxr #1,d2
    roxr.l #1,d3
    movea.l d3,a0
    bcc.s loop2
loc8
    move.l d3,-(a6)
    move.l d3,-(a5)
    subq.b #1,linecount(a3)
    bne.s loop1

    lea 160(a5),a5
	sub dy(pc),d5          ;sub	@#dya, r5
	bne loop0

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
loc5:
         pea home(pc)
         move #9,-(sp)    ;print line
         trap #1
         addq.l #6,sp

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
         ;pea encursor(pc)
         ;move #9,-(sp)    ;print line
         ;trap #1
         ;addq.l #6,sp
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

  macro mentry
     dc.b -\1, \2
     dc.w \1*320-384   ;dx, dy, x0 = dx*HSize, niter
     dc.b \3,0
  endm

dataindex dc.w 0
iter dc.w 0
data mentry 8, 12, 10 ;1
     mentry 6, 10, 11 ;2
     mentry 5,  8, 12 ;3
     mentry 4,  7, 14 ;4
     mentry 3,  6, 15 ;5
     mentry 3,  5, 16 ;6
     mentry 3,  4, 17 ;7
     mentry 2,  4, 18 ;8
     mentry 3,  3, 19 ;9
     mentry 4,  3, 20 ;10
     mentry 2,  5, 21 ;11
     mentry 2,  3, 37 ;12

linecount dc.b 0

home    dc.b 27,"H",0
discursor dc.b 27,"f",0
;encursor dc.b 27,"e",0
msg     dc.b "  **********************************",13,10
        dc.b "  * Superfast Mandelbrot generator *",13,10
        dc.b "  *   mono fullscreen, 640x400, v1 *",13,10
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

