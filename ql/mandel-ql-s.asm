;for VASM assembler, Motorola syntax
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;The next code was made by litwr in 2022
;
;256x256 (fullscreen) Mandelbrot for the Sinclair Ql (the 68000 code)
;pseudo 16 colors = 4 bits per pixel but flashing is almost unusable on the QL :(

HSize = 256

    basereg SOD,a3

start:
    lea.l define(pc),a1
    move $110,a2      ;BP.INIT
    jsr (a2)

    lea.l SOD(pc),a3
    lea.l data(pc),a0
    move.l a0,dataindex(a3)
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
    moveq.l #0,d0
    rts

mandel:
    lea.l SOD(pc),a3

    move.l dataindex(pc),a0
    move (a0)+,dx(a3)
    move (a0)+,dy(a3)
    move (a0)+,x0(a3)
    move (a0),niter(a3)
    addq #2,(a0)+
    lea data+2*4*12(pc),a1
    cmpa.l a1,a0
    bne .le1

    lea.l data(pc),a0
.le1:
    move.l a0,dataindex(a3)

    lea.l serve_flag(pc),a0
    moveq.l #$1c,d0   ;MT.LPOLL
    move.w d0,(a0)+
    lea.l updtimer(pc),a1
    move.l a1,4(a0)
    clr.l 8(a0)     ;clear timer
    trap #1

    trap #0
    move.l a6,a6_save(a3)
    move #$800,a6

    moveq #-2,d6   ;-2=$fe
    movea.l #$20000+128,a5	;screen top
    movea.l #$20000+32768-128,a2 ;screen bottom
    lea.l sqr0+$16b0(pc),a4
	move dy(pc),d5
	asl #7,d5		; r5 = 128*dy
	move #$80,d7
loop0:
	move x0(pc),d4
loop2:
	add dx(pc),d4   ;r4 += dx, d4 - r4
	move niter(pc),d2	;d2 = r2  ;max iter. count
	move d4,d0		;d0 - r0
	move d5,d1		;d1 - r1
loc1:
    move d1,d1
    and.b d6,d1
	move (a4,d1.w),d3 ;d3 = r3 = sqr(r1)
	add d0,d1       ;r1 += r0
    and.b d6,d0
	move (a4,d0.w),d0    ;r0 = sqr(r0)
	add d3,d0       ;r0 += r3
	cmp a6,d0       ;if r0 >= 4.0 then
    ;cmp #$800,d0
	bcc	loc2

    and.b d6,d1
	move (a4,d1.w),d1 ;r1 = sqr(r1)
	sub d0,d1       ;r1 -= r0
	sub d3,d0       ;r0 -= r3
	sub d3,d0       ;r0 -= r3
	add d4,d0       ;r0 += r4
	add d5,d1       ;r1 += r5
	dbra d2,loc1
loc2:
    addi #1,d2
	and #15,d2      ;get bits of color
    lsl d2
    move icolor(a3,d2.w),d0
    ;move tcolor(pc),d1
    lsr #2,d7
    bcs.s loc3

    or d0,d7
    ;move d1,tcolor(a3)
    bra loop2
loc3:
    or d0,d7
    move d7,-(a5)
    move d7,-(a2)
    move #$80,d7   ;tcolor(a3)
    move a5,d0
    and.b #$7f,d0
	bne	loop2		; if not first word in line

    lea.l 256(a5),a5
	sub dy(pc),d5          ;sub	@#dya, r5
	bne loop0

    move.l a6_save(pc),a6
    andi #$07ff,sr

    lea.l serve_flag(pc),a0
    moveq.l #$1d,d0   ;MT.RPOLL
    clr.w (a0)+
    trap #1
    bra.s job_done

basini     move.l timer(pc),d1
;*
;* Convert D1.L into a floating point value (see December QLW)
;*
return_fp  move.w   d1,d4           ;D4 will be exponent
           move.l   d1,d5           ;D5 will be mantissa
           beq.s    normalised      ;Zero is a trivial case

           move.w   #2079,d4        ;First guess at exponent
           add.l    d1,d1           ;Already normalised?
           bvs.s    normalised

           subq.w   #1,d4           ;No, halve exponent weight
           move.l   d1,d5           ;Double mantissa to match
           moveq    #16,d0          ;Try a 16 bit shift

normalise  move.l   d5,d1           ;Take copy of mantissa
           asl.l    d0,d1           ;Shift mantissa D0 places
           bvs.s    too_far         ;Overflow; must shift less

           sub.w    d0,d4           ;Correct exponent for shift
           move.l   d1,d5           ;New mantissa is more normal
too_far    asr.w    #1,d0           ;Halve shift distance
           bne.s    normalise       ;Try shift of 8, 4, 2 and 1
;*
;* Check there's enough space for the result: 6 bytes
;*
normalised moveq.l #6,d1           ;No. of bytes needed
           move $11A,a0         ;BV.CHRIX vector
           jsr (a0)
           movea.l $58(a6),a1      ;Get safe A1 value
           subq.l #6,a1
           move.l a1,$58(a6)      ;Grab 6 more bytes safely

           move.l d5,2(a1,a6.l)   ;Stack mantissa
           move d4,0(a1,a6.l)   ;Stack exponent
           moveq.l #2,d4           ;Floating point result
job_done   moveq.l #0,d0
           rts
updtimer
       lea.l timer(pc),a0
       addq.l #1,(a0)
       rts

SOD:
;tcolor dc.w $80
icolor dc.w 0,$1<<6,$2<<6,$3<<6,$00<<6,$01<<6,$02<<6,$03<<6,$200<<6,$201<<6
       dc.w $202<<6,$203<<6,$200<<6,$201<<6,$202<<6,$203<<6

  macro mentry
     dc.w -\1, \2
     dc.w \1*HSize/2-384   ;dx, dy, x0 = dx*HSize, niter
     dc.w \3
  endm

data:  ; x in [x0+256dx,x0+dx], y in [-128dy,128dy] 
     ;     dx, dy,   x0, niter
     mentry 18, 18, 6   ;1
     mentry 15, 15, 7   ;2
     mentry 13, 13, 8   ;3
     mentry 11, 11, 9   ;4
     mentry  9, 10, 10  ;5
     mentry  9,  8, 11  ;6
     mentry  8,  6, 12  ;7
     mentry  7,  5, 13  ;8
     mentry  6,  5, 14  ;9
     mentry  5,  5, 15  ;10
     mentry  5,  5, 24  ;11
     mentry  5,  5, 36  ;12

define     dc.w     1               ;One procedure
           dc.w     mandel-*
           dc.b     5,'MANDL'
           dc.w     0,1             ;One function
           dc.w     basini-*
           dc.b     5,'TIMER'
           dc.w     0
serve_flag dc.w     0	    ;Set if server is on
serve_link dc.l     0       ;Points to server list
serve_ptr  dc.l     0       ;Points to server code
timer dc.l 0   ;@timer@   ;must be after serve_ptr
a6_save dc.l 0
dx dc.w 0
dy dc.w 0
x0 dc.w 0
niter dc.w 0
dataindex dc.l 0

sqr0:
;         DCB.B	$16b0,0
;sqrbase: DCB.B	$16b0,0
