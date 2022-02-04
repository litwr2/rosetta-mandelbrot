;for VASM assembler, Motorola syntax
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;The next code was made by litwr in 2022
;
;256x256 (fullscreen) Mandelbrot for the Sinclair Ql (the 68000 code)
;pseudo 16 colors = 4 bits per pixel but flashing is almost unusable on the QL :(

    basereg SOD,a3

start:
    lea.l define(pc),a1
    move $110,a2      ;BP.INIT
    jsr (a2)

    lea.l SOD(pc),a3
    lea.l data(a3),a0
    move.l a0,dataindex(a3)
   	clr	d0		;clr r0; 7 lower bits in high byte
	clr	d1		;clr r1; higher 11+1 bits
	clr	d2		;clr r2; operand-index
	lea.l sqr0+$16b0(a3),a4	;mov	#sqr, r4; for lower half-table
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

    move.l dataindex(a3),a0
    move (a0)+,dx(a3)
    move (a0)+,dy(a3)
    move (a0)+,x0(a3)
    move (a0),niter(a3)
    addq #2,(a0)+
    lea data+2*4*12(a3),a1
    cmpa.l a1,a0
    bne .le1

    lea.l data(a3),a0
.le1:
    move.l a0,dataindex(a3)

    lea.l serve_flag(a3),a0
    moveq.l #$1c,d0   ;MT.LPOLL
    move.w d0,(a0)+
    lea.l updtimer(a3),a1
    move.l a1,4(a0)
    clr.l 8(a0)     ;clear timer
    trap #1

    move sr,sr_save(a3)
    trap #0
    move.l a6,a6_save(a3)
    move #$800,a6

    moveq #-2,d6   ;-2=$fe
    movea.l #$20000+128,a5	;screen top
    movea.l #$20000+32768-128,a2 ;screen bottom
    lea.l sqr0+$16b0(a3),a4
	move dy(a3),d5
	asl #7,d5		; r5 = 128*dy
loop0:
	move x0(a3),d4
loop2:
	add dx(a3),d4   ;add	@#dxa, r4		; update a
	move niter(a3),d2	; max iter. count
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
	cmp a6,d0      ;cmp	r0, r6		; if r0 >= 4.0 then
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
	and #15,d2      ;bic	#177770, r2	; get bits of color
    lsl d2
    move icolor(a3,d2.w),d0
    move tcolor(a3),d1
    lsr #2,d1
    bcs.s loc3

    or d0,d1
    move d1,tcolor(a3)
    bra loop2
loc3:
    or d0,d1
    move d1,-(a5)
    move d1,-(a2)
    move #$80,tcolor(a3)
    move a5,d0
    and.b #$7f,d0
	bne	loop2		; if not first word in line

    lea.l 256(a5),a5
	sub dy(a3),d5          ;sub	@#dya, r5
	bne loop0

    move.l a6_save(a3),a6
    move sr_save(a3),sr
    lea.l serve_flag(a3),a0
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
tcolor dc.w $80
icolor dc.w 0,$1<<6,$2<<6,$3<<6,$00<<6,$01<<6,$02<<6,$03<<6,$200<<6,$201<<6
       dc.w $202<<6,$203<<6,$200<<6,$201<<6,$202<<6,$203<<6
     ;     dx, dy,   x0, niter
data
     dc.w -18, 18, 2232, 7   ;1
     dc.w -15, 15, 1841, 8   ;2
     dc.w -13, 13, 1714, 9   ;3
     dc.w -11, 11, 1430, 10  ;4
     dc.w  -9, 10, 1200, 11  ;5
     dc.w  -9,  8, 1120, 12  ;6
     dc.w  -8,  6, 1000, 13  ;7
     dc.w  -7,  5,  700, 14  ;8
     dc.w  -6,  5,  500, 15  ;9
     dc.w  -5,  5,  320, 16  ;10
     dc.w  -5,  5,  300, 25  ;11
     dc.w  -5,  5,  270, 37  ;12

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
sr_save dc.l 0
a6_save dc.l 0
dx dc.w 0
dy dc.w 0
x0 dc.w 0
niter dc.w 0
dataindex dc.l 0

sqr0:
;         DCB.B	$16b0,0
;sqrbase: DCB.B	$16b0,0
