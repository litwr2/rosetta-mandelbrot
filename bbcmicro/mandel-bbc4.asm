;for vasm assembler, oldstyle syntax
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;The next code was made by litwr in 2021
;
;128x256 Mandelbrot for the BBC Micro, 4 color mode simulates 8 colors using textures

OSWRCH = $FFE3
OSRDCH = $FFE0
OSWORD = $FFF1

sqrbase = $2900 ;must be $xx00, it takes area $1250-$3fb0
initer	= 7
idx	=	-36       ;-.0703125
idy	=	18        ;.03515625, 1 = 1/512
ix0	=	-62*idx
imx	=	10*idx		; x move
sf4	=	436/4		; sf/4

r0 = $70
r1 = $72
r2 = $74
alo = $75
r3 = $76
r4 = $78
r5 = $7a
tmp = $7c
zpat1 = $7e
zpat2 = $80

d = $74    ;..$77
divisor = $74     ;..$77
dividend = $78	  ;..$7b
remainder = $70   ;$71
quotient = dividend ;save memory by reusing divident to store the quotient

;black - 0
;red - 1
;yellow = 2
;white = 3

   org $e00
   jsr init
fillsqr:
   lda #0
   sta tmp
    tay
    tax
.loop:
    sta r0,x
    inx
    cpx #6
    bne .loop

    lda #>sqrbase
    ;ldx #<sqrbase   ;=0
    sta r4+1
    sty r4
    sta r5+1
    sty r5
sqrloop:
    lda r1     ;mov	r1, (r5)+	; to upper half tbl
    sta (r5),y
    lda r1+1
    iny
    sta (r5),y
    dey
    clc
    lda r5
    adc #2
    sta r5
    bcc .l1

    inc r5+1
.l1:inc r2	  ;inc	r2		; R2 = x + 2^-9
    bne .l2

    inc r2+1
.l2:lda r2    ;mov	r2, -(r6)
    pha
    lda r2+1
    pha
	asl r2    ;asl	r2		; R2 = 2*x + 2^-8
    rol       ;swab	r2		; LLLLLL00 00HHHHHH
    ldx r2
    stx r2+1
    sta r2
	sta r3    ;movb	r2, r3		; 00000000 00HHHHHH
    sty r3+1
	clc       ;add	r2, r0		; add up lower bits
    adc r0
    sta r0
    txa
    adc r0+1
    sta r0+1
	tya       ;adc	r1		; add carry to r1
    adc r1
    tax
    tya
    adc r1+1   ;sets C=0
    sta r1+1
	txa        ;add	r3, r1		; R1:R0 = x^2 + 2^-8*x + 2^-16
    adc r3
    sta r1
    lda r3+1
    adc r1+1
    sta r1+1
    php
	lda r4    ;mov	r1, -(r4)	; to lower half tbl
    sec
    sbc #2
    sta r4
    bcs .l3

    dec r4+1
.l3:lda r1
    sta (r4),y
    lda r1+1
    iny
    sta (r4),y
    dey
    plp
	pla      ;mov	(r6)+, r2
    sta r2+1
    pla
    sta r2
    bcs mandel		; exit on overflow

	inc r2   ;inc	r2
    bne sqrloop

    inc r2+1
	bne	sqrloop

mandel:
         lda #0
         sta ti
         sta ti+1
         sta ti+2
         ldx #<ti
         ldy #>ti
         lda #2
         jsr OSWORD
    lda #$41
    sta .m1hi
    lda #$7f
    sta .m2hi
    ldy #$f8
    sty alo
    lda dy
    lsr
    sta r5+1
    lda dy+1
    ror
    sta r5    ;r5 = 128*dy
.mloop0:
.x0lo = * + 1
    lda #<ix0
    sta r4
.x0hi = * + 1
    lda #>ix0
    sta r4+1  ;mov	#x0, r4
.mloop2:
    clc  
    lda r4
    adc dx
    sta r4
    sta r0
    lda r4+1
    adc dx+1
    sta r4+1      ;add	@#dxa, r4
    sta r0+1      ;mov	r4, r0
.niter = * + 1
    lda #initer   
    sta r2        ;mov	#niter, r2
	lda r5
    sta r1
    lda r5+1
    sta r1+1      ;mov	r5, r1
.loc1:
    clc
    lda r1+1
    adc #>sqrbase  ;sets C=0
    sta tmp+1
    lda r1
    and #$fe
    tay
    lda (tmp),y
    sta r3
    iny
    lda (tmp),y
    sta r3+1         ;mov	sqr(r1), r3
    lda r0
    adc r1    ;C=0
    sta r1
    lda r0+1
    adc r1+1
    sta r1+1      ;add	r0, r1
    clc
    lda r0+1
    adc #>sqrbase   ;sets C=0
    sta tmp+1
    lda r0
    and #$fe
    tay
    iny
    lda (tmp),y   ;y=1
    tax
    dey
    lda (tmp),y   ;mov	sqr(r0), r0
    adc r3  ;C=0
    sta r0
    txa
    adc r3+1
    sta r0+1      ;add	r3, r0
    cmp #8
    bcs .loc2

    lda r1+1
    adc #>sqrbase   ;C=0, sets C=0
    sta tmp+1
    lda r1
    and #$fe
    tay
    iny
    lda (tmp),y
    tax
    dey
    lda (tmp),y     ;mov sqr(r1), r1
    adc r5   ;C=0
    tay
    txa
    adc r5+1
    tax        ;add	r5, r1
    sec
    tya
    sbc r0
    sta r1
    txa
    sbc r0+1
    sta r1+1     ;sub	r0, r1
	sec
    lda r0
    sbc r3
    tax
    lda r0+1
    sbc r3+1
    tay        ;sub	r3, r0
	;sec   ;it seems, C=1 is always here
    txa
    sbc r3
    tax
    tya
    sbc r3+1
    tay        ;sub	r3, r0
	clc
    txa
    adc r4
    sta r0
    tya
    adc r4+1
    sta r0+1       ;add	r4, r0
    dec r2
    ;bne .loc1
	beq .loc2
    jmp .loc1       ;sob	r2, 1$
.loc2:
    lda r2
    and #7   ;color index
    tay
.xtoggle = * + 1
    lda #0
    eor #1
    sta .xtoggle
    beq .loc8

    lda (zpat1),y
    sta .tcolor1
    lda (zpat2),y
    sta .tcolor2
    jmp .mloop2

.loc8:
.tcolor1 = * + 1
    lda #0
    lsr
    lsr
    ora (zpat1),y
    ldx alo
.m1hi = * + 2
.m1lo = * + 1
    sta $4100,x
.tcolor2 = * + 1
    lda #0
    lsr
    lsr
    ora (zpat2),y
.m2hi = * + 2
.m2lo = * + 1
    sta $7f07,x
    txa
    sec
    sbc #8
    sta alo
    bcs .loop2t

    lda .m1hi
    and #1
    beq .loc6

    dec .m1hi
    dec .m2hi
    bne .loop2t ;=jmp
.loc6:
    inc .m1hi
    inc .m2hi
    inc .m1lo
    lda .m1lo
    and #7
    beq .loc5

    dec .m2lo
    bcc .updr5  ;=jmp
.loc5:
    sta .m1lo   ;A=0
    lda #7
    sta .m2lo
    inc .m1hi
    inc .m1hi
    dec .m2hi
    dec .m2hi
.updr5:
    lda zpat1
    ldx zpat2
    sta zpat2
    stx zpat1
    sec
    lda r5
    sbc dy
    sta r5
    lda r5+1
    sbc dy+1
    sta r5+1    ;sub	@#dya, r5
	beq .loc7
.loop0t:
    jmp .mloop0
.loop2t:
    jmp .mloop2
.loc7:
    lda r5
    bne .loop0t  ;bgt	loop0

    clc
    lda .x0lo
    adc mx
    sta .x0lo
    lda .x0hi
    adc mx+1
    sta .x0hi     ;add	@#mxa, @#x0a
    ldx #4
.loc4:
    clc
    lda #<(sqrbase+sf4)
    adc dx,x
    and #$fe
    tay
    lda #>(sqrbase+sf4)
    adc dx+1,x
    sta tmp+1
    iny
    lda (tmp),y
    pha
    dey
    lda (tmp),y
    pha        ;mov	sqr+sf4(r2), (r1)
    lda #<(sqrbase-sf4)
    clc
    adc dx,x
    and #$fe
    tay
    lda #>(sqrbase-sf4)
    adc dx+1,x      ;sets C=0
    sta tmp+1
    pla
    sec
    sbc (tmp),y
    sta dx,x
    iny
    pla
    sbc (tmp),y
    sta dx+1,x
    dex
    dex   ;sub	sqr-sf4(r2), (r1)+
    bpl .loc4  ;sob	r0, 4$

    inc	.niter
         ldx #<ti
         ldy #>ti
         lda #1
         jsr OSWORD
    jsr OSRDCH
    and #$df
    cmp #"T"
    beq *+5
    jmp mandel

   lda #31   ;set the cursor position
   jsr OSWRCH
   lda #16    ;x
   jsr OSWRCH
   lda #6     ;y
   jsr OSWRCH
    lda .niter
    sec
    sbc #7
    tax
    lda #0
    jsr pr0000
    lda #" "
    jsr OSWRCH
    lda ti
         sta dividend
         lda ti+1
         sta dividend+1
         lda ti+2
         sta dividend+2
         lda ti+3
         sta dividend+3
         lda #0
         sta divisor+1
         lda #100
         sta divisor
         lda dividend+3   ;dividend = quotient
         jsr div32x16w
         ldx quotient
         lda quotient+1
         jsr pr0000
         lda #"."
         jsr OSWRCH
         lda remainder  ;*100,*5
         ldx remainder+1
         asl
         rol remainder+1
         asl
         rol remainder+1
         adc remainder
         sta remainder
         txa
         adc remainder+1
         sta remainder+1

         lda remainder  ;*5
         ldx remainder+1
         asl
         rol remainder+1
         asl
         rol remainder+1
         adc remainder
         sta remainder
         txa
         adc remainder+1
         sta remainder+1

         lda remainder  ;*4
         asl
         rol remainder+1
         asl
         rol remainder+1
         ;sta remainder
         tax

         lda remainder+1
         ;ldx remainder
         jsr pr0000
    lda #30  ;hide cursor
    jsr OSWRCH
    jsr OSRDCH
    jmp mandel

dx:  	word idx
dy:	    word idy
mx:     word imx

;    align 4  ;??
pat1:   byte 0, $88, $80, 8, $84, $cc, $c0, $c  ;pat1 & pat2 must be on the same page
pat2:   byte 0, $44,   4, 4, $44, $cc, $c0, $c
; black, white-black, yellow-black, red-black, yellow-red, white, yellow, red
; black, black-white, black-yellow, black-red, black-white, white, yellow, red

ti byte 0,0,0,0,0

pr0000:  sta d+2
         lda #<1000
         sta d
         lda #>1000
         sta d+1
         jsr pr0
         lda #100
         sta d
         lda #0
         sta d+1
         jsr pr0
         lda #10
         sta d
         jsr pr0
         txa
         tay
prd      tya
         eor #$30
         jmp OSWRCH

pr0      ldy #255
prn      iny
         lda d+2
         cmp d+1
         bcc prd
         bne prc

         cpx d
         bcc prd

prc      txa
         sbc d
         tax
         lda d+2
         sbc d+1
         sta d+2
         bcs prn


div32x16w        ;dividend+2 < divisor, divisor < $8000
        ;;lda dividend+3
        ldy #16

.l3      asl dividend
        rol dividend+1
        rol dividend+2
	rol
        ;bcs .l2   ;for divisor>$7fff

        cmp divisor+1
        bcc .l1
        bne .l2

        ldx dividend+2
        cpx divisor
        bcc .l1

.l2     tax
        lda dividend+2
        sbc divisor
        sta dividend+2
        txa
        sbc divisor+1
	inc quotient
.l1     dey
        bne .l3

        sta remainder+1
        lda dividend+2
        sta remainder
        ;lda #0
        ;sta dividend+2
	;sta dividend+3
	rts

msg     byte "**********************************",13
        byte "* Superfast Mandelbrot generator *",13
        byte "*     4 colors + textures        *",13
        byte "**********************************",13
        byte "The original version was published for",13
        byte "the BK0011 in 2021 by Stanislav",13
        byte "Maslovski.",13
        byte "This BBC Micro port was created by",13
        byte "Litwr, 2021.",13
        byte "The T-key gives us timings",0

init:
   lda #>msg
   sta r0+1
   lda #<msg
   sta r0
   ldy #0
nchar:
   lda (r0),y
   beq msgend

   iny
   bne *+4
   inc r0+1
   jsr OSWRCH
   jmp nchar
msgend:
   jsr OSRDCH
   lda #>pat1
   sta zpat1+1
   ;lda #>pat2   ;pat1 & pat2 are on the same page
   sta zpat2+1
   lda #<pat2
   sta zpat2
   lda #<pat1
   sta zpat1
   lda #22
   jsr OSWRCH
   lda #1     ;mode 1
   jsr OSWRCH
   sei
   ldx #1
   lda #$40
   stx $fe00
   sta $fe01
;   inx
;   lda #$60
;   stx $fe00
;   sta $fe01
   ldx #12
   lda #$8    ;$4000 graph mem start
   stx $fe00
   sta $fe01
   inx
   lda #0
   stx $fe00
   sta $fe01
   cli
   rts

