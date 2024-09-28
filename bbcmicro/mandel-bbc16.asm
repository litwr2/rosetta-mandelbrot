;for vasm assembler, oldstyle syntax
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;Thanks to reddie for some help with optimization
;
;128x256 Mandelbrot for the BBC Micro, 16 color mode

OSWRCH = $FFE3
OSRDCH = $FFE0
OSWORD = $FFF1
OSBYTE = $FFF4

NOCALC = 0

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
;r4 = $78
;r5 = $7a
t = $7a
tmp = $7c

dx = $7e
dy = $80
mx = $82

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

    ldx #0
    stx dy+1
    lda #idx
    sta dx
    lda #idy
    sta dy
    dex
    stx dx+1
    lda #<imx
    sta mx
    lda #>imx
    sta mx+1

fillsqr:
    inx
    txa
    tay
.loop:
    sta r0,x
    inx
    cpx #6
    bne .loop

    lda #>sqrbase
    ;ldx #<sqrbase   ;=0
    sta tmp+1
    sty tmp
    sta t+1
    sty t
sqrloop:
    lda r1     ;mov	r1, (r5)+	; to upper half tbl
    sta (t),y
    lda r1+1
    iny
    sta (t),y
    dey
    clc
    lda t
    adc #2
    sta t
    bcc .l1

    inc t+1
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
	lda tmp    ;mov	r1, -(r4)	; to lower half tbl
    sec
    sbc #2
    sta tmp
    bcs .l3

    dec tmp+1
.l3:lda r1
    sta (tmp),y
    lda r1+1
    iny
    sta (tmp),y
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
         lda #16
         sta bcount
         lda #0
         sta tmp
         sta ti
         sta ti+1
         sta ti+2
         ldx #<ti
         ldy #>ti
         lda #2
         jsr OSWORD
mandel1:
    lda #$41
    sta .m1hi
    lda #$7f
    sta .m2hi
    ldy #$f8
    sty alo
    lda dy
    lsr
    sta r5hi
    lda #0
    ror
    sta r5lo    ;r5 = 128*dy
.mloop0:
  if NOCALC=0
.x0lo = * + 1
    lda #<ix0
    sta r4lo
.x0hi = * + 1
    lda #>ix0
    sta r4hi  ;mov	#x0, r4
  endif
.mloop2:
  if NOCALC=0
    clc
    lda r4lo
    adc dx
    sta r4lo
    sta r0
    lda r4hi
    adc #$ff
    sta r4hi      ;add	@#dxa, r4
    sta r0+1           ;mov	r4, r0
  endif
.niter = * + 1
    lda #initer
  if NOCALC=0
    sta r2        ;mov	#niter, r2
    lda r5lo
    sta r1
    lda r5hi
    sta r1+1      ;mov	r5, r1
.loc1:
    clc
    lda r1+1
    adc #>sqrbase
    sta tmp+1
    lda r1
    and #$fe
    tay
    lda (tmp),y
    sta r3
    iny
    lda (tmp),y
    sta r3+1         ;mov	sqr(r1), r3

    lda r0+1
    clc
    adc #>sqrbase  ;C=0
    sta tmp+1
    lda r0
    ora #1
    tay
    lda (tmp),y   ;y=1
    tax
    dey
    lda (tmp),y   ;mov	sqr(r0), r0
    clc
    adc r3
    sta t
    txa
    adc r3+1

    cmp #8
    bcs .loc2

    sta t+1      ;add	r3, r0
    lda r0
    adc r1    ;C=0
    tax
    lda r0+1
    adc r1+1
    ;sta r1+1      ;add	r0, r1
    ;lda r1+1
    clc
    adc #>sqrbase
    sta tmp+1
    txa
    and #$fe
    tay
    lda (tmp),y
    clc
  endif
r5lo = * + 1
    adc #0   ;C=0
  if NOCALC=0
    tax
    iny
    lda (tmp),y     ;mov sqr(r1), r1
  endif
r5hi = * + 1
    adc #0
  if NOCALC=0
    tay        ;add	r5, r1

    sec
    txa
    sbc t
    sta r1
    tya
    sbc t+1
    sta r1+1     ;sub	r0, r1
    sec
    lda t
    sbc r3
    tax
    lda t+1
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
r4lo = * + 1
    adc #0
    sta r0
    tya
r4hi = * + 1
    adc #0
    sta r0+1     ;add	r4, r0
    dec r2
    ;bne .loc1
	beq .loc2
    jmp .loc1       ;sob	r2, 1$
  endif
.loc2:
    lda r2
    and #15   ;color index
    tay
.xtoggle = * + 1
    lda #0
    eor #1
    sta .xtoggle
    beq .loc8

    lda pat,y
    sta .tcolor
    jmp .mloop2

.loc8:
.tcolor = * + 1
    lda #0
    lsr
    ora pat,y
    ldx alo
.m1hi = * + 2
.m1lo = * + 1
    sta $4100,x
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
.loop2t:
    jmp .mloop2
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
    sec
    lda r5lo
    sbc dy
    sta r5lo
    lda r5hi
    sbc #0
    sta r5hi    ;sub	@#dya, r5
	beq .loc7
.loop0t:
    jmp .mloop0
.loc7:
    lda r5lo
    bne .loop0t  ;bgt	loop0
  if NOCALC=0
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
  endif
    ldx benchmark
    cpx #"B"
    bne .loc3

    dec bcount
    beq .loc3
    jmp mandel1
.loc3:
         ldx #<ti
         ldy #>ti
         lda #1
         jsr OSWORD

    lda benchmark
    cmp #"B"
    beq .loc9

    jsr OSRDCH
    and #$df
    cmp #"Q"
    bne .noq
.exit:
   lda #22
   jsr OSWRCH
   lda #7     ;mode 7
   jmp OSWRCH
.noq:
    cmp #"T"
    beq *+5
    jmp mandel
.loc9:
   lda #31   ;set the cursor position
   jsr OSWRCH
   lda #8    ;x
   jsr OSWRCH
   lda #6     ;y
   jsr OSWRCH
    lda .niter
    sec
    sbc #7
    tax
    lda #0
    jsr pr000
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
         jsr pr000
         lda #"."
         jsr OSWRCH
         lda remainder  ;*10,*5
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

         lda remainder  ;*2
         asl
         rol remainder+1
         ;sta remainder
         tax

         lda remainder+1
         ;ldx remainder
         jsr pr000
    lda #30  ;hide cursor
    jsr OSWRCH
    jsr OSRDCH
    and #$df
    cmp #"Q"
    beq .exit1
    jmp mandel
.exit1:
    jmp .exit

pat     byte   0, $8a, $88, $82, $aa, $20, 8, 2 
        byte $a0, $a,  $22, $28, $a2, $a8, $2a, $80

ti byte 0,0,0,0,0

pr000:   sta d+2
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

benchmark byte 0
bcount byte 0

msg     byte "**********************************",13
        byte "* Superfast Mandelbrot generator *",13
        byte "*         16 colors, v7          *",13
        byte "**********************************",13
        byte "The original version was published for",13
        byte "the BK0011 in 2021 by Stanislav",13
        byte "Maslovski.",13
        byte "This BBC Micro port was created by",13
        byte "Litwr, 2021-24.",13
        byte "The T-key gives us timings.",13
        byte "Use the Q-key to quit",13
        byte "Press B to enter benchmark mode",255

emsg    byte 13,"Can't use CoPro",13,255

idata   byte 22,2    ;mode 2
        byte 23,0,1,64,0,0,0,0,0,0   ;64-chars wide
        byte 23,0,2,90,0,0,0,0,0,0
        byte 23,0,12,8,0,0,0,0,0,0   ;Gr.mem starts at $4000
        byte 255   ;the end

init:
   lda #16
   ldx #0
   jsr OSBYTE    ;no ADC
   ldx #>msg
   lda #<msg
   jsr .send
   jsr OSRDCH
   and #$df
   sta benchmark

   lda #$ea
   ldx #0
   ldy #$ff
   jsr OSBYTE
   inx
   bne .cont
   
   ldx #>emsg
   lda #<emsg
   jsr .send
   pla
   pla
   rts
.cont
   ldx #>idata
   lda #<idata
.send
   stx r0+1
   sta r0
   ldy #0
.nbyte:
   lda (r0),y
   cmp #255
   beq .tend

   iny
   bne *+4
   inc r0+1
   jsr OSWRCH
   jmp .nbyte
.tend:
   rts

