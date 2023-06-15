;for vasm assembler, oldstyle syntax
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;Thanks to reddie for some help with optimization
;
;512x256 Mandelbrot for the Commodore 128 (VDC), 2 colors are used to form 4x1 bricks to simulates 8 colors

APORT = $D600
DPORT = $D601
TIMER = $A0

BSOUT = $FFD2    ;print char in AC
PRIMM = $FA17
GETIN = $FFE4

NOCALC = 0

sqrbase = $3900 ;must be $xx00, it takes area $2250-$4fb0
initer	= 7
idx	=	-36       ;-.0703125
idy	=	18        ;.03515625, 1 = 1/512
ix0	=	-62*idx
imx	=	10*idx		; x move
sf4	=	436/4		; sf/4

r0 = $50
r1 = $fb ;$fc
r2 = $41
r3 = $43
t = $66 ;$67
tmp = $64 ;$65
zpat1 = $ac   ;$ab
zpat2 = $87   ;$88

dx = $5d
dy = $5f
mx = $61

d = $41     ;..$44
divisor = $41     ;..$44
dividend = $64	  ;..$67
remainder = $50   ;..$51
quotient = dividend ;save memory by reusing divident to store the quotient

         * = $1c01
   byte $b,$1c,$a,0,$9e
   byte start/1000+48,start%1000/100+48,start%100/10+48,start%10+48
   byte 0,0,0

start: JSR PRIMM
       byte 14,"**************************************",13
       byte "*  sUPERFAST mANDELBROT GENERATOR V1 *",13
       byte "**************************************",13
       byte "tHE ORIGINAL VERSION WAS PUBLISHED FOR",13
       byte "THE bk0011 IN 2021 BY sTANISLAV",13
       byte "mASLOVSKI.",13
       byte "tHIS cOMMODORE 128 PORT WAS CREATED",13,0
       JSR PRIMM
       byte "BY LITWR, 2023.",13
       byte "tHE t-KEY GIVES US TIMINGS",13
       byte 'pRESS b TO ENTER BENCHMARK MODE',0
       JSR waitk
       sta benchmark

   lda #2
   sta $ff00  ;$4000-$7fff to RAM

   ldx #25    ;VDC 512x256 bw graphics
   lda #$87
   jsr setr

   ldx #12
   lda #0
   jsr setr
   
   inx
   jsr setr

   ldx #1
   lda #64
   jsr setr

   ldx #2
   lda #94    ;102
   jsr setr

   ldx #4
   lda #39
   jsr setr

   ldx #6
   lda #32
   jsr setr

   ldx #7
   lda #36
   jsr setr
   
   lda #>pat1
   sta zpat1+1
   ;lda #>pat2   ;pat1 & pat2 are on the same page
   sta zpat2+1
   lda #<pat2
   sta zpat2
   lda #<pat1
   sta zpat1

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

         jsr fast
         lda #0
         sta tmp
         sta TIMER+2
         sta TIMER+1
         sta TIMER
mandel1:
    lda #0
    sta .m1hi
    lda #$3f
    sta .m2hi
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
    lsr
    lsr
    ora (zpat1),y
    pha
    ldx #18
.m1hi = * + 1
    lda #0
    jsr setr
    inx
.m1lo = * + 1
    lda #$3f
    jsr setr
    ldx #31
    pla
    jsr setr
.tcolor2 = * + 1
    lda #0
    lsr
    lsr
    lsr
    lsr
    ora (zpat2),y
    pha
    ldx #18
.m2hi = * + 1
    lda #0
    jsr setr
    inx
.m2lo = * + 1
    lda #$ff
    jsr setr
    ldx #31
    pla
    jsr setr

    ldx .m2lo
    bne .lt1

    dec .m2hi
.lt1 dex
    stx .m2lo
    ldx .m1lo
    txa
    dex
    stx .m1lo
    and #$3f
    bne .loop2t

    inx
    txa
    clc
    adc #127
    sta .m1lo
    lda .m1hi
    adc #0
    sta .m1hi
.updr5:
    lda zpat1
    ldx zpat2
    sta zpat2
    stx zpat1
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
.loop2t:
    jmp .mloop2
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
         sei
         lda TIMER
         sta ti+2
         lda TIMER+1
         sta ti+1
         LDA TIMER+2
         cli
         sta ti

    lda benchmark
    cmp #"B"
    beq .loc9

    jsr slow
    jsr waitk
    and #$df
    cmp #"Q"
    bne .noq
.exit:
    lda #0
    sta $ff00
   rts
.noq:
    cmp #"T"
    beq *+5
    jmp mandel
.loc9:
   lda #13
   jsr BSOUT
    lda .niter
    sec
    sbc #7
    tax
    lda #0
    jsr pr000
    lda #" "
    jsr BSOUT
    lda ti
         sta dividend
         lda ti+1
         sta dividend+1
         lda ti+2
         sta dividend+2
         lda #60
         sta divisor
         lda #0
         sta dividend+3
         sta divisor+1
         jsr div32x16w
         ldx quotient
         lda quotient+1
         jsr pr000
         lda #"."
         jsr BSOUT
         lda remainder  ;*5
         asl
         rol remainder+1
         asl
         rol remainder+1
         adc remainder
         sta remainder
         lda #0
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
         
         lda remainder  ;*2
         asl
         rol remainder+1
         sta dividend
         lda remainder+1
         sta dividend+1
         
         lda #3
         sta divisor
         lda #0   ;dividend = quotient
         sta dividend+2
         jsr div32x16w
         ldx quotient
         ldy quotient+1
         lda remainder
         cmp #2
         bcs .l8
         
         inx
         bne .l8

         iny
.l8:     tya
         jsr pr000
    jsr slow
    jsr waitk
    and #$df
    cmp #"Q"
    beq .exit1
    jmp mandel
.exit1:
    jmp .exit

pat1: byte	0,$e0,$d0,$c0,$a0,$50,$10,$f0  ;pat1 & pat2 must be on the same page
pat2: byte	0,$b0,$60,$30,$10,$a0,$e0,$f0
;pat1: byte	15,1,2, 3, 5,10,14,0   ;inv
;pat2: byte	15,4,9,12,14, 5, 1,0

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
         jmp BSOUT

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

ti byte 0,0,0
benchmark byte 0
bcount byte 0

setr:
     stx APORT
.l1: bit APORT
     bpl .l1

     sta DPORT
     rts

waitk:
      jsr GETIN
      ora #0
      beq waitk
      rts

slow:
      lda #0
      sta $d030
      lda $d011
      and #$7f
      ora #$10
      sta $d011
      rts

fast:
      lda $d011
      and #$6f   ;ef?
      sta $d011
      lda #1
      sta $d030
      rts
 
