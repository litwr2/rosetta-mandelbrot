;for vasm assembler, oldstyle syntax
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;The next code was made by litwr in 2021
;Thanks to reddie for some help with optimization
;
;128x256 Mandelbrot for the Commodore +4, 4 color mode simulates 8 colors using flashing

; text data for 32 lines:
;    $a000 - $a3e7,  $a400 - a7e7  1000 chars
;    $1be8 - 1bff, $1fe8 - 1fff    24 chars
;    $1800 - 18ff, $1c00 - 1cff   256 chars
; graph mc data for 32 lines: A1=$2000, B1=$4000
;    A+$0000 - 1f3f  8000 bytes
;    B+$1f40 - 1fff   192 bytes
;    B+$0000 - 07ff  2048 bytes
;colors = $800
;bm1 = $2000/$6000
;bm2 = $4000/$8000
;rows: $2020-$21DF ... $3E20-$3F1F  $5F60-$5FFF, $4000-$405F  $40A0-$419F ...

BSOUT = $FFD2
JPRIMM = $FF4F

sqrbase = $BF00 ;must be $xx00
initer	= 7
idx	=	-36       ;-.0703125
idy	=	18        ;.03515625, 1 = 1/512
ix0	=	-62*idx
imx	=	10*idx		; x move
sf4	=	436/4		; sf/4

r0 = $d0
r1 = $d2
r2 = $d4
r3 = $d6
r4 = $d8
r5 = $da
tmp = $dc
vy = $de
vx = $df
alo = $d5
a1 = $e0

d = $d0    ;..$d3
divisor = $d4     ;..$d7
dividend = $de	  ;..$e1
remainder = $d8   ;$d9
quotient = dividend ;save memory by reusing divident to store the quotient

color0 = 1    ;black
color1 = $3d  ;blue
color2 = $35  ;green
color3 = $32  ;red
   ;$ee - border

   org $1001
   byte $b,$10,$a,0,$9e,"4112",0,0,0

   org $1010
       JSR JPRIMM
       byte 9,14,"**************************************",13
       byte "* sUPERFAST mANDELBROT GENERATOR V2F *",13
       byte "**************************************",13
       byte "tHE ORIGINAL VERSION WAS PUBLISHED FOR",13
       byte "THE bk0011 IN 2021 BY sTANISLAV",13
       byte "mASLOVSKI.",13
       byte "tHIS cOMMODORE+4 PORT WAS CREATED BY",13,0
       JSR JPRIMM
       byte "LITWR, 2021.",13
       byte "tHE t-KEY GIVES US TIMINGS",0
       JSR getkey

       LDA #$55
       LDY #0
       LDX #$20
loopk: STX loopi+2
loopi: STA $2000,Y
       INY
       BNE loopi

       INX
       CPX #$C0
       BNE loopk

       LDA #0
       STA a1
       LDA #$A0   ;color ram
       STA a1+1
       LDY #0
       STY vy  ;y
loop1: LDX #32
       CPX vy
       BNE loop2
       BEQ finish

loop2: STY vx  ;x
loop2c:LDX vx
       CPX #40
       BNE loop2d

       INC vy
       BNE loop1

loop2d:LDX #25
       CPX vy
       BNE .l1

       DEX
       CPX vx
       BNE .l1

       LDA a1+1    ;C=1
       ADC #$FB    ;$A400 -> $A000
       STA a1+1
.fill1:LDA #(color2&$f0)|(color1&$f0)>>4    ;lum
       STA (a1),Y
       LDA a1+1
       PHA
       EOR #4
       STA a1+1
       LDA #(color2&$f)|(color1&$f)<<4
       STA (a1),Y
       PLA
       STA a1+1
       INC a1
       BNE .l4

       INC a1+1
.l4:   INC vx
       BNE loop2d  ;=jmp

.l1:   LDX #25
       CPX vy
       BNE .l2

       LDX vx
       BNE .l2

       LDA a1+1    ;C=1
       ADC #$77     ;$BE8 -> $1BE8
       STA a1+1
.l2:   LDX vx
       BNE .l3

.fill0:LDX #4
.l0:   LDA #6
       STA (a1),Y
       LDA a1+1
       PHA
       EOR #4
       STA a1+1
       LDA #$e0
       STA (a1),Y
       PLA
       STA a1+1
       INC a1
       BNE .l5

       INC a1+1
.l5:   INC vx
       DEX
       BNE .l0
       BEQ loop2c  ;=JMP

.l3:   LDX #36
       CPX vx
       BEQ .fill0
       BNE .fill1  ;=JMP

finish:LDA #color3
       STA $FF16

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
    sei
    STA $FF3F
    LDX #$3B
    STX $FF06
    LDX #$18
    STX $FF07
    LDA #color0
    STA $FF15
    JSR iniirq
    lda $a5  ;timer
    sta ti
    lda $a4
    sta ti+1
    lda $a3
    sta ti+2
    cli

    lda #$20
    sta .m1hi
    lda #$60
    sta .m2hi
    lda #$46
    sta .m3hi
    lda #$86
    sta .m4hi
    ldy #$f8
    sty alo

    lda dy
    lsr
    sta r5+1
    lda dy+1
    ror
    sta r5    ;r5 = 200*dy
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
    tax           ;mov	r4, r0
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
    txa
    adc r1+1
    sta r1+1      ;add	r0, r1
    clc
    txa
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
    tax       ;add	r4, r0
    dec r2
    ;bne .loc1
	beq .loc2
    jmp .loc1       ;sob	r2, 1$
.loc2:
    lda r2
    and #7   ;color index
    tay
.xtoggle = * + 1
    ldx #4
    dex
    beq .loc8

    stx .xtoggle
.tcolor1 = * + 1
    lda #0
    lsr
    lsr
    ora pat1,y
    sta .tcolor1
.tcolor2 = * + 1
    lda #0
    lsr
    lsr
    ora pat2,y
    sta .tcolor2
    jmp .mloop2

.loc8:
    ldx #4
    stx .xtoggle
    lda .tcolor2
    lsr
    lsr
    ora pat2,y
    pha
    lda .tcolor1
    lsr
    lsr
    ora pat1,y
    ldx alo
.m1hi = * + 2
.m1lo = * + 1
    sta $2020,x
.m3hi = * + 2
.m3lo = * + 1
    sta $46e7,x
    pla
.m2hi = * + 2
.m2lo = * + 1
    sta $6020,x
.m4hi = * + 2
.m4lo = * + 1
    sta $86e7,x
    txa
    sec
    sbc #8
    sta alo
    bcs .loc7

    inc .m1lo
    lda .m1lo
    and #7
    beq .loc5

    inc .m2lo
    dec .m3lo
    dec .m4lo
    lda .m3hi
    cmp #$5f
    bne .updr5

    lda #$3f
    sta .m3hi
    lda #$7f
    sta .m4hi 
    bne .updr5  ;=jmp
.loc5:
    lda .m1lo
    clc
    adc #$38
    sta .m1lo
    lda .m1hi
    adc #1
    sta .m1hi
    lda .m2lo
    adc #$39  ;C=0
    sta .m2lo
    lda .m2hi
    adc #1
    sta .m2hi
    lda .m3lo
    sbc #$38  ;C=0
    sta .m3lo
    lda .m3hi
    sbc #1
    sta .m3hi
    tay
    lda .m4lo
    sbc #$39  ;C=1
    sta .m4lo
    lda .m4hi
    sbc #1
    sta .m4hi
    cpy #$5e
    bne .updr5

    lda #$3e
    sta .m3hi
    lda #$7e
    sta .m4hi
    bne .updr5  ;=jmp
.loc7:
    lda .m3hi
    cmp #$3f
    bne .loop2t

    lda alo
    cmp #$98
    bne .loop2t

    lda #$5f
    sta .m3hi
    lda #$9f
    sta .m4hi
.loop2t:
    jmp .mloop2
.loop0t:
    jmp .mloop0
.updr5:
    sec
    lda r5
    sbc dy
    sta r5
    lda r5+1
    sbc dy+1
    sta r5+1    ;sub	@#dya, r5
	bne .loop0t

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
    sei
    sec
    lda $a5
    sbc ti
    sta ti
    lda $a4
    sbc ti+1
    sta ti+1
    lda $a3
    sbc ti+2
    sta ti+2
    cli
    jsr getkey
    cpx #$c0  ;T-key?
    beq *+5
    jmp .mandel

    sei
    sta $ff3e
    lda #8
    sta $ff14
    STA $FF07
    lda #$F1
    sta $ff15
    LDA #$1B
    STA $FF06
    LDA #$C4
    STA $FF12
    cli
    lda #147    ;clear screen
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
         lda #0
         sta dividend+3
         sta divisor+1
         lda #50
         sta divisor
         lda dividend+3   ;dividend = quotient
         jsr div32x16w
         ldx quotient
         lda quotient+1
         jsr pr000
         lda #"."
         jsr BSOUT
         lda remainder  ;*20,*5
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
         jsr pr000
    ldy #0
    ldx #0
.delay
    inx
    bne .delay
    iny
    bne .delay

    jsr getkey
.mandel
	jmp	mandel

dx:  	word idx
dy:	    word idy
mx:     word imx
pat1:   byte 0,2*64,0   ,1*64,3*64,1*64,2*64,3*64
pat2:   byte 0,1*64,2*64,3*64,2*64,1*64,2*64,3*64
ti:     byte 0,0,0

iniirq:LDA #$F8
       STA irqe3.cnt
       LDA #$20
       LDA irqe2.bma
       LDA #$18
       LDA irqe3.bma
       LDA #>irqe1
       STA $FFFF
comm1: LDA #<irqe1
       STA $FFFE
       LDA #$1C     ;$11c = 284
       STA $FF0B
       LDA #$A3		;1 - hi byte, raster irq only
       STA $FF0A
       RTS

div32x16w:        ;dividend+2 < divisor, divisor < $8000
        ;;lda dividend+3
        ldy #16
.l3     asl dividend
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

.l2:    tax
        lda dividend+2
        sbc divisor
        sta dividend+2
        txa
        sbc divisor+1
	    inc quotient
.l1:    dey
        bne .l3

        sta remainder+1
        lda dividend+2
        sta remainder
        ;lda #0
        ;sta dividend+2
	;sta dividend+3
	rts

getkey:
   ldx #$7f
.waitkey:
   stx $fd30
   stx $ff08
   ldx $ff08
   inx
   beq .waitkey
   rts

pr000:   ;prints ac:xr < 10000
         sta d+2
         lda #100
         sta d
         lda #0
         sta d+1
         jsr .pr0
         lda #10
         sta d
         jsr .pr0
         txa
         tay
.prd:    tya
         eor #$30
         jmp BSOUT

.pr0:    ldy #255
.prn:    iny
         lda d+2
         cmp d+1
         bcc .prd
         bne .prc

         cpx d
         bcc .prd

.prc     txa
         sbc d
         tax
         lda d+2
         sbc d+1
         sta d+2
         bcs .prn

    align 8
irqe1  STA .sa      ;@284
       LDA #$36
       STA $FF1D    ;310
       LDA #$CA		;202
       STA $FF0B
       LDA #$A2		;0 - hi byte
       STA $FF0A
       LDA #<irqe2
       STA $FFFE
.sa = * + 1
       LDA #0
irqe0  INC $FF09
       RTI

irqe2  STA .sa      ;@202
       LDA #$92
       STA $FF1D
       LDA #$CE		;206
       STA $FF0B
       LDA #<irqe3
       STA $FFFE
       LDA #$18     ;$1800
       STA $FF14
       INC $FF09
.bma = * + 1
       LDA #$20     ;$4000
    pha  ;a delay
    pla
    pha
    pla
    pha
    pla
    pha
    pla
       STA $FF12
.sa = * + 1
       LDA #0
       RTI

irqe3  STA .sa    ;@206
       LDA #$EC
       STA $FF1D  ;236
       JSR comm1  
       INC $FF09
       LDA #$A0    ;$800
       STA $FF14
.cnt = * + 1
    LDA #$f8
    CLC
    ADC #$8
    STA .cnt
    BNE .l1

       LDA irqe2.bma
       EOR #$30     ;$4000/$8000 toggle
       STA irqe2.bma
       LDA .bma
       EOR #$10   ;$2000/$6000 toggle
       STA .bma
       ;LDA $FF15
       ;EOR #$30
       ;STA $FF15
.l1:
.bma = * + 1
       LDA #$18    ;$2000
       STA $FF12
       inc $a5
       bne .l2

       inc $a4
       bne .l2

       inc $a3
.l2:
.sa = * + 1
       LDA #0
       RTI

