;for vasm assembler, oldstyle syntax
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;The next code was made by litwr in 2021, 2022
;Thanks to reddie for some help with optimization
;
;160xN (fullscreen) Mandelbrot for the Commodore +4, 4 color mode simulates 8/16 colors using flashing/interlacing
;16 colors actually are 10 for interlacing
;interactive version

; text data for 32 lines:
;    $a000 - a3e7, $a400 - a7e7  1000 chars
;    $1be8 - 1bff, $1fe8 - 1fff    24 chars
;    $1800 - 18ff, $1c00 - 1cff   256 chars
; graph mc data for 32 lines: A1=$2000, B1=$4000
;    A+$0000 - 1f3f  8000 bytes
;    B+$0140 - 9ff  2240 bytes
;colors = $a000/$a800
;bm1 = A/B = $2000/$6000
;bm2 = A/B = $4000/$8000

BSOUT = $FFD2
JPRIMM = $FF4F

sqrbase = $C800 ;must be $xx00
colorpat1 = $1f60
colorpat2 = $1f70
HSize = 160
VSize = 280  ;must be a multiple of 8, up to 280 @vsize@

r0 = $d0
r1 = $d2
r2 = $d4
r3 = $d6
t = $da
tmp = $dc
alo = $d5

dx = $d8
dy = $d9

color0 = 1    ;black
color1 = $3d  ;blue
color2 = $35  ;green
color3 = $32  ;red
   ;$ee - border

       org $1001
       include mandel-i.inc

       org $1510
start: lda #$a0    ;@start@
       sta loopi2+2
       lda #$a8
       sta loopi2.t+2
       lda #$a4
       sta loopi3+2
       lda #$ac
       sta loopi3.t+2
       LDA #$55
       LDY #0
       LDX #$20
loopk: STX loopi+2
loopi: STA $2000,Y
       INY
       BNE loopi

       INX
       CPX #$a0
       BNE loopk

       LDA #(color2&$f0)|(color1&$f0)>>4    ;lum
       LDX #4
loopi2:STA $a000,Y
.t:    STA $a800,Y
       INY
       BNE loopi2

       inc loopi2+2
       inc .t+2
       dex
       BNE loopi2

       LDA #(color2&$f)|(color1&$f)<<4 ;color
       LDX #4
loopi3:STA $a400,Y
.t:    STA $ac00,Y
       INY
       BNE loopi3

       inc loopi3+2
       inc .t+2
       dex
       BNE loopi3

       LDA #color3
       STA $FF16

fillsqr:
    lda #0
    tay
    tax
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

.m1hi = $e3
.m1lo = $e2
.m2hi = $e5
.m2lo = $e4
.m3hi = $e7
.m3lo = $e6
.m4hi = $e1
.m4lo = $e0

    lda #0
    sta tmp

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

    lda #0
    sta .m1lo
    sta .m2lo
    lda #$20
    sta .m1hi
    lda #$60
    sta .m2hi
    lda #<($c7+(VSize-256)*40)   ;$c7 when VSize=256
    sta .m3lo
    sta .m4lo

    lda #>($48c7+(VSize-256)*40)   ;$48 when VSize=256
    sta .m3hi
    lda #>($88c7+(VSize-256)*40)   ;$88 when VSize=256
    sta .m4hi
    lda #1
    sta .ahi
    ldy #$38
    sty alo
.mloop0:
.x0lo = * + 1
    lda #0      ;@x0lo@
    sta r4lo
.x0hi = * + 1
    lda #0      ;@x0hi@
    sta r4hi  ;mov	#x0, r4
.mloop2:
    clc
    lda r4lo
    adc dx
    sta r4lo
    sta r0
    lda r4hi
    adc #$ff   ;dx+1
    sta r4hi      ;add	@#dxa, r4
    sta r0+1           ;mov	r4, r0
.niter = * + 1
    lda #0        ;@n@
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
r5lo = * + 1
    adc #0   ;C=0   @r5lo@
    tax 
    iny
    lda (tmp),y     ;mov sqr(r1), r1
r5hi = * + 1
    adc #0         ;@r5hi@
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
	sec   ;it seems, C=1 is always here??
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
.loc2:
    lda r2
    and #0   ;@color@ index
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
    ora colorpat1,y
    sta .tcolor1
.tcolor2 = * + 1
    lda #0
    lsr
    lsr
    ora colorpat2,y
    sta .tcolor2
.loc9:
    jmp .mloop2
.loc8:
    ldx #4
    stx .xtoggle
    lda .tcolor2
    lsr
    lsr
    ora colorpat2,y
    pha
    lda .tcolor1
    lsr
    lsr
    ora colorpat1,y
    ldy alo
.ahi = * + 1
    ldx #0
    beq .loc7

    cpy #$38
    bne .loc11

    ldx .m3hi
    cpx #$40
    bne .loc11

    ldx #$3e
    stx .m3hi
    ldx #$7e
    stx .m4hi
.loc11:
    inc .m1hi
    inc .m2hi
    inc .m3hi
    inc .m4hi
    sta (.m1lo),y
    sta (.m3lo),y
    pla
    sta (.m2lo),y
    sta (.m4lo),y
    dec .m1hi
    dec .m2hi
    dec .m3hi
    dec .m4hi
    tya
    sec
    sbc #8
    sta alo
    bcs *+5
    dec .ahi
    jmp .mloop2
.loc7:
    sta (.m1lo),y
    sta (.m3lo),y
    pla
    sta (.m2lo),y
    sta (.m4lo),y
    tya
    sec
    sbc #8
    sta alo
    bcs .loc9

    lda #1
    sta .ahi
    lda #$38
    sta alo
    inc .m1lo
    lda .m1lo
    and #7
    beq .loc5

    dec .m3lo
    dec .m4lo
    inc .m2lo
    bne .updr5
.loc5:
    lda .m1lo
    ;clc
    adc #$38  ;C=0
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
    lda .m4lo
    sbc #$39  ;C=1
    sta .m4lo
    lda .m4hi
    sbc #1
    sta .m4hi
.updr5:
    sec
    lda r5lo
    sbc dy
    sta r5lo
    lda r5hi
    sbc #0     ;dy+1
    sta r5hi    ;sub	@#dya, r5
	beq .loc10
.loop0t:
    jmp .mloop0
.loc10:
    lda r5lo
    bne .loop0t  ;bgt	loop0

    inc	.niter
    sec
    sei
    lda $a5
    sbc ti
    sta $d0
    lda $a4
    sbc ti+1
    sta $d1
    lda $a3
    cli
    sbc ti+2
    sta $d2
    jsr getkey
    ;sei
    sta $ff3e
    lda #$a2
    sta $ff0a
    lda #8
    sta $ff14
    STA $FF07
    lda #$F1
    sta $ff15
    LDA #$1B
    STA $FF06
    LDA #$C4
    STA $FF12
    ;cli

    ldy #0
    ldx #0
.delay
    inx
    bne .delay

    iny
    bne .delay

    sty $ef   ;clear kbd buffer
    rts

irqe1  pha      ;@284 when VSize=256
       LDA #$36
       STA $FF1D    ;310
       LDA #$CA		;202
       STA $FF0B
       LDA #$A2		;0 - hi byte
       STA $FF0A
       LDA #<irqe2
       STA $FFFE
       pla
irqe0  INC $FF09
       RTI

irqe2  pha      ;@202
       LDA #(402-VSize)   ;146 when VSize=256
       STA $FF1D
       LDA #206   	 ;206 when VSize=256
       STA $FF0B
       LDA #<irqe3
       STA $FFFE
       LDA #$A8     ;$A800
       STA $FF14
       INC $FF09

       stx .sx+1
.bma = * + 1
       LDA #$20
       ldx flash_st
       beq .l1

       nop
       bne .sx
.l1:
       EOR #$30     ;$4000/$8000 toggle
       STA .bma
.sx:   ldx #0    
       pha  ;a delay
       pla
       nop
       nop

       STA $FF12
       ;LDA #0
       STA $FF1A
       LDA #40
       STA $FF1B
       PLA
       RTI

irqe3  pha    ;@206
       LDA #(108+VSize/2)  ;236 when VSize=256
       STA $FF1D
       JSR comm1
       INC $FF09
       LDA #$A0    ;$A000
       STA $FF14
       lda flash_st
       php
       beq .l1
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
.l1:
.bma = * + 1
       LDA #$18    ;$2000
       STA $FF12
       plp
       bne .l3

       EOR #$10   ;$2000/$6000 toggle
       STA .bma
.l3:
       inc $a5   ;a3-a5 60Hz!
       bne .l2

       inc $a4
       bne .l2

       inc $a3
.l2:   PLA
       RTI

  assert *&0xff00==irqe1&0xff00, alignment error

getkey:
   ldx #$7f
.waitkey:
   stx $fd30
   stx $ff08
   ldx $ff08
   inx
   beq .waitkey
   rts

iniirq:lda flash_st
       beq .l1

       LDA #$F8
       STA irqe3.cnt
       LDX #$18
       LDA #$20
       bne .l2
.l1:
       LDA #$10
       LDX #$8
.l2:   STA irqe2.bma
       STX irqe3.bma
       LDA #>irqe1
       STA $FFFF
comm1: LDA #<irqe1
       STA $FFFE
       LDA #<(412-VSize/2)  ;284 when VSize=256
       STA $FF0B
       LDA #$A3		;1 - hi byte, raster irq only
       STA $FF0A
       RTS

pat1_2   byte 0,2*64   ;@coltab@
pat2_2   byte 0,2*64
pat1_4   byte 0,1*64,2*64,3*64
pat2_4   byte 0,1*64,2*64,3*64
pat1_8   byte 0,1*64,2*64,3*64,1*64,2*64,0*64,3*64
pat2_8   byte 0,1*64,2*64,0*64,3*64,1*64,2*64,3*64
pat1_16  byte 0,1*64,2*64,3*64,   0,1*64,2*64,3*64,   0,1*64,2*64,3*64,0,   2*64,1*64,3*64
pat2_16  byte 0,1*64,2*64,   0,1*64,2*64,3*64,1*64,2*64,3*64,0*64,2*64,3*64,1*64,0*64,3*64
ti     byte 0,0,0
flash_st byte 1   ;@flash@

