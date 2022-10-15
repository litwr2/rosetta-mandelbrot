;for vasm assembler, oldstyle syntax
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;The next code was made by litwr in 2022
;
;256x128 Mandelbrot for the Apple II gs, 16 color mode

COUT = $FDED    ;print char in AC, the redirection is possible
;COUT1 = $FDF0   ;print char in AC, screen only
CROUT = $FD8E   ;print nl, corrupts AC
RDKEY = $FD0C   ;wait and read a char to AC from the kbd
HOME = $FC58
IOSAVE = $FF4A
IOREST = $FF3F
SETMOUSE = $12
SERVEMOUSE = $13
INITMOUSE = $19

SEEKMOUSE = 1         ;seek mouse card, 0 means to use the $c400 address

sqrbase = $2600 ;must be $xx00, it takes area $f50-$3bb0
initer	= 7
idx	=	-36       ;-.0703125
idy	=	18        ;.03515625, 1 = 1/512
ix0	=	-62*idx
imx	=	10*idx		; x move
sf4	=	436/4		; sf/4

r0 = $ec   ;$ed
r1 = $ee   ;$ef
r2 = $fa   ;$fb
r3 = $4a   ;$4b
t = $fc   ;$fd
tmp = $ce   ;$cf

dx = $4c   ;$4d   ;these 3 values must be in one bundle
dy = $4e   ;$4f
mx = $50   ;$51

d = $fa   ;..$fd
divisor = $4a     ;$4b, $4c..$4d used for hi-bytes and the product
dividend = $4e	  ;..$51 used for hi-bytes
remainder = $ce   ;$cf used for hi-byte
quotient = dividend ;save memory by reusing divident to store the quotient

   org $a00
   a8
   x8
   ;setdp 0

start:   jsr IOSAVE
         ;jsr HOME  ;clear screen

         ;**jsr setmouse

         sei
         ldx #INITMOUSE
         ;**jsr mousesub
         lda #8
         ldx #SETMOUSE
         ;**jsr mousesub
         lda $3fe
         ;**sta mlo+1
         lda $3ff
         ;**sta mhi+1
         ;**lda #<timeirq
         ;**sta $3fe
         ;**lda #>timeirq
         ;**sta $3ff
         cli

    CLC            ; set native mode
    XCE
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

    lda $c029  ;set super hi-res
    ora #$c0
    sta $c029

;    lda #0     ;fill scan-line control bytes
;    ldx #200
;.l1:dex
;    sta $e19d00,x
;    bne .l1

    ldx #31   ;fill palette #0 bytes
.l2:lda pal,x
    sta $e19e00,x
    dex
    bpl .l2

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
         lda #0
         sta tmp

         ldx #2          ;clear timer
.loopt:  sta time,x
         dex
         bpl .loopt

    lda dy
    lsr
    sta r5hi
    lda #0
    ror
    sta r5lo    ;r5 = 128*dy
.mloop0:
.x0lo = * + 1
    lda #<ix0
    sta r4lo
.x0hi = * + 1
    lda #>ix0
    sta r4hi  ;mov	#x0, r4
.mloop2:
    clc  
    lda r4lo
    adc dx
    sta r4lo
    sta r0
    lda r4hi
    adc #$ff
    sta r4hi      ;add	@#dxa, r4
    sta r0+1           ;mov	r4, r0
.niter = * + 1
    lda #initer   
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
    sta t+1      ;add	r3, r0

    cmp #8
    bcs .loc2

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
    adc #0   ;C=0
    tax 
    iny
    lda (tmp),y     ;mov sqr(r1), r1
r5hi = * + 1
    adc #0
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
.loc2:
    rep #$10   ;16-bit index
    x16

.index = * + 1
    ldx #0

    lda r2   ;color index
.xtoggle = * + 1
    ldy #0
    bne .loc8

    and #15
.z1:sta $e1207f,x
    asl
    asl
    asl
    asl
.z0:sta $e12000,x
    rep #$20  ;16-bit acc
    a16
    txa
    clc   ;rep??
    adc #$a0
    sta .index
    cmp #160*128
    bne .la

    lda #0
    sta .index
    inc .xtoggle
    jmp .lr

.la:sep #$30     ;8-bit index/acc
    x8
    a8
    jmp .mloop2

    x16
.loc8:
    and #15
.z2:ora $e12000,x
.z3 sta $e12000,x
    ASL      ;Efficient nybble-swap on 6502 by Garth Wilson
        ADC  #$80
        ROL
        ASL
        ADC  #$80
        ROL
.z4:sta $e1207f,x
    rep #$20  ;16-bit acc
    a16
    txa
    clc
    adc #$a0
    sta .index
    cmp #160*128
    beq .lzz
.lzx:
    sep #$30     ;8-bit index/acc
    x8
    a8
    jmp .mloop2

    x16
    a16
.lzz:
    lda #0
    sta .index
    dec .xtoggle
    inc .z0+1
    inc .z2+1
    inc .z3+1
    dec .z1+1
    dec .z4+1

.lr:sep #$30
    a8
    x8
    sec
    lda r5lo
    sbc dy
    sta r5lo
    lda r5hi
    sbc #0
    sta r5hi   ;sub	@#dya, r5
	beq .loc7
.lf:sep #$30     ;8-bit index/acc
    jmp .mloop0
.loc7:
    lda r5lo
    bne .lf

    sep #$30     ;8-bit index/acc
    x8
    a8
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

    lda #$7f
    sta .z1+1
    sta .z4+1
    lda #0
    sta .z0+1
    sta .z2+1
    sta .z3+1

    inc	.niter
         ;**get timing
    sec
    xce
    lda $4e
    pha
    lda $4f
    pha
    jsr RDKEY
    pla
    sta $4f
    pla
    sta $4e
    clc
    xce
    jmp mandel
    ;and #$df
    cmp #"Q"
    bne .noq

    lda $c029  ;reset super hi-res
    and #$3f
    sta $c029
   jmp exit

.noq:
    cmp #"T"
    ;**beq *+5
    jmp mandel
  if 0
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
    jmp mandel
  endif
exit     sec
         xce
         sei
;mlo      lda #0
;         sta $3fe
;mhi      lda #0
;         sta $3ff
;         lda #0
;         ldx #SETMOUSE
;         jsr mousesub
exitprg  jmp IOREST
 
  if 0
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
         jmp COUT

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
         bcs prn  ;always

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
  endif
time: byte 0,0,0    ;@timer@

  if 0
msg     byte "**********************************",13
        byte "* Superfast Mandelbrot generator *",13
        byte "*              v1                *",13
        byte "**********************************",13
        byte "The original version was published for",13
        byte "the BK0011 in 2021 by Stanislav",13
        byte "Maslovski.",13
        byte "This Apple IIgs port was created by",13
        byte "Litwr, 2022.",13
        byte "The T-key gives us timings.",13
        byte "Use the Q-key to quit",0

mousesub stx p6+1
p6       ldx $c400
         stx p2+1
         pha
         lda p6+2
         tax
         asl
         asl
         asl
         asl
         tay
         pla
p2       jmp 0

timeirq
p3       jsr 0
         bcs nomouse

         inc time
         bne nomouse

         inc time+1
         bne nomouse

         inc time+2
nomouse
         rti

setmouse
  if SEEKMOUSE
         ldx #$c1
         stx .p4+2
.loop3    ldx #4
.loop4    ldy .amagic,x
         lda .vmagic,x
.p4       cmp $c000,y
         beq .match

         inc .p4+2
         ldy .p4+2
         cpy #$c8
         bne .loop3

         jsr .mouserr
         pla
         pla
         jmp exitprg

.amagic .byte 5,7,$b,$c,$fb
.vmagic .byte $38,$18,1,$20,$d6

.match    dex
         bpl .loop4

         lda .p4+2
         sta .p7+2
         sta p3+2
         sta p2+2
         sta p6+2
  endif
.p7      lda $c400+SERVEMOUSE
         sta p3+1
         rts

.mouserr  ldx #0
.loop8    lda msg,x
         beq .exiterr

         jsr COUT
         inx
         bne .loop8
.exiterr  rts

msg .text "can't find a mouse card"
    .byte 0
  endif

pal: word 0     ;0 black  RGB
     word $fff  ;1 white
     word $f00  ;2 red
     word $0f0  ;3 green
     word $00f  ;4 blue
     word $ff0  ;5 yellow
     word $f0f  ;6 cyan
     word $0ff  ;7 magenta
     word $aaa  ;8 grey
     word $555  ;9 grey
     word $700  ;a red
     word $070  ;b green
     word $007  ;c blue
     word $770  ;d yellow
     word $707  ;e cyan
     word $077  ;f magenta

