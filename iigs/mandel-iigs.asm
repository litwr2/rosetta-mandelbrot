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
    lda $c029  ;set super hi-res
    ora #$c0
    sta $c029
    rep #$30   ;16-bit index/acc
    x16
    a16

    lda #idy
    sta dy
    lda #idx
    sta dx
    lda #imx
    sta mx

;    lda #0     ;fill scan-line control bytes
;    ldx #200
;.l1:dex
;    sta $e19d00,x
;    bne .l1

    ldx #30   ;fill palette #0 bytes
.l2:lda pal,x
    sta $e19e00,x
    dex
    dex
    bpl .l2

fillsqr:
    lda #0
    sta r0
    sta r1
    sta r2

    lda #sqrbase
    sta tmp
    sta t
sqrloop:
    lda r1     ;mov	r1, (r5)+	; to upper half tbl
    sta (t)
    inc t
    inc t
    inc r2	  ;inc	r2		; R2 = x + 2^-9
    lda r2    ;mov	r2, -(r6)
    pha
	asl       ;asl	r2		; R2 = 2*x + 2^-8
    xba       ;swab	r2		; LLLLLL00 00HHHHHH
    sta r2
    pha
	and #$ff    ;movb	r2, r3		; 00000000 00HHHHHH
    sta r3
    pla
	clc       ;add	r2, r0		; add up lower bits
    adc r0
    sta r0
	lda r1       ;adc	r1		; add carry to r1
    adc #0
	;clc        ;add	r3, r1		; R1:R0 = x^2 + 2^-8*x + 2^-16
    adc r3
    sta r1
	dec tmp    ;mov	r1, -(r4)	; to lower half tbl
    dec tmp
    sta (tmp)
	pla      ;mov	(r6)+, r2
    sta r2
    bcs mandel		; exit on overflow

	inc r2   ;inc	r2
    bne sqrloop
mandel:
         lda #0
         sta tmp
         sta time  ;clear timer
         sta time+1

    lda dy   ;mov	@#dya, r5
	xba      ;swab	r5
	lsr      ;asr	r5		; r5 = 128*dy
    sta r5
.mloop0:
.x0 = * + 1
    lda #ix0
    sta r4     ;mov	#x0, r4
.mloop2:
    clc
    lda r4
    adc dx
    sta r4
    sta r0      ;add	@#dxa, r4
               ;mov	r4, r0
.niter = * + 1
    lda #initer
    sta r2        ;mov	#niter, r2
	lda r5
    sta r1       ;mov	r5, r1
.loc1:
    clc
    lda r1
    adc #sqrbase
    and #$fffe
    tay
    lda 0,y
    sta r3         ;mov	sqr(r1), r3

    clc   ;??
    lda r0
    adc #sqrbase
    and #$fffe
    tay
    lda 0,y
          ;mov	sqr(r0), r0

    clc   ;??
    adc r3
    sta t      ;add	r3, r0
    cmp #$800
    bcs .loc2

    lda r0
    adc r1    ;C=0
    ;sta r1      ;add	r0, r1
    clc
    adc #sqrbase
    and #$fffe
    tay
    lda 0,y
    ;sta r1     ;mov sqr(r1), r1
    clc   ;??
r5 = * + 1
    adc #0   ;C=0
    ;sta r1     ;add	r5, r1

    sec   ;??
    sbc t
    sta r1     ;sub	r0, r1
    sec    ;??
    lda t
    sbc r3
    ;sta r0        ;sub	r3, r0
	;sec   ;it seems, C=1 is always here
    sbc r3
    ;sta r0        ;sub	r3, r0
	clc   ;??
r4 = * + 1
    adc #0
    sta r0     ;add	r4, r0
    dec r2
    bne .loc1       ;sob	r2, 1$
.loc2:
.index = * + 1
    ldx #0
    lda r2   ;color index
.xtoggle = * + 1
    ldy #0
    sep #$20  ;8-bit acc
    a8
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
    beq .la
    jmp .mloop2

.la:inc .xtoggle
    lda #0
    sta .index
    beq .lr   ;always

.loc8:
    a8
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
;    bne .mloop2
    beq .lzz
    jmp .mloop2
.lzz:
    lda #0
    sta .index
    dec .xtoggle
    inc .z0+1
    inc .z2+1
    inc .z3+1
    dec .z1+1
    dec .z4+1

.lr:sec
    lda r5
    sbc dy
    sta r5   ;sub	@#dya, r5
;	bne .mloop0
    beq .lzx
    jmp .mloop0
.lzx:
    sep #$30     ;8-bit index/acc
    x8
    a8
    clc
    lda .x0
    adc mx
    sta .x0
    lda .x0+1
    adc mx+1
    sta .x0+1     ;add	@#mxa, @#x0a
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
    rep #$30     ;16-bit index/acc
    x16
    a16
    jmp mandel
    ;and #$df
    cmp #"Q"
    bne .noq

    ;lda $c029  ;reset super hi-res
    ;and #$3f
    ;sta $c029
   ;jmp exit

.noq:
  if 0
    cmp #"T"
    ;**beq *+5
    jmp mandel
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

