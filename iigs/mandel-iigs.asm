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

dx = $e0   ;$e1   ;these 3 values must be in one bundle
dy = $e2   ;$e3
mx = $e4   ;$e5

xpos = $62

d = $fa   ;..$fd
divisor = $4a     ;$4b, $4c..$4d used for hi-bytes and the product
dividend = $50	  ;..$53 used for hi-bytes
remainder = $ce   ;$cf used for hi-byte
quotient = dividend ;save memory by reusing divident to store the quotient

   org $a00
   a8
   x8
   ;setdp 0

start:  jsr IOSAVE
        ;jsr HOME  ;clear screen
.m:     lda msg
        beq .exm

        ora #$80
        jsr COUT
        inc .m+1
        bne .m

        inc .m+2
.lg:    bne .m    ;always

.exm:   jsr RDKEY
        jsr setmouse
        sei
        ldx #INITMOUSE
        jsr mousesub
        lda #8
        ldx #SETMOUSE
        jsr mousesub

    lda $c029  ;set super hi-res
    ora #$c0
    sta $c029
    CLC            ; set native mode
    XCE
    rep #$30   ;16-bit index/acc
    x16
    a16

        lda $3fe
        sta .irqv
        lda #timeirq
        sta $3fe
        cli
    lda #idy
    sta dy
    lda #idx
    sta dx
    lda #imx
    sta mx

;    lda #0     ;fill scan-line control bytes, it is done automatic for zeroes
;    ldx #200
;.l1:dex
;    sta $e19d00,x
;    bne .l1

    ldx #30   ;fill palette #0 bytes, MVP??
.l2:lda pal,x
    sta $e19e00,x
    dex
    dex
    bpl .l2

.fillsqr:
    stz r0
    stz r1
    stz r2

    lda #sqrbase
    sta tmp
    sta t
.sqrloop:
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
    bcs .mandel		; exit on overflow

	inc r2   ;inc	r2
    bne .sqrloop
.mandel:
         stz time  ;clear timer
         stz time+2
    lda #$db0   ;$b0 = bcs - start timer
    sta timeirq.sw
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
    sep #$20  ;8-bit acc ?? .xtoggle to byte
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

.la:stz .index
    inc .xtoggle
    bne .lr   ;always

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
    stz .index
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
    clc
    lda .x0
    adc mx
    sta .x0     ;add	@#mxa, @#x0a
    ldx #4
.loc4:
    clc
    lda #sqrbase+sf4
    adc dx,x
    and #$fffe
    tay
    lda 0,y
    pha        ;mov	sqr+sf4(r2), (r1)
    lda #sqrbase-sf4
    clc
    adc dx,x
    and #$fffe
    tay
    pla
    sec
    sbc 0,y
    sta dx,x
    dex
    dex   ;sub	sqr-sf4(r2), (r1)+
    bpl .loc4  ;sob	r0, 4$

    sep #$20  ;8-bit acc
    a8
    lda #$7f
    sta .z1+1
    sta .z4+1
    stz .z0+1
    stz .z2+1
    stz .z3+1
    inc	.niter
    lda #$90   ;bcc  - stop timer
    sta timeirq.sw
    rep #$20  ;16-bit acc
    a16

    sec
    xce
    x8
    a8
    jsr RDKEY
    and #$1f
    cmp #"Q"&$1f
    bne .noq

    clc
    xce
    rep #$30     ;16-bit index/acc
    x16
    a16

.irqv:
    lda #0
    sta $3fe
    sec
    xce
    a8
    x8
    sei
    lda $c029  ;reset super hi-res
    and #$3f
    sta $c029
         lda #0
         ldx #SETMOUSE
         jsr mousesub
   jsr IOREST
   jmp 3

.noq:
    cmp #"T"&$1f
    beq .t

    clc
    xce
    rep #$30     ;16-bit index/acc
    x16
    a16
    jmp .mandel

.t: clc
    xce
    rep #$31  ;16-bit idx/acc
    ldx #0
    stx xpos
    lda .niter
    sbc #6   ;C=0

    jsr pr000
    ldy #10*8   ;space
    jsr outdigi

    lda time
    sta dividend
    lda time+2
    sta dividend+2
    lda #60
    sta divisor
    jsr div32x16m
    lda quotient
    jsr pr000
    ldy #11*8   ;dot
    jsr outdigi
         lda remainder  ;*50
         asl
         asl
         adc remainder
         sta dividend
         asl
         asl
         adc dividend
         asl
         sta dividend
         lda #0
         sta dividend+2
         lda #3
         sta divisor
         jsr div32x16m
         lda remainder
         cmp #2
         bcc *+4
         inc quotient
         lda quotient
         jsr pr000
    sec
    xce
    jsr RDKEY
    clc
    xce
    rep #$30
    a16
    x16
    jmp .mandel

       x16
       a16 
div32x16m:       ;dividend+2 < divisor
        lda dividend+2
        clc
        ldy #16
.l3:    rol dividend
        rol
        cmp divisor
        bcc .l1

        sbc divisor
.l1:    dey
        bne .l3
        rol dividend
        sta remainder
        stz dividend+2
	    rts

       x8
       a8
mousesub:stx .p6+1
.p6:     ldx $c400
         stx .p2+1
         pha
         lda #$c4   ;.p6+2
         tax
         asl
         asl
         asl
         asl
         tay
         pla
.p2:     jmp $c400

timeirq: jsr $c400
         bcs .nomouse
.sw:     bcc .nomouse

         inc time
         bne .nomouse

         inc time+1
         bne .nomouse

         inc time+2
.nomouse:rti

setmouse:lda $c400+SERVEMOUSE
         sta timeirq+1
         rts

       x16
       a16
outdigi:   ;xpos,Y-char(0..11)*8,8/16-bit acc/idx
t1 = r0
t2 = r1
t3 = r2
         sep #$20
         a8
         ldx xpos
.l3:     lda digifont,y
         phy
         sta t1
         lda #4
         sta t2
.l6:     lda #2
         sta t3
.l4:     ldy #4
.l1:     clc
         bit t1
         bpl .l5

         sec
.l5:     rol
         dey
         bne .l1

         asl t1
         dec t3
         bne .l4

         sta $e12000,x
         inx
         dec t2
         bne .l6

         rep #$20
         a16
         txa
         clc
         adc #$a0-4
         tax
         sep #$20
         a8
         ply
         iny
         tya
         and #7
         bne .l3

         lda xpos
         clc
         adc #4
         sta xpos
         rep #$20
         rts

        a16
pr000: ;prints C = B:A
         sta d+2
         lda #100
         sta d
         jsr .pr0
         lda #10
         sta d
         jsr .pr0
         ldx d+2
.prd:    txa
         asl
         asl
         asl
         tay
         sep #$20
         a8
         jsr outdigi
         rep #$20
         a16
         rts

.pr0:    ldx #65535
.prn:    inx
         lda d+2
         cmp d
         bcc .prd

         sbc d
         sta d+2
         bcs .prn   ;always

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
         
digifont db $3c,$66,$6e,$76,$66,$66,$3c,0  ;0
         db $18,$18,$38,$18,$18,$18,$7e,0  ;1
         db $3c,$66,6,$c,$30,$60,$7e,0     ;2
         db $3c,$66,6,$c,6,$66,$3c,0     ;3
         db 6,$e,$1e,$36,$7f,6,6,0       ;4
         db $7e,$60,$7c,6,6,$66,$3c,0   ;5
         db $3c,$66,$60,$7c,$66,$66,$3c,0  ;6
         db $7e,$66,$c,$18,$18,$18,$18,0   ;7
         db $3c,$66,$66,$3c,$66,$66,$3c,0  ;8
         db $3c,$66,$66,$3e,6,$66,$3c,0   ;9
         db 0,0,0,0,0,0,0,0               ;space
         db 0,0,0,0,0,$18,$18,0         ;dot

time    byte 0,0,0,0
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

