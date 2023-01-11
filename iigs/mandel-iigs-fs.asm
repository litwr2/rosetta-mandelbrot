;for vasm assembler, oldstyle syntax
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;The next code was made by litwr in 2022
;
;Fullscreen (320x200) Mandelbrot for the Apple II gs, 16 color mode

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

sqrbase = $2700 ;must be $xx00, it takes area $f50-$3bb0 +$100

;r0 = $ec   ;$ed
r1 = $ee   ;$ef
r2 = $fa   ;$fb
r3 = $4a   ;$4b
t = $fc   ;$fd
;tmp = $ce   ;$cf

dx = $e0   ;$e1
dy = $e2   ;$e3
;mx = $e4   ;$e5

xpos = $62

d = $fa   ;..$fd
divisor = $4a     ;$4b, $4c..$4d used for hi-bytes and the product
dividend = $50	  ;..$53 used for hi-bytes
remainder = $ce   ;$cf used for hi-byte
quotient = dividend ;save memory by reusing divident to store the quotient

   org $a00
   a8
   x8
   setdp 0

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
        sta .irqv+1
        lda #timeirq
        sta $3fe
        cli
    lda #0
    sta dy
    lda #-1
    sta dx

;    lda #0     ;fill scan-line control bytes, it is done automatic for zeroes
;    ldx #200
;.l1:dex
;    sta $e19d00,x
;    bne .l1

     phb
     lda #31
     ldx #pal
     ldy #$9e00
     mvn 0,$e1
     plb

.fillsqr:
    stz t
    stz r1
    stz r2

    ldx #sqrbase
    txy
.sqrloop:
    lda t     ;mov	r1, (r5)+	; to upper half tbl
    sta 0,x
    inx
    inx
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
	clc       ;add	r2, r5		; add up lower bits
    adc r5
    sta r5
	lda t       ;adc	r1		; add carry to r1
    adc #0      ;add	r3, r1		; R1:R0 = x^2 + 2^-8*x + 2^-16
    adc r3
    sta t
	dey    ;mov	r1, -(r4)	; to lower half tbl
    dey
    sta 0,y
	pla      ;mov	(r6)+, r2
    sta r2
    bcs .mandel		; exit on overflow

	inc r2   ;inc	r2
    bne .sqrloop
.mandel:
    ldx dataindex
    lda 0,x
    sep #$20
    a8
    sta dx
    xba
    sta dy
    inx
    inx
    ldy 0,x
    sty .x0
    inx
    inx
    lda 0,x
    sta .niter
    inc
    inc
    sta 0,x
    inx
    cpx #iter
    bne .l7

    ldx #mdata
.l7:stx dataindex
    rep #$20
    a16

         stz time  ;clear timer
         stz time+2
    lda #$db0   ;$b0 = bcs - start timer
    sta timeirq.sw
    lda dy   ;mov	@#dya, r5
    sta r4
    asl
    asl
    asl    ;*8
    sta r5
    asl    ;*16
    adc r5 ;*17
    adc r4 ;*25
    asl
    asl    ; r5 = 100*dy
    sta r5
    ldx #$a0
    ldy #$7d00
.mloop0:
.x0 = * + 1
    lda #0
    sta r4     ;mov	#x0, r4
.mloop1:
    stx .tindex
    sty .bindex
.mloop2:
    clc
    lda r4
    adc dx
    sta r4
    tay        ;add	@#dxa, r4
               ;mov	r4, r0
.niter = * + 1
    lda #0
    sta r2        ;mov	#niter, r2
	lda r5
    sta r1       ;mov	r5, r1
.loc1:
    clc
    lda r1
    adc #sqrbase
    and #$fffe
    tax
    lda 0,x
    sta r3         ;mov	sqr(r1), r3
    clc
    tya
    adc #sqrbase
    and #$fffe
    tax
    lda 0,x       ;mov	sqr(r0), r0
    clc
    adc r3
    sta t      ;add	r3, r0
    cmp #$800
    bcs .loc2

    tya
    adc r1    ;C=0
    ;sta r1      ;add	r0, r1
    clc
    adc #sqrbase
    and #$fffe
    tax
    lda 0,x
    ;sta r1     ;mov sqr(r1), r1
    clc
r5 = * + 1
    adc #0   ;C=0
    ;sta r1     ;add	r5, r1
    sec
    sbc t
    sta r1     ;sub	r0, r1
    sec
    lda t
    sbc r3
    ;tay        ;sub	r3, r0
	;sec   ;it seems, C=1 is always here
    sbc r3
    ;tay        ;sub	r3, r0
	clc
r4 = * + 1
    adc #0
    tay     ;add	r4, r0
    dec r2
    bne .loc1       ;sob	r2, 1$
.loc2:
    sep #$20
    a8
    lda r2   ;color index
.ptoggle = * + 1
    ldy #0
    bne .loc8

    and #15
    sta .ccolor
    inc
    sta .ptoggle
    rep #$21  ;16-bit acc, C=0
    bra .mloop2
.loc8:
    stz .ptoggle
    asl
    asl
    asl
    asl
.ccolor = * + 1
    ora #0
.bindex = * + 1
    ldx #$7d00
    dex
    sta $e12000,x
    txy
.tindex = * + 1
    ldx #$a0
    dex
    sta $e12000,x
    dec .xcount
    rep #$21  ;16-bit acc, C=0
    a16

    ;bne .mloop1
    beq .lb
    jmp .mloop1
.xcount: word 160
.lb:
    lda #160
    sta .xcount
    txa
    adc #160*2
    tax

    sec
    lda r5
    sbc dy
    sta r5   ;sub	@#dya, r5
;	bne .mloop0
    beq .lzx
    jmp .mloop0
.lzx:
    sep #$20  ;8-bit acc
    a8
    inc	iter
    lda #$90   ;bcc  - stop timer
    sta timeirq.sw
    ;rep #$20  ;16-bit acc
    ;a16

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
   jmp IOREST

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
    stz xpos
    lda iter
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
         stz dividend+2
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
    and #$1f
    cmp #"Q"&$1f
    bne .loc7
    jmp .irqv
.loc7:
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
t1 = r3
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

  macro mentry
     byte -\1*319/320-1, \2*199/200+1
     word \1*160-384   ;dx, dy, x0 = dx/160, niter
     byte \3
  endm

;x-min = (x0+dx*HSize)/512, x-max = x0/512, y-max = dy*VSize/1024
mdata:    ;dx, dy, iterations
     mentry 9, 14, 7   ;1
     mentry 8, 11, 8   ;2
     mentry 8, 9, 9   ;3
     mentry 7, 8, 10  ;4
     mentry 6, 7, 11  ;5
     mentry 5, 6, 12   ;6
     mentry 5, 5, 13   ;7
     mentry 4, 4, 14   ;8
     mentry 4, 4, 15   ;9
     mentry 4, 4, 16   ;10
     mentry 3, 4, 25   ;11
     mentry 4, 6, 37   ;12

iter word 0
dataindex word mdata

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
        byte "* Fullscreen, 320x200, 16 colors *",13
        byte "*              v2                *",13
        byte "**********************************",13
        byte "This code for the Apple IIgs was",13
        byte "created by Litwr in 2022. It is based",13
        byte "on code published for the BK0011 in",13
        byte "2021 by Stanislav Maslovski.",13
        byte "The T-key gives us timings.",13
        byte "Use the Q-key to quit",0

