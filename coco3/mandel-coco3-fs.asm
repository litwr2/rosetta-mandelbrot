;for asm6809 assembler
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;
;320x225 Fullscreen Mandelbrot for the Tandy CoCo 3 (the 6809/6309 code), 16 colors

CPU6309 equ 0
HSize equ 320
VSize equ 225

CHROUT equ $A002
POLCAT equ $A000    ;Z=0 and A=key
STIMER equ $112

sqrbase equ $2900  ; +-$16b0 = $1250-3fb0

         org $b00
         setdp dpage/256
  if CPU6309==1
     ldmd #1     ;to native mode
  endif
     ldx #msg
2    lda ,x+
     beq 1F

     jsr [CHROUT]
     bra 2B

1    jsr [POLCAT]
     beq 1B

     ldb #dpage/256
     tfr b,dp
     sta $ffd9   ;high speed
   orcc #$50  ;stop interrupts

     lds #msg+70

     lda #$4c   ;??
     sta $ff90  ;bit 7 only
     ldd #$807e  ;+$2000  ;??alternative palette
     std $ff98  ;320x225x16
     ldd #$e800
     std $ff9d
     ;;lda #0
     ;;sta $ff9f   ;hor offset

    ldx #$ffb0   ;set palette
    lda #0
1   sta ,x+
    adda #$41
    cmpx #$ffc0
    bne 1B

   	ldd #0
	std <r0	;clr r0; 7 lower bits in high byte
	std <r1   	;clr r1; higher 11+1 bits
	tfr d,u		;clr r2; operand-index
	ldx #sqrbase	;mov	#sqr, r4; for lower half-table
	tfr x,y		;mov	r4, r5; for upper half-table
fillsqr
	std ,y++    ;mov r1, (r5)+; to upper half tbl
    leau 1,u    ;inc r2
	tfr u,d     ;asl r2
    aslb
    rola
    exg a,b     ;swab r2  ; LLLLLL00 00HHHHHH
    stb <r3+1    ;movb	r2, r3		; 00000000 00HHHHHH
    clr <r3
	addd <r0     ;add	r2, r0		; add up lower bits
    std <r0
	ldd <r3      ;adc	r1		; add carry to r1
	adcb <r1+1   ;add	r3, r1		; R1:R0 = x^2 + 2^-8*x + 2^-16
    adca <r1
    std <r1
    std ,--x    ;mov	r1, -(r4)	; to lower half tbl
    bcs mandel  ;bcs mdlbrt

    leau 1,u    ;inc r2
	bra fillsqr

mandel
    ldx dataindex
    ldd ,x++
    sta <dx+1
    stb <dy+1
    ldd ,x++
    std x0
    lda ,x
    sta niter
    inca
    inca
    sta ,x+
    cmpx #dataindex
    bne 1F

    ldx #mdata
1   stx dataindex

    ldd #0
    std STIMER
    std <time

    ldy #$4000+HSize/2
    ldx #$4000+HSize/2*VSize

	lda <dy+1      ;mov	@#dya, r5
    ldb #VSize/2
    mul
    std <r1
    lda <dy
    ldb #VSize/2
    mul
    exg a,b
  if VSize%2==1
    addd <dy
  endif
    addd <r1
    std r5
    ldu #sqrbase
loop0
x0 equ *+1
	ldd	#0     ;mov	#x0, r4
    std r4
    lda #HSize/2
    sta <xcount
loop2
    ldd r4        ;add @#dxa,r4
    addd <dx
    std r4
    std <r0           ;mov r4,r0
niter equ *+1
    lda #0
    sta <r2        ;mov #initer,r2
    ldd r5        ;mov r5,r1
    std <r1
loc1
    ldd <r1        ;mov sqr(r1),r3
    andb #$fe
    ldd d,u
    std <r3

    ldd <r0        ;mov sqr(r0),r0
    andb #$fe
    ldd d,u

    addd <r3       ;add r3,r0
	cmpa #8       ;cmp r0,r6
    bcc loc2      ;bge loc2

    std <t
    ldd <r0       ;add r0,r1
    addd <r1
    andb #$fe     ;mov sqr(r1),r1
    ldd d,u
    subd <t       ;sub r0,r1
r5 equ *+1
    addd #0      ;add r5,r1
    std <r1
    ldd <t        ;sub r3,r0
    subd <r3
    subd <r3       ;sub r3,r0
r4 equ *+1
    addd #0      ;add r4,r0
    std <r0
    dec <r2       ;sob r2,1$
    bne loc1
loc2
    lda <r2
xtoggle equ * + 1
    ldb #0
    bne loc8

    anda #$f
    sta tcolor
    inca
    sta xtoggle   ;??
    bra loop2
loc8
    clr xtoggle
    asla
    asla
    asla
    asla
tcolor equ *+1
    ora #0
    sta ,-x   ;bottom
    sta ,-y   ;top
    dec <xcount
    bne loop2

    leay HSize,y
    ldd r5
    subd <dy
    std r5   ;sub	@#dya, r5
	lbne loop0

    inc <iter
    ldd STIMER
    addd <time
    std <time

    jsr getchr
    cmpa #"T"
    beq 2F

    cmpa #"Q"
    lbne mandel
exit
    sta $ffde  ;rom
    lda #$cc
    sta $ff90
    sta $71    ;restart basic
    jmp $8c1b

2   clra
    sta <xpos+1
    ldb <iter
    jsr pr000
;    ldy #11*8   ;space
;    jsr outdigi
    lda <xpos+1
    adda #4
    sta <xpos+1

    ldd <time
    std <dividend
    ldd #60
    jsr div16x16w
    std <divisor   ;remainder
    ldd <dividend
    jsr pr000
    ldy #10*8   ;dot
    jsr outdigi
    lda divisor+1
    ldb #50
    mul
         std <dividend
         ldd #3
         jsr div16x16w
         cmpb #2
         ldd <dividend
         bcs 1F

         addd #1
1        jsr pr000
    jsr getchr
    cmpa #"Q"
    beq exit
    jmp mandel

getchr
    sta $ffde  ;rom
   andcc #$af  ;allow interrupts
    lda #0
    tfr a,dp
    jsr [POLCAT]
    beq getchr

    ldb #dpage/256
    tfr b,dp
   orcc #$50  ;stop interrupts
    sta $ffdf   ;ram
    rts

outdigi   ;xpos,Y-char(0..11)*8
         ldx <xpos
3        lda digifont,y
         pshs y
         sta <r1
         lda #4
         sta <r2
6        lda #2
         sta <r3
4        ldy #4
1        lda #$80
         adda <r1
         rolb
         leay -1,y
         bne 1B

         asl <r1
         dec <r3
         bne 4B

         stb $4000,x
         leax 1,x
         dec <r2
         bne 6B

         leax HSize/2-4,x
         puls y
         leay 1,y
         tfr y,d
         andb #7
         bne 3B

         lda <xpos+1
         adda #4
         sta <xpos+1
         rts

pr000 ;prints D = B:A
         std <ds+2
         ldd #100
         std <ds
         jsr pr0
         lda #10
         sta <ds+1
         jsr pr0
         ldx <ds+2
prd      tfr x,d
         aslb
         aslb
         aslb
         tfr d,y
         bra outdigi

pr0      ldx #65535
prn      leax 1,x
         ldd <ds+2
         cmpd <ds
         bcs prd

         subd <ds
         std <ds+2
         bra prn

div16x16w        ;dividend, dividend < divisor, divisor < $8000
                 ;quotinent = dividend, remainder = D, X = 0
        std <divisor
        ldx #16
        ldd #0
2       asl <dividend+1
        rol <dividend
        rolb
	    rola
	    cmpd <divisor
        bcs 1F

        subd <divisor
	    inc <dividend+1
1       leax -1,x
        bne 2B

        ;std <remainder
        rts

digifont fcb $3c,$66,$6e,$76,$66,$66,$3c,0  ;0
         fcb $18,$18,$38,$18,$18,$18,$7e,0  ;1
         fcb $3c,$66,6,$c,$30,$60,$7e,0     ;2
         fcb $3c,$66,6,$c,6,$66,$3c,0     ;3
         fcb 6,$e,$1e,$36,$7f,6,6,0       ;4
         fcb $7e,$60,$7c,6,6,$66,$3c,0   ;5
         fcb $3c,$66,$60,$7c,$66,$66,$3c,0  ;6
         fcb $7e,$66,$c,$18,$18,$18,$18,0   ;7
         fcb $3c,$66,$66,$3c,$66,$66,$3c,0  ;8
         fcb $3c,$66,$66,$3e,6,$66,$3c,0   ;9
         fcb 0,0,0,0,0,$18,$18,0         ;dot
         ;fcb 0,0,0,0,0,0,0,0               ;space

mentry macro
     fcb -\1*319/HSize-1, \2*199/VSize+1
     fdb \1*HSize/2-384   ;dx, dy, x0 = dx/160, niter
     fcb \3
     endm

;x-min = (x0+dx*HSize)/512, x-max = x0/512, y-max = dy*VSize/1024
mdata    ;dx, dy, iterations
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

dataindex fdb mdata

         org $e00
dpage

dx	fdb	-1
dy	fdb	0
xpos fdb 0,0   ;the 1st zero matters
iter fcb 0

msg     fcb "**************************",13
        fcb "*  Superfast Mandelbrot  *",13
        fcb "* generator, fullscreen  *",13
        fcb "* 16 colors, 320x225, v1 *",13
        fcb "**************************",13
        fcb "This code for the Tandy",13
        fcb "Color 3 was created by Litwr",13
        fcb "in 2023. It is based on code",13
        fcb "published for the BK0011 in",13
        fcb "2021 by Stanislav Maslovski.",13
        fcb "The T-key gives us timings.",13
        fcb "Use the Q-key to quit.",0
xcount equ msg  ;1 byte
time equ msg+2
;xpos equ msg+4
r0 equ msg+6
r1 equ msg+8
r3 equ msg+10
dividend equ r3
t equ msg+12
divisor equ t
r2 equ msg+14
ds equ msg+16

