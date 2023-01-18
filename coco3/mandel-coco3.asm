;for asm6809 assembler
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;
;256x128 Mandelbrot for the Tandy CoCo 3 (the 6809 code), 16 colors, rotated
;on the 256x192 16 colors screen

NOCALC equ 0

CHROUT equ $A002
POLCAT equ $A000    ;Z=0 and A=key
STIMER equ $112

initer	equ 7
idx	equ	-36       ;-.0703125
idy	equ	18        ;.03515625, 1 = 1/512
ix0	equ	-62*idx
imx	equ	10*idx		; x move
sf4	equ	436/4		; sf/4

sqrbase equ $2900  ; +-$16b0 = $1250-3fb0

         org $b00+24
         setdp dpage/256
     ldx #msg
2    lda ,x+
     beq 1F

     jsr [CHROUT]
     bra 2B

1    jsr [POLCAT]
     beq 1B

     ldb #dpage/256
     tfr b,dp
     sta <benchmark
     sta $ffd9   ;high speed
     lds #msg+70

     lda #$4c   ;??
     sta $ff90  ;bit 7 only
     ldd #$801a ;+$2000  ;??alternative palette
     std $ff98  ;256x192x16
     ldd #$e800
     std $ff9d
     ;;lda #0
     ;;sta $ff9f   ;hor offset

    ldx #$ffb0   ;set palette
    clra
1   sta ,x+
    adda #$41
    cmpx #$ffc0
    bne 1B

   	ldd #0
	std <r0	        ;clr r0; 7 lower bits in high byte
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
    ldd #0
    std STIMER
    std <time
    lda #16
    sta <bcount
mand1
    ldy #$4000
    ldx #$407f

	ldd <dy      ;mov	@#dya, r5
    exg a,b     ;swab	r5
    lsra        ;asr	r5		; r5 = 128*dy
    rorb
    std r5
    ldu #sqrbase
loop0
  if NOCALC==0
x0 equ *+1
	ldd	#ix0     ;mov	#x0, r4
    std r4
  endif
loop2
  if NOCALC==0
    ldd r4        ;add @#dxa,r4
    addd <dx
    std r4
    std <r0           ;mov r4,r0
  endif
niter equ *+1
    lda #initer
  if NOCALC==0
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
  endif
r5 equ *+1
    addd #0      ;add r5,r1
  if NOCALC==0
    std <r1
    ldd <t        ;sub r3,r0
    subd <r3
    subd <r3       ;sub r3,r0
r4 equ *+1
    addd #0      ;add r4,r0
    std <r0
    dec <r2       ;sob r2,1$
    bne loc1
  endif
loc2
    lda <r2
    anda #$f
xtoggle equ * + 1
    ldb #0
    bne loc8

    sta ,x   ;right
    asla
    asla
    asla
    asla
    sta ,y   ;left
    leax 128,x
    leay 128,y
    cmpx #$8000
    lbcs loop2

    inc xtoggle  ;??
    leay -$4000,y
    leax -$4000,x
    bra lr
loc8
    ora ,y
    sta ,y
    ASLa       ;Efficient nybble-swap on 6502 by Garth Wilson
    ADCa  #$80 ;adapted for the 6809
    ROLa
    ASLa
    ADCa  #$80
    ROLa
    sta ,x
    leay 128,y
    leax 128,x
    cmpy #$8000
    lbcs loop2

    leax -$4001,x
    leay -$3fff,y
    clr xtoggle
lr
    ldd r5
    subd <dy
    std r5   ;sub	@#dya, r5
	lbne loop0

  if NOCALC==0
    ldd x0
    addd <mx
    std x0     ;add	@#mxa, @#x0a
    ldx #6
loc4
    ldd #sqrbase+sf4
    addd dx-2,x
    andb #$fe
    tfr d,y
    ldd ,y
    pshs a,b        ;mov	sqr+sf4(r2), (r1)
    ldd #sqrbase-sf4
    addd dx-2,x
    andb #$fe
    tfr d,y
    puls a,b
    subd ,y
    std dx-2,x
    leax -2,x      ;sub	sqr-sf4(r2), (r1)+
    bne loc4  ;sob	r0, 4$
  endif
    inc	niter
    lda <benchmark
    cmpa #"B"
    bne 1F

    dec <bcount
    lbne mand1

1   ldd STIMER
    addd <time
    std <time
    lda <benchmark
    cmpa #"B"
    beq 2F

    jsr getchr
    cmpa #"T"
    beq 2F

    cmpa #"Q"
    lbne mandel
exit
    lda #$cc
    sta $ff90
    sta $71    ;restart basic
    jmp $8c1b

2   clra
    sta <xpos+1
    ldb niter
    subb #7
    jsr pr000
    ldy #11*8   ;space
    jsr outdigi
    ;lda <xpos+1
    ;adda #4
    ;sta <xpos+1

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
    clra
    tfr a,dp
    jsr [POLCAT]
    beq getchr

    ldb #dpage/256
    tfr b,dp
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

         leax 124,x    ;128 - screen width in bytes
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
         fcb 0,0,0,0,0,0,0,0               ;space

         org $e00
dpage

dx	fdb	idx
dy	fdb	idy
mx	fdb	imx
xpos fdb 0,0   ;the 1st zero matters

msg     fcb "**************************",13
        fcb "*  Superfast Mandelbrot  *",13
        fcb "*       generator        *",13
        fcb "* 16 colors, rotated, v1 *",13
        fcb "**************************",13
        fcb "The original version was",13
        fcb "published for the BK0011 in",13
        fcb "2021 by Stanislav Maslovski.",13
        fcb "This Tandy CoCo 3 port was",13
        fcb "created by Litwr, 2023.",13
        fcb "The T-key gives us timings.",13
        fcb "Use the Q-key to quit.",13
        fcb "Press B to enter benchmark mode"
endp    fcb 0

benchmark equ msg
bcount equ msg+1 
time equ msg+2
;xpos equ msg+4
r0 equ msg+6
r1 equ msg+8
r3 equ msg+10
dividend equ r3
t equ msg+12
divisor equ t
r2 equ msg+14
ds equ msg+16   ;4 bytes are used

