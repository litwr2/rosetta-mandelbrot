	.title fractal
	.sbttl (C) 2021 Stanislav Maslovski <stanislav.maslovski@gmail.com>, all rights reserved.
;;https://www.pouet.net/prod.php?which=87739
;;this variant includes minor changes (+perfect texture, timings, 10/11 autotoggle, ...), by litwr, 2021

timerport1 = ^O177706            ;$ffc6
timerport2 = ^O177710            ;$ffc8
timerport3 = ^O177712            ;$ffca
kbddtport  = ^O177662            ;kbd data, palette, timer, $ffb2

	.asect
	.=1000
    mov #msg,r1
    clr r2
    emt ^O20
    emt 6
    mov #18.,r0    ;home cursor
    emt ^O16
    clr @#timerport1
;;	mov     #2*400+330, r0
;;	mov	r0, @#177664			; set scroll reg - it usually does nothing
;;	mtps	r0				; also mask interrupts - the same
    CMPB @#^O177717,#^O200
    BEQ 9$  ;BK0010

    mov	#75.*256., @#kbddtport  ; set pallete
    mov #488.,ticonst     ;4 MHz,4000000/64/128
9$:
;************************************************************
; Fixpoint squares up to approx. 8.0^2, 11 significant bits
; operand-index: 0000XXX.X XXXXXXX0
; result:        0XXXXXX.X XXXXXXX0
; In this scale, 1.0 decimal = 1000 octal
;************************************************************
r6	=	%6
sqr	=	20000		; table base
;************************************************************
	clr	r0		; 7 lower bits in high byte
	clr	r1		; higher 11+1 bits
	clr	r2		; operand-index
	mov	#sqr, r4	; for lower half-table
	mov	r4, r5		; for upper half-table
fsqr:
	mov	r1, (r5)+	; to upper half tbl
	inc	r2		; R2 = x + 2^-9
	mov	r2, -(r6)
	asl	r2		; R2 = 2*x + 2^-8
	swab	r2		; LLLLLL00 00HHHHHH
	movb	r2, r3		; 00000000 00HHHHHH
	add	r2, r0		; add up lower bits
	adc	r1		; add carry to r1
	add	r3, r1		; R1:R0 = x^2 + 2^-8*x + 2^-16
	mov	r1, -(r4)	; to lower half tbl
	mov	(r6)+, r2
	bcs	mdlbrt		; exit on overflow
	inc	r2
	br	fsqr
;************************************************************
niter	=	7
max2	=	4000		; 4.0
dx	=	-14*3
dy	=	6*3
x0	=	-76*dx
mx	=	12*dx		; x move
sf4	=	664/4		; sf/4
;************************************************************
mdlbrt:
    mov #^B1110010,@#timerport3    ;sets timer, /64
    mov @#timerport2,@#time

	mov	#100000, r0	; last screen address
	mov	#max2+6, r6	; set stack and the limit
	mov	@#dya, r5
	swab	r5
	asr	r5		; r5 = 200*dy = b
loop0:
x0a	=	.+2
	mov	#x0, r4		; r4 = a
loop1:
	mov	r0, -(r6)	; push address on stack
	clr	r1		; clear
    mov r1,r3
	inc	r1		;   pixels
loop2:
    mov r1, -(r6)
	mov	r3, -(r6)	; push mask on stack
	add	@#dxa, r4		; update a
nitera	=	.+2
	mov	#niter, r2	; max iter. count
	mov	r4, r0		; r0 = x = a
	mov	r5, r1		; r1 = y = b
1$:
	mov	sqr(r1), r3	; r3 = y^2
	add	r0, r1		; r1 = x+y
	mov	sqr(r0), r0	; r0 = x^2
	add	r3, r0		; r0 = x^2+y^2
	cmp	r0, r6		; if r0 >= 4.0 then
	bge	2$		; overflow

	mov	sqr(r1), r1	; r1 = (x+y)^2
	sub	r0, r1		; r1 = (x+y)^2-x^2-y^2 = 2*x*y
	add	r5, r1		; r1 = 2*x*y+b, updated y
	sub	r3, r0		; r0 = x^2
	sub	r3, r0		; r0 = x^2-y^2
	add	r4, r0		; r0 = x^2-y^2+a, updated x
	sob	r2, 1$		; to next iteration
2$:
    mov (r6)+,r3
    asl	r3		; shift
	asl	r3		;   left
	asl	r3		; shift
	asl	r3		;   left
	bic	#177770,r2	; get 3 bits of color
patt2 = .+2
    bisb	pat1(r2),r3
7$:	mov	(r6)+,r1	; pop mask from stack
	asl	r1		; shift
	asl	r1		;   left
	asl	r1		; shift
	asl	r1		;   left
patt1	=	.+2
	bisb	pat0(r2), r1	; OR the pattern
3$:
	bcc	loop2		; to next pixel
	mov	(r6)+, r0	; pop addr from stack
	mov	r1, -(r0)	; update addr, write to screen
	mov	#^b0011111111000000, r2
	xor	r0, r2		; r2 = address in the top half
	mov	r3, (r2)	; write to screen
	bic	#177700, r2
	bne	loop1		; if not first word in line

	mov	@#patt2,r1
    mov @#patt1,@#patt2
	mov	r1,@#patt1	; switch patterns
	sub	@#dya, r5		; update b
	bgt	loop0		; continue while b > 0

	add	@#mxa, @#x0a	; shift x0

	; scale the params
	mov	#3, r0
	mov	#dxa, r1
4$:
	mov	(r1), r2		; x
	mov	sqr+sf4(r2), (r1)	; (x + sf/4)^2
	sub	sqr-sf4(r2), (r1)+ 	; (x + sf/4)^2 - (x - sf/4)^2 = x*sf
	sob	r0, 4$

	inc	@#nitera	; increase the iteration count
    sub @#timerport2,@#time
    emt 6
    bic #^B1111111100100000,r0
    cmpb #"T",r0
    bne mdlbrt

    mov #18.,r0    ;home cursor
    emt ^O16
    ;mov #155.,r0    ;32/64 screen
    ;emt ^O16
    mov @#nitera,r2
    sub #7,r2
    clr r1
    mov #100.,r3
    call @#pr0
    mov #10.,r3
    call @#pr0
    call @#pr1
    mov #" ",r0    ;clear screen
    emt ^O16
         mov @#time,r2
         clr r3
         asl r2     ;*100
         rol r3
         asl r2
         rol r3
         add @#time,r2
         adc r3
         mov r3,-(sp)
         mov r2,-(sp)
         asl r2
         rol r3
         asl r2
         rol r3
         add (sp)+,r2
         adc r3
         add (sp)+,r3
         asl r2
         rol r3
         asl r2
         rol r3
ticonst = . + 2
         mov #366.,r1             ;3 MHz,3000000/64/128
         call @#div32x16s
         call @#printsec
    emt 6
    ;mov #155.,r0    ;32/64 screen
    ;emt ^O16
    jmp @#mdlbrt

div32x16s: ;R1:R2 = R3:R2/R1, R3 = R3:R2%R1, used: R0,R4
           ;compact form - 64 bytes
                             ;may work wrong if R1>$7fff
     cmp r3,r1
     bcc 32$

     call @#3$
     clr r1
     return

32$: mov r2,r0
     mov r3,r2
     clr r3
     call @#3$
     mov r2,r4
     mov r0,r2
     call @#3$
     mov r4,r1
     return

3$:  call @#.+4
     call @#.+4
     call @#.+4
     call @#.+4
     asl r2
     rol r3
     cmp r3,r1
     bcs 0$

     sub r1,r3
     inc r2
0$:  return

printsec:  ;prints R1:R2/100
        clr r4
        mov #1,r5 
        mov #34464.,r3  ;100000-65536
        call @#pr0
        clr r5 
        mov #10000.,r3
        call @#pr0
        mov #1000.,r3
        call @#pr0
        inc r4
        mov #100.,r3
        call @#pr0
        movb #'.,r0
        emt ^O16
        mov #10.,r3
        call @#pr0
pr1:    mov r2,r0
pr02:   add #48.,r0
        emt ^O16
        inc r4
pr05:     return

pr07:     tst r4
        bne pr02

        tst r0
        beq pr05

        inc r4
        br pr02

pr0:     mov #65535.,r0
4$:	    inc r0
        cmp r1,r5
        bcs pr07
        bne 8$

	cmp r2,r3
	bcs pr07

8$:     sub r3,r2
        sbc r1
        sub r5,r1
	br 4$

;************************************************************
time:   .word 0

dxa:	.word	dx
dya:	.word	dy
mxa:	.word	mx
pat0:	.byte	0,1,2,3,16,5,12,17
pat1:	.byte	0,4,10,14,4,5,12,17

msg:.byte 12.,155.
    .ascii "Superfast Mandelbrot generator"
    .byte 10.
    .ascii "The original version"
    .byte 10.
    .ascii "was published"
    .byte 10.
    .ascii "in 2021 by Stanislav Maslovski."
    .byte 10.
    .ascii "This version just uses several"
    .byte 10.
    .ascii "additional features that were"
    .byte 10.
    .ascii "added by Litwr (2021)."
    .byte 10.
    .ascii "The T-key gives us timings"
    .byte 0
	.end
