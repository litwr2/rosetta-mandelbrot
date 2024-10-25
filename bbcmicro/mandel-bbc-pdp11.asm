;for macro-11 assembler
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;for the BBC Micro PDP-11 second co-pro

OSWRCH = 4   ;A = r0
OSRDCH = 6   ;A = r0
OSWORD = 3   ;A = r0, XY = r1, Y = r2
OSBYTE = 2   ;A = r0, X = r1, Y = r2

VIDEO = 1

	.asect
	.=1000
    call @#init
;************************************************************
; Fixpoint squares up to approx. 8.0^2, 11 significant bits
; operand-index: 0000XXX.X XXXXXXX0
; result:        0XXXXXX.X XXXXXXX0
; In this scale, 1.0 decimal = 1000 octal
;************************************************************
r6	=	%6
sqr	=	20000		; table base
;************************************************************
	mov r6,@#stacks
	;clr	r0		; 7 lower bits in high byte
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
    movb #16.,@#bcount
    mov #timelo,r1
    clr @r1
    clr @#timehi
    mov #2,r0
    emt OSWORD
mdlbrt1:
	mov	#16888.,r0	;$41f8 - the first line
	mov #32767.,r1  ;$7fff - the last line
	mov	#max2+4,r6	; set stack and the limit
	mov	@#dya, r5
	swab	r5
	asr	r5		; r5 = 200*dy = b
loop0:
x0a	=	.+2
	mov	#x0, r4		; r4 = a
loop1:
	mov	r0,-(r6)	;push address of the top line
	mov r1,-(r6)    ;push address of the bottom line
loop2:
	add	@#dxa, r4		; update a
nitera	=	.+2
	mov	#niter, r2	; max iter. count
	mov	r4, r0		; r0 = x = a
	mov	r5, r1		; r1 = y = b
1$:
	;mov	sqr(r1), r3	; r3 = y^2
	mov r1,r3
    bic #1,r3
    mov sqr(r3),r3
	add	r0, r1		; r1 = x+y
	bic #1,r0
	mov	sqr(r0), r0	; r0 = x^2
	add	r3, r0		; r0 = x^2+y^2
	cmp	r0, r6		; if r0 >= 4.0 then
	bge	2$		; overflow

    bic #1,r1
	mov	sqr(r1), r1	; r1 = (x+y)^2
	sub	r0, r1		; r1 = (x+y)^2-x^2-y^2 = 2*x*y
	add	r5, r1		; r1 = 2*x*y+b, updated y
	sub	r3, r0		; r0 = x^2
	sub	r3, r0		; r0 = x^2-y^2
	add	r4, r0		; r0 = x^2-y^2+a, updated x
	sob	r2, 1$		; to next iteration
2$:
.if ne VIDEO
	bic	#^B1111111111110000,r2	; get 4 bits of color, $fff0
.endc
	mov #1,r0
	xor r0,@#xtoggle
	beq 8$
.if ne VIDEO
    movb pat(r2),@#tcolor
.endc
	br	loop2		; to next pixel
8$:
.if ne VIDEO
tcolor = . + 2
    mov #0,r1
    asr r1
    bisb pat(r2),r1
.endc
	mov	(r6)+,@#coio1	; pop the bottom from the stack
.if ne VIDEO
	movb r1,@#coio1+4  ;color
	movb r1,@#coio2+4  ;color
	mov #coio1,r1
	mov #6,r0
	emt OSWORD
.endc
    mov	(r6)+,@#coio2	; pop the top from the stack
.if ne VIDEO
	mov #coio2,r1
	mov #6,r0
	emt OSWORD
.endc	
	mov @#coio2,r0 ;top
	sub #8.,r0
	mov @#coio1,r1 ;bottom
	mov r1,r2
	sub #8.,r1
	xor r1,r2
	bit #256.,r2
	beq	loop1		; if not first word in line

    bit #256.,r0
    beq loop1

    add #513.,r0
    add #511.,r1
    bitb #7,r0
    bne 12$

    add #504.,r0
    sub #504.,r1
12$:
	sub	@#dya,r5		; update b
	bgt	loop0		; continue while b > 0
23$:
	add	@#mxa, @#x0a	; shift x0

	; scale the params
	mov	#3, r0
	mov	#dxa, r1
4$:
	mov	(r1), r2		; x
	;mov	sqr+sf4(r2), (r1)	; (x + sf/4)^2
	;sub	sqr-sf4(r2), (r1)+ 	; (x + sf/4)^2 - (x - sf/4)^2 = x*sf
	add #sqr+sf4,r2
	bic #1,r2
	mov (r2),(r1)
	sub #2*sf4,r2
	sub (r2),(r1)+
	sob	r0, 4$

	inc	@#nitera	; increase the iteration count
         mov #timelo,r1
         mov #1,r0
         emt OSWORD
    cmpb #"B",@#benchmark
    bne 16$

    decb @#bcount
    ;bne mdlbrt1
    beq 15$
    jmp @#mdlbrt1
16$:
    emt OSRDCH
    bic #^B1111111100100000,r0
    cmpb #"Q",r0
    bne 10$:
11$:
   mov #22.,r0
   emt OSWRCH
   mov #7,r0     ;mode 7
   emt OSWRCH
   mov @#stacks,r6
   return
10$:
    cmpb #"T",r0
    beq 15$
    jmp @#mdlbrt
15$:
    mov #31.,r0    ;set the cursor position
    emt OSWRCH
    mov #8.,r0    ;x
   emt OSWRCH
   mov #6,r0     ;y
   emt OSWRCH
    mov @#nitera,r2
    sub #7,r2
    clr r1
    mov #100.,r3
    call @#pr0
    mov #10.,r3
    call @#pr0
    call @#pr1
    mov #" ",r0
    emt OSWRCH
         mov @#timelo,r2
         mov @#timehi,r1
         call @#printsec
    mov #30.,r0  ;hide cursor
    emt OSWRCH
    emt OSRDCH
    bic #^B1111111100100000,r0
    cmpb #"Q",r0
    beq 22$
    jmp @#mdlbrt
22$:
    br 11$
init:
   mov #16.,r0
   clr r1
   emt OSBYTE    ;no ADC
   mov #msg,r2
   call @#1$  ;send
   emt OSRDCH
   bic #^B1111111100100000,r0          ;~$df
   movb r0,@#benchmark
   mov #idata,r2
1$:   ;send
   movb (r2)+,r0
   cmpb #255.,r0
   beq 2$

   emt OSWRCH
   br 1$
2$: return

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
        emt OSWRCH
        mov #10.,r3
        call @#pr0
pr1:    mov r2,r0
pr02:   add #48.,r0
        emt OSWRCH
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
timelo: .word 0
timehi: .word 0
        .word 0  ;padding

dxa:	.word	dx
dya:	.word	dy
mxa:	.word	mx
pat:    .byte   0, 138., 136., 130., 170., 32., 8., 2 
        .byte 160., 10.,  34., 40., 162., 168., 42., 128.

stacks: .word 0
bcount: .byte 0
benchmark: .byte 0
xtoggle: .word 0

coio1:    .byte 0,0,0,0,0
          .byte 0  ;alignment
coio2:    .byte 0,0,0,0,0

msg:.byte 12.
    .ascii "Superfast Mandelbrot generator"
    .byte 10.,13.
    .ascii "  16 colors, v2"
    .byte 10.,13.
    .ascii "The original version was published for"
    .byte 10.,13.
    .ascii "the BK0011 in 2021 by Stanislav"
    .byte 10.,13.
    .ascii "  Maslovski."
    .byte 10.,13.
    .ascii "This BBC Micro PDP-11 2nd co-pro port"
    .byte 10.,13.
    .ascii "  was created by Litwr, 2024."
    .byte 10.,13.
    .ascii "The T-key gives us timings."
    .byte 10.,13.
    .ascii "Use the Q-key to quit."
    .byte 10.,13.
    .ascii "Press B to enter benchmark mode"
    .byte 255.

idata:   .byte 22.,2    ;mode 2
        .byte 23.,0,1,64.,0,0,0,0,0,0   ;64-chars wide
        .byte 23.,0,2,90.,0,0,0,0,0,0
        .byte 23.,0,12.,8.,0,0,0,0,0,0   ;Gr.mem starts at $4000
        .byte 255.   ;the end
	.end

