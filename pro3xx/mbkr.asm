;for macro-11 assembler
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;
;256x128 Mandelbrot for the DEC Professional 325/350/380 under RT-11
;256x240 16 shade graphic mode

	.title mandelbrotrs
	.sbttl (C) 2023 litwr
    .radix 10

PRO380 = 1  ;set it to 0 on the Pro-325/350
NOCALC = 0

      .MCall .exit, .rsum, .trpset, .print, .ttyout, .ttyin, .gtim, .gval, .settop
      CONFIG = ^O300
      TTSPC$ =: ^O10000
      $JSW =: ^O44

sqr = 8192    ;the table base

START:   bis #TTSPC$,@#$JSW
         .settop #-2
         ;.rsum
.if eq PRO380    ;PRO380-1 if for xhomer
         mov #63488,R1    ;$f800
         mov #6,R3
2$:      .trpset #area,#3$
         cmpb #2,(R1)  ;ID for the Pro 325/350
         beq 1$

         cmp -(SP),-(SP)
3$:      cmp (SP)+,(SP)+
         add #128,R1    ;$80, upto $fb00
         sob R3,2$

         .print #emsg
         .exit
1$:       .trpset #area,#0
.iff
         mov #64256,r1 ;$fb00
.endc
    mov #2050,sp  ;$802
    mov r1,-(sp)
    .print #smsg
    .ttyin
    bic #^B1111111100100000,r0
    movb r0,@#benchmark
    .print #term1
    call @#sreg
	clr	r0		; 7 lower bits in high byte
	clr	r1		; higher 11+1 bits
	clr	r2		; operand-index
	mov	#sqr, r4	; for lower half-table
	mov	r4, r5		; for upper half-table
fsqr:
	mov	r1, (r5)+	; to upper half tbl
	inc	r2		; R2 = x + 2^-9
	mov	r2, -(sp)
	asl	r2		; R2 = 2*x + 2^-8
	swab	r2		; LLLLLL00 00HHHHHH
	movb	r2, r3		; 00000000 00HHHHHH
	add	r2, r0		; add up lower bits
	adc	r1		; add carry to r1
	add	r3, r1		; R1:R0 = x^2 + 2^-8*x + 2^-16
	mov	r1, -(r4)	; to lower half tbl
	mov	(sp)+, r2
	bcs	mandl	; exit on overflow

	inc	r2
	br	fsqr

niter	=	7
dx	=	-12*3
dy	=	6*3
x0	=	-62*dx
mx	=	10*dx		; x move
sf4	=	436/4		; sf/4 = 109

mandl:
    movb #16,@#bcount
    .gtim #area,#time
mandl1:
    mov #127,@#YC
    clr @#XCL
    mov #1020,@#XCR
	mov	@#dya, r5
	swab	r5
	asr	r5		; r5 = 128*dy = b
loop0:
.if eq NOCALC
x0a	=	.+2
	mov	#x0, r4		; r4 = a
.endc
loop2:
.if eq NOCALC
	add	@#dxa, r4		; update a
.endc
nitera	=	.+2
	mov	#niter, r2	; max iter. count
.if eq NOCALC
	mov	r4, r0		; r0 = x = a
	mov	r5, r1		; r1 = y = b
1$:
.if ne PRO380
    mov r1,r3
    bic #1,r3
    mov sqr(r3),r3
.iff
	mov	sqr(r1), r3	; r3 = y^2
.endc
	add	r0, r1		; r1 = x+y
.if ne PRO380
	bic #1,r0
.endc
	mov	sqr(r0), r0	; r0 = x^2
	add	r3, r0		; r0 = x^2+y^2
	cmp	r0, sp		; if r0 >= 4.0 then
	;cmp r0,#2048      ;$800
	bge	2$		; overflow
	
.if ne PRO380
    bic #1,r1
.endc
	mov	sqr(r1), r1	; r1 = (x+y)^2
	sub	r0, r1		; r1 = (x+y)^2-x^2-y^2 = 2*x*y
	add	r5, r1		; r1 = 2*x*y+b, updated y
	sub	r3, r0		; r0 = x^2
	sub	r3, r0		; r0 = x^2-y^2
	add	r4, r0		; r0 = x^2-y^2+a, updated x
	sob	r2, 1$		; to next iteration
.endc
2$:
	mov @sp,r1
    mov r2,20(r1)
    mov @#YC,16(r1)
    mov @#XCL,14(r1)
    mov #4,18(r1)
    tst 4(r1)   ;transfer done?
    bpl .-4

    mov @#XCR,14(r1)
    mov #4,18(r1)
    dec @#YC
    bpl loop2

    mov #127,@#YC
    add #4,@#XCL
    add #-4,@#XCR
	sub	@#dya,r5		; update b
	bgt	loop0		; continue while b > 0
.if eq NOCALC
	add	@#mxa, @#x0a	; shift x0

	; scale the params
	mov	#3, r0
	mov	#dxa, r1
4$:
	mov	(r1), r2		; x
.if ne PRO380
	add #sqr+sf4,r2
	bic #1,r2
	mov (r2),(r1)
	sub #2*sf4,r2
	sub (r2),(r1)+
.iff
	mov	sqr+sf4(r2), (r1)	; (x + sf/4)^2
	sub	sqr-sf4(r2), (r1)+ 	; (x + sf/4)^2 - (x - sf/4)^2 = x*sf
.endc
	sob	r0, 4$
.endc
	inc	@#nitera	; increase the iteration count
    .gtim #area,#time2
    cmpb #"B",@#benchmark
    bne 6$

    decb @#bcount
    beq 5$
    jmp @#mandl1
6$:
    .ttyin
    bic #^B1111111100100000,r0
    cmpb #"T",r0
    beq 5$

    cmpb #"Q",r0
    beq 7$
    jmp @#mandl
7$:
         call @#rreg
         ;tst (sp)+
         .print #term2
         bic #TTSPC$,@#$JSW
         .exit
5$:
;    mov @sp,r1
;    clr r4
;    mov r4,20(r1)
;12$:mov r4,16(r1)
;    mov #0,14(r1)
;    mov #112,18(r1)
;    tst 4(r1)   ;transfer done?
;    bpl .-4
;
;    inc r4
;    cmp #10,r4
;    bne 12$

    call @#rreg
    .print #chome
    mov @#nitera,r2
    sub #7,r2
    clr r1
    mov #100,r3
    call @#pr0
    mov #10,r3
    call @#pr0
    call @#pr1
    mov #" ",r0
    .ttyout
    sub @#time+2,@#time2+2
    sbc @#time2
    sub @#time,@#time2
    mov @#time2,r2
    mov @#time2+2,r3
    asl r3
    rol r2
         .gval #area,#CONFIG
         mov r0,r4
         mov r3,r1
         mov r2,r0
         bit #32,r4    ;50 or 60 Hz?
         bne 9$

         asl r1     ;*5
         rol r0
         add @#time2+2,r1
         adc r0
         add @#time2,r0
         asl r1    ;quotient is limited to 15 bits!
         rol r0
         mov r0,r3
         clr r2
         div #3,r2
         mov r3,r0
         asr r0
         ror r1
         div #3,r0
         asr r1
         add r0,r1
         clr r0
         asr r2
         ror r0
         add r0,r1
         adc r2
         mov r2,r0
9$:      call @#printsec
    call @#sreg
    .ttyin
    bic #^B1111111100100000,r0
    cmpb #"Q",r0
    beq 7$
    jmp @#mandl

printsec:  ;prints R1:R2/100
        mov r1,r2
        mov r0,r1
        clr r4
        mov #1,r5 
        mov #34464,r3  ;100000-65536
        call @#pr0
        clr r5 
        mov #10000,r3
        call @#pr0
        mov #1000,r3
        call @#pr0
        inc r4
        mov #100,r3
        call @#pr0
        movb #'.,r0
        .ttyout
        mov #10,r3
        call @#pr0
pr1:    mov r2,r0
pr02:   add #48,r0
        .ttyout
        inc r4
pr05:     return

pr07:     tst r4
        bne pr02

        tst r0
        beq pr05

        inc r4
        br pr02

pr0:     mov #65535,r0
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

rreg:  mov 2(sp),r1
       mov #save4,r2
       mov (r2)+,4(r1)
       mov (r2)+,6(r1)
       mov (r2)+,8(r1)
       mov (r2)+,14(r1)
       mov (r2),16(r1)
       return

sreg:  mov 2(sp),r1
       mov #save4,r2
       mov 4(r1),(r2)+
       mov 6(r1),(r2)+
       mov 8(r1),(r2)+
       mov 14(r1),(r2)+
       mov 16(r1),(r2)
     mov #^B0000000000000000,4(r1)   ;240 lines
     mov #^B0000000000010010,6(r1)   ;256 dots, move to screen
     mov #^B0000000000000000,8(r1)  ;disable planes 3 (red) and 2 (green)
       return

time:   .word 0,0
time2: .word 0,0
area:    .word 0,0
addr:    .word 0

dxa:	.word	dx
dya:	.word	dy
mxa:	.word	mx

bcount: .byte 0
benchmark: .byte 0
save4:   .blkw 5
YC:  .word 0
XCL: .word 0
XCR: .word 0

smsg:
    .ascii "Superfast Mandelbrot generator, v3 (Pro-3"
.if ne PRO380
    .ascii "80"
.iff
    .ascii "25/350"
.endc
    .ascii "), 256x128, 16 shades"
    .byte 13,10
    .ascii "The original version was published in 2021 by Stanislav Maslovski."
    .byte 13,10
    .ascii "This RT-11 port for the DEC Pro 325/350/380 was created by Litwr, 2023."
    .byte 13,10
    .ascii "The T-key gives us timings. Press Q-key to quit"
    .byte 13,10
    .asciz "Press B to enter benchmark mode"
emsg:    .asciz "cannot find the graphic system"
eol = . - 1
chome:   .ascii <27> "[H" <128>    ;[home]
term2:   .ascii <27> "[2J" <27> "[?25h" <128>  ;[clear] [show cursor]
term1:   .ascii <27> "[?25l" <27> "[2J" <128>    ;[hide cursor] [clear]
.End	START
