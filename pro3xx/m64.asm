;for macro-11 assembler
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;
;Mandelbrot for the DEC Professional 325/350/380 + EBO under RT-11
;512x240/256 or 512x480/512 if interlaced
;64 colors

	.title mandelbrot64
	.sbttl (C) 2023 litwr
    .radix 10

PRO380 = 1  ;set it to 0 on the Pro-325/350
HSIZE = 512  ;fixed!
VSIZE = 256  ;may be 240 or 256
INTERLACE = 0 ;may be 0 or 1, only works on the Pro-380
SUPER = 0   ;0 is faster but the image is less fine

      .MCall .exit, .rsum, .trpset, .print, .ttyout, .ttyin, .gtim, .gval, .settop
      CONFIG = ^O300
      TTSPC$ =: ^O10000
      $JSW =: ^O44

sqr = 8192    ;the table base
VINT = INTERLACE*PRO380
VVSZ = VSIZE*<VINT+1>


START:   bis #TTSPC$,@#$JSW
         .settop #-2
         ;.rsum
.if eq PRO380
         mov #63488,R1    ;$f800
         mov #6,R3
2$:      .trpset #area,#3$
         cmpb #2,(R1)  ;ID for the Pro 325/350
         beq vidok

         cmp -(SP),-(SP)
3$:      cmp (SP)+,(SP)+
         add #128,R1    ;$80, upto $fb00
         sob R3,2$

         .print #emsg
         .exit
vidok:   .trpset #area,#0
.iff
         mov #64256,r1 ;$fb00
.endc
    bit #^B0010000000000000,4(r1)  ;EBO present
    beq 1$

    .print #e2msg
    .exit

1$: mov #2050,sp  ;$802
    mov r1,-(sp)
    .print #smsg
    .ttyin
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

niter	=	63+<VINT*64>
dx	=	-3
dy	=	5-<2*VINT>
x0	=	-dx*HSIZE/2-384

mandl:
    clr @#YCU
    mov #VVSZ-1,@#YCL
    mov #1020,@#XC
    .gtim #area,#time
	mov #dy,r5
	mul #VVSZ/2,r5
loop0:
	mov	#x0, r4		; r4 = a
loop2:
	add	#dx, r4		; update a
nitera	=	.+2
	mov	#niter, r2	; max iter. count
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
	cmp r0,sp
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
2$:
.if ne SUPER
    call sshift
.endc
	mov @sp,r1
	mov #^B0000000000001010,6(r1)  ;plane 1, the MOV-op
	mov #^B0000100000001000,8(r1)  ;plane 2 & 3, the NOP-op
    mov r2,20(r1)
    mov @#YCU,16(r1)
    mov @#XC,14(r1)
    mov #2,18(r1)
	mov r1,r0
	add #4,r0
    tst (r0)   ;transfer done?
    bpl .-2

    mov @#YCL,16(r1)
    mov r2,20(r1)
    mov #2,18(r1)
    asr r2
    asr r2
    tst (r0)   ;transfer done?
    bpl .-2

    mov #^B0000000000001000,6(r1) ;plane 1, the NOP-op
    mov #^B0000100000001010,8(r1) ;plane 2 & 3, the MOV/NOP-op
    mov r2,20(r1)
    mov #2,18(r1)
    tst (r0)   ;transfer done?
    bpl .-2

    mov r2,20(r1)
    mov @#YCU,16(r1)
    mov #2,18(r1)
    asr r2
    asr r2
    tst (r0)   ;transfer done?
    bpl .-2

    mov #^B0000101000001000,8(r1) ;plane 2 & 3, the NOP/MOV-op
    mov r2,20(r1)
    mov #2,18(r1)
    tst (r0)   ;transfer done?
    bpl .-2

    mov r2,20(r1)
    mov @#YCL,16(r1)
    mov #2,18(r1)
    sub #2,@#XC
    bpl loop2

    mov #1020,@#XC
    inc @#YCU
    dec @#YCL
	sub	#dy,r5		; update b
	bgt	loop0		; continue while b > 0

	inc	@#nitera	; increase the iteration count
    .gtim #area,#time2
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
    mov @sp,r1
	mov #^B0000000000000010,6(r1)  ;plane 1, the MOV-op
	mov #^B0000001000000010,8(r1) ;plane 2 & 3, the MOV/MOV-op
    clr r4
    mov r4,20(r1)
12$:mov r4,16(r1)
    mov #0,14(r1)
    mov #112,18(r1)
    tst 4(r1)   ;transfer done?
    bpl .-4

    inc r4
    cmp #10,r4
    bne 12$

    call @#rreg
    .print #chome
    mov @#nitera,r2
    ;sub #niter,r2
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
.if eq VSIZE-240
     mov #^B0000000000000000!<4*VINT>,4(r1)   ;240 lines
.iff
     mov #^B0000000000000001!<4*VINT>,4(r1)   ;256 lines
.endc
       return

time:   .word 0,0
time2: .word 0,0
area:    .word 0,0
addr:    .word 0

;dxa:	.word	dx
;dya:	.word	dy

save4:   .blkw 5
YCU: .word 0
YCL: .word 0
XC: .word 0

.if ne SUPER
sshift:
    mov r2,r0
    bic #65408,r0   ;$fff8
    movb ts8(r0),r0
    asr r2
    asr r2
    asr r2
    ;bic #65408,r2   ;$fff8
    movb ts8(r2),r2
    asl r2
    add r0,r2
    return

ts8: .byte 0,1,4,5,16,17,20,21
.endc

smsg:
    .ascii "Superfast Mandelbrot generator, 512x"
    .byte VVSZ/100+48,<VVSZ-<<VVDZ/100>*100>>/10+48,VVSZ-<<VVSZ/10>*10>+48
    .ascii ", 64 colors, v4 (Pro-3"
.if ne PRO380
    .ascii "80"
.iff
    .ascii "25/350"
.endc
     .ascii ")"
    .byte 13,10
    .ascii "This RT-11 port for the DEC Pro 325/350/380 + EBO was created by Litwr, 2023."
    .byte 13,10
    .ascii "It is based on code published by Stanislav Maslovski for the BK0011 in 2021."
    .byte 13,10
    .asciz "The T-key gives us timings. Press Q-key to quit"
emsg:    .asciz "cannot find the graphic system"
e2msg:    .asciz "cannot find the EBO"
eol = . - 1
chome:   .ascii <27> "[H" <128>    ;[home]
term2:   .ascii <27> "[2J" <27> "[?25h" <128>  ;[clear] [show cursor]
term1:   .ascii <27> "[?25l" <27> "[2J" <128>    ;[hide cursor] [clear]
.End	START

