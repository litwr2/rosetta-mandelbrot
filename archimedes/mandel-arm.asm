Screen_Mode = 9   ;320x200 16 colors

VD_ScreenStart = 148 
;IKey_LeftClick = 0xf6
;IKey_RightClick = 0xf4
;IKey_Space = 0x9d
ErrorV = 0x01

OSByte_ReadKey = 129
OSByte_ClearEscCond = 126
OSByte_CursorSt = 4

OSWord_WritePal = 12

OS_Byte = 6
OS_Word = 7
OS_WriteC = 0
OS_Claim = 0x1f
OS_Release = 0x20
OS_ReadVduVariables = 0x31
OS_ReadMonotonicTime = 0x42
OS_Exit = 0x11

;OS_ConvertHex2 = 0xd1
;OS_ConvertHex4 = 0xd2
OS_ConvertHex8 = 0xd4
OS_ConvertCardinal2 = 0xd6
OS_WriteO = 2

;processor cpu32_v1
;processor cpu32_v2
;processor CPU32_26BIT

    org 0x8000

Start:
    add sp, pc, stack_base-$-8
	B main

    rb 1024
stack_base:

main:
    bl init
    ;bl debug_write_32

    mov r4,#sqr and 0xfffffc00
    add r4,#sqr and 0x3ff
    mov r8,r4
    mov r5,r4
    mov r0,#0
    mov r1,r0
    mov r2,r0
sqrloop:
    mov r9,r1,lsr #16
    strb r9,[r5],#1
    mov r9,r1,lsr #24
    strb r9,[r5],#1      ;mov	r1, (r5)+	; to upper half tbl
	add r2,#0x10000      ;inc	r2		; R2 = x + 2^-9
    mov r6,r2	         ;mov	r2, -(r6)
	mov r2,r2,lsl #1     ;asl	r2		; R2 = 2*x + 2^-8
	mov r3,r2,lsl #8
    mov r12,r2,lsr #24
    add r2,r3,r12,lsl #16  ;swab	r2		; LLLLLL00 00HHHHHH
	mov r3,r2,lsl #8
    mov r3,r3,asr #8     ;movb	r2, r3		; 00000000 00HHHHHH
	adds r0,r2            ;add	r2, r0		; add up lower bits
    addcs r1,r1,#0x10000 ;adc	r1		; add carry to r1
	adds r1,r3           ;add	r3, r1		; R1:R0 = x^2 + 2^-8*x + 2^-16
    mov r9,r1,lsr #24
	strb r9,[r4,#-1]!
    mov r9,r1,lsr #16
    strb r9,[r4,#-1]!    ;mov	r1, -(r4)	; to lower half tbl
	mov r2,r6            ;mov	(r6)+, r2
	bcs mandel           ;bcs	mdlbrt		; exit on overflow

	add r2,#0x10000      ;inc	r2
	b sqrloop            ;br	fsqr

initer	=	7
idx	= -36       ;-.0703125
idy	=	18        ;.03515625
ix0	=	-62*idx
imx	=	10*idx		; x move
sf4	=	436/4*65536		; sf/4
mandel:
    swi OS_ReadMonotonicTime
    str r0,[timer]
    mov r7,#0x9f00
    add r7,#0xa0
    ldr r12, [screen_addr]
    add r7,r7,r12
    add r12,r12,#64

    ldr r10,[dxa]
    ldr r5,[dya]
    mov r5,r5,lsl #7
loop0:
    mov r9,#16
    ldr r4,[x0a]
loop1:
    mov r11,0x1
loop2:
    add r4,r4,r10
    ldr r2,[nitera]
    mov r0,r4
    mov r1,r5
.l1:
    tst r1,#0x20000
    bic r3,r1,#0x30000
    ldr r3,[r8,r3,asr 16]
    movne r3,r3,lsr #16
    mov r3,r3,lsl #16       ;mov	sqr(r1), r3
    add r1,r1,r0   ;??move  ;add	r0, r1
    tst r0,#0x20000
    bic r0,r0,#0x30000
    ldr r0,[r8,r0,asr 16]
    movne r0,r0,lsr #16
    mov r0,r0,lsl #16       ;mov	sqr(r0), r0
    add r0,r3,r0            ;add	r3, r0
    tst r0,#0xf8000000      ;cmp	r0, r6
    bne .l2

    tst r1,#0x20000
    bic r1,r1,#0x30000
    ldr r1,[r8,r1,asr 16]
    movne r1,r1,lsr #16
    mov r1,r1,lsl #16       ;mov	sqr(r1), r1
    sub r1,r1,r0            ;sub	r0, r1
    add r1,r1,r5            ;add	r5, r1
    ;sub r0,r0,r3,lsl #1
    sub r0,r0,r3            ;sub	r3, r0
    sub r0,r0,r3  ;??       ;sub	r3, r0
    add r0,r0,r4            ;add	r4, r0
    subs r2,#1
    bne .l1
.l2:
    and r2,r2,#15
    movs r11,r11,lsl #4
    orr r11,r2
    bcc loop2

    str r11,[r12,#-4]!
    str r11,[r7,#-4]!
    subs r9,r9,#1
    bne loop1

    add r12,r12,#224
    sub r7,r7,#96
    ldr r0,[dya]
    subs r5,r5,r0
    bne loop0

    ldr r0,[nitera]
    add r0,#1
    str r0,[nitera]    ;inc	@#nitera
    swi OS_ReadMonotonicTime
    ldr r1,[timer]
    sub r0,r0,r1
    str r0,[timer]
    bl getkey
    bic r1,#0x20
    cmp r1,#"Q"
    beq exit

    cmp r1,#"T"
    bne .l5

	mov r0, #30   ;VDU = Home Cursor
	swi OS_WriteC
    ldr r0,[nitera]
    sub r0,#7
	add r1, pc, text_string-$-8
	mov r2, #12
	swi OS_ConvertCardinal2
	add r0, pc, text_string-$-8
	swi OS_WriteO   ;??
	mov r0, #32
	swi OS_WriteC

    ldr r0,[timer]
  ;mov r0,#0xfe
  ;add r0,r0,#0x7700
  ;mov r7,r0
  ;bl debug_write_32
  ;mov r0,r7
    mov r4,r0
    add r0,r0,r0,lsr #2
    add r0,r0,r0,lsr #5
    sub r0,r0,r0,lsr #10
    sub r0,r0,r0,lsr #12
    mov r0,r0,lsr #7
    add r2,r0,r0,lsl #2
    add r2,r2,r2,lsl #2
    subs r4,r4,r2,lsl #2
    addmi r4,r4,#100
    submi r0,#1         ;division by 100, it works up to 17298
    add r1, pc, text_string-$-8
	mov r2, #12
	swi OS_ConvertCardinal2
    add r0, pc, text_string-$-8
	swi OS_WriteO  ;??
	mov r0, #"."
	swi OS_WriteC
    cmp r4,#10
    bcs .l6

	mov r0, #"0"
	swi OS_WriteC
.l6:mov r0,r4
    add r1, pc, text_string-$-8
	mov r2, #12
	swi OS_ConvertCardinal2
    add r0, pc, text_string-$-8
	swi OS_WriteO  ;??
    bl getkey
.l5:ldr r1,[mxa]
    ldr r2,[x0a]
    add r2,r1
    str r2,[x0a]   ;add	@#mxa, @#x0a

    mov r5,#3
    add r1,pc,dxa-$-8
.l4:ldr r2,[r1]   ; mov	(r1), r2
    add r3,r2,#sf4
    tst r3,#0x20000
    bic r3,r3,#0x30000     ;??
    ldr r3,[r8,r3,asr 16]
    movne r3,r3,lsr #16
    sub r4,r2,#sf4
    tst r4,#0x20000
    bic r4,r4,#0x30000     ;??
    ldr r4,[r8,r4,asr 16]
    movne r4,r4,lsr #16
    sub r3,r3,r4
    mov r3,r3,lsl #16
    str r3,[r1],#4     ;mov	sqr+sf4(r2), (r1) // sub	sqr-sf4(r2), (r1)+
    subs r5,#1
    bne .l4
	b mandel
	
error_noscreenmem:
	dw 0
	db "Cannot allocate screen memory!"
	align 4
	dw 0

get_screen_addr:
	str lr, [sp, #-4]!
	add r0, pc, screen_addr_input-$-8
    ;adr r0, screen_addr_input
	add r1, pc, screen_addr-$-8
	swi OS_ReadVduVariables
	ldr pc, [sp], #4
	
screen_addr_input:
	dw VD_ScreenStart, -1
screen_addr:
	dw 0
dxa:	dw	idx*65536
dya:	dw	idy*65536
mxa:	dw	imx*65536
x0a:    dw  ix0*65536
nitera: dw  initer
timer:  dw 0

exit:	
	; wait for vsync (any pending buffers)
	mov r0, #19
	swi OS_Byte

	; release our error handler
	mov r0, #ErrorV
	add r1, pc, error_handler-$-8

	SWI OS_Exit

error_handler:
	STMDB sp!, {r0-r2, lr}
	MOV r0, #ErrorV
	add r1, pc, error_handler-$-8
	MOV r2, #0
	SWI OS_Release
	LDMIA sp!, {r0-r2, lr}   ;??
	MOVS pc, lr              ;??

init:
    str lr, [sp, #-4]!
	MOV r0,#22	;VDU = Set MODE
	SWI OS_WriteC
	MOV r0,#Screen_Mode
	SWI OS_WriteC

	bl set_palette

    add r0, pc, msg-$-8
	swi OS_WriteO
    bl getkey

	MOV r0,#23	;VDU = Disable cursor
	SWI OS_WriteC
	MOV r0,#1
	SWI OS_WriteC
	MOV r0,#0
	SWI OS_WriteC
	SWI OS_WriteC
	SWI OS_WriteC
	SWI OS_WriteC
	SWI OS_WriteC
	SWI OS_WriteC
	SWI OS_WriteC
	SWI OS_WriteC

    MOV r0, #OSByte_CursorSt
	MOV r1, #1
	SWI OS_Byte

    ;clear screen
	mov r0, #12   ;VDU = Clear Screen
	SWI OS_WriteC   ;??

	; Claim the Error vector
	MOV r0, #ErrorV
    add r1, pc, error_handler-$-8
	MOV r2, #0
	SWI OS_Claim

	bl get_screen_addr
	ldr pc, [sp], #4

set_palette:
	str lr, [sp, #-4]!
	mov r0, #OSWord_WritePal 
	add r1, pc, colors-$-8
	add r2, pc, colorse-$-8
.l1:
	swi OS_Word
	add r1, r1, #5
	cmp r1, r2
	bne .l1
	ldr pc, [sp], #4

colors:
	db 8, 16, 0, 0, 128
	db 9, 16, 0, 128, 0
	;db 10, 16, 128, 0, 0
	db 11, 16, 128, 128, 0
	db 12, 16, 128, 0, 128
	db 13, 16, 0, 128, 128
	db 14, 16, 128, 128, 128
	db 15, 16, 128, 0, 0
colorse:
    align 4

getkey:
.l3:MOV r0, #OSByte_ReadKey
	MOV r1, #120
	MOV r2, r1
	SWI OS_Byte
    cmp r2,#0x1b
    bne .l2

    mov r0,#OSByte_ClearEscCond
    SWI OS_Byte
    b .l3

.l2:cmp r2,#0xff
    beq .l3
    mov pc,lr

debug_write_32:
	add r1, pc, text_string-$-8
	mov r2, #12
	swi OS_ConvertHex8
	add r0, pc, text_string-$-8
	swi OS_WriteO
	mov r0, #32
	swi OS_WriteC
	mov pc, lr

text_string:
	rb 12

msg     db 12
        db "  **********************************",13,10
        db "  * Superfast Mandelbrot generator *",13,10
        db "  *          16 colors, v1         *",13,10
        db "  **********************************",13,10
        db "The original version was published for",13,10
        db "the BK0011 in 2021 by Stanislav",13,10
        db "Maslovski.",13,10
        db "This Acorn Archimedes port was created",13,10
        db "by Litwr, 2021.",13,10
        db "The T-key gives us timings.",13,10
        db "Use the Q-key to quit.",0

    align 4
sqr0:
    rb 0x16b0-sqr0+msg
sqr:rb 0x16b0

