;for fasmarm assembler
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;The next code was made by litwr in 2022
;
;Fullscreen Mandelbrot for the Acorn Archimedes (only the ARM2 code), 16 colors

;it uses Screen Mode for WriteC 22
;Screen_Mode = 12   ;640x256 16 colors
;Screen_Mode = 16   ;1056x256 16 colors
Screen_Mode = 20    ;640x512 16 colors
;Screen_Mode = 27   ;640x480 16 colors
;Screen_Mode = 35   ;768x288 16 colors
;Screen_Mode = 39   ;896x352 16 colors
NColors = 4
HSize = 640
VSize = 512

VD_ScreenStart = 148

OSByte_ReadKey = 129
OSByte_ClearEscCond = 126
OSByte_CursorSt = 4

OSWord_WritePal = 12

OS_WriteC = 0
OS_Byte = 6
OS_Word = 7
OS_Exit = 0x11
OS_Mouse = 0x1c
OS_ReadVduVariables = 0x31
OS_ReadMonotonicTime = 0x42

;OS_ConvertHex2 = 0xd1
;OS_ConvertHex4 = 0xd2
OS_ConvertHex8 = 0xd4
OS_ConvertCardinal2 = 0xd6
OS_WriteO = 2

processor CPU32_26BIT
processor +cpu32_v1
processor +cpu32_v2

    org 0x8000

Start:
    mov sp,#stack_base and 0xfffffc00
    add sp,#stack_base and 0x3fc
    ;add r1, pc, stack_base-$-8
    bl init

    mov r4,#sqr and 0xfffffc00
    add r4,#sqr and 0x3fc
    ;add r1, pc, sqr-$-8
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

mandel:
    ldr r0,[dataindex]
    ldrb r1,[r0],1
    strb r1,[dxa+2]
    ldrb r1,[r0],1
    strb r1,[dya+2]
    ldrb r1,[r0],1
    strb r1,[x0a+2]
    ldrb r1,[r0],1
    strb r1,[x0a+3]
    eor r6,r6
    ldrb r6,[r0]   ;niter
    add r1,r6,2
    strb r1,[r0],1
    add r1, pc, iter-$-8
    cmp r0,r1
    bne .l11

    add r0, pc, mdata-$-8
.l11:
    str r0,[dataindex]

    swi OS_ReadMonotonicTime
    str r0,[timer]
    mov r7,(HSize*VSize/8*NColors)and 0xfffffc00
    add r7,(HSize*VSize/8*NColors)and 0x3ff
    ldr r12,[screen_addr]
    add r7,r7,r12
    add r12,r12,HSize/8*NColors

    ldr r10,[dxa]
    ldr r5,[dya]
    mov r9,VSize/2
    mul r5,r9
loop0:
    mov r9,HSize*NColors/32
    ldr r4,[x0a]
loop1:
    mov r11,#1
loop2:
    add r4,r4,r10
    mov r2,r6
    mov r0,r4
    mov r1,r5
.l1:
    tst r1,#0x20000
    bic r3,r1,#0x30000
    ldr r3,[r8,r3,asr 16]
    movne r3,r3,lsr #16
    mov r3,r3,lsl #16       ;mov	sqr(r1), r3
    tst r0,#0x20000
    bic lr,r0,#0x30000
    ldr lr,[r8,lr,asr 16]
    movne lr,lr,lsr #16
    mov lr,lr,lsl #16       ;mov	sqr(r0), r0
    add lr,r3,lr            ;add	r3, r0
    tst lr,#0xf8000000      ;cmp	r0, r6
    bne .l2

    add r1,r1,r0            ;add	r0, r1
    tst r1,#0x20000
    bic r1,r1,#0x30000
    ldr r1,[r8,r1,asr 16]
    movne r1,r1,lsr #16
    mov r1,r1,lsl #16       ;mov	sqr(r1), r1
    sub r1,r1,lr            ;sub	r0, r1
    add r1,r1,r5            ;add	r5, r1
    sub r0,lr,r3,lsl #1     ;sub	r3, r0 // sub	r3, r0
    add r0,r0,r4            ;add	r4, r0
    subs r2,#1
    bne .l1
.l2:
    and r2,r2,#15
    movs r11,r11,lsl NColors
    orr r11,r2
    bcc loop2

    str r11,[r12,#-4]!
    str r11,[r7,#-4]!
    subs r9,r9,#1
    bne loop1

    add r12,r12,HSize/4*NColors
    ldr r0,[dya]
    subs r5,r5,r0
    bne loop0

    ldrb r0,[iter]
    add r0,1
    strb r0,[iter]
    swi OS_ReadMonotonicTime
    ldr r1,[timer]
    sub r0,r0,r1
    str r0,[timer]
    bl getkey
    bic r1,#0x20
    cmp r1,#"Q"
    beq exit

    cmp r1,#"T"
    bne mandel

	mov r0, #30   ;VDU = Home Cursor
	swi OS_WriteC
    eor r0,r0
    ldrb r0,[iter]
	add r1, pc, text_string-$-8
	mov r2, #12
	swi OS_ConvertCardinal2
	add r0, pc, text_string-$-8
	swi OS_WriteO
	mov r0, #32
	swi OS_WriteC

    ldr r0,[timer]
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
	swi OS_WriteO
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
	swi OS_WriteO
    bl getkey
    b mandel
	
get_screen_addr:
	str lr, [sp, #-4]!
	add r0, pc, screen_addr_input-$-8
	add r1, pc, screen_addr-$-8
	swi OS_ReadVduVariables
	ldr pc, [sp], #4
	
screen_addr_input:
	dw VD_ScreenStart, -1
screen_addr:
	dw 0
dxa:	dw	0xffff0000
dya:	dw	0
x0a:    dw  0
timer:  dw 0

exit:	
	; wait for vsync (any pending buffers)
	mov r0, #19
	swi OS_Byte
	SWI OS_Exit

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
	SWI OS_WriteC

	bl get_screen_addr
	ldr pc, [sp], #4

set_palette:
	str lr, [sp, #-4]!
	mov r0, #OSWord_WritePal 
	add r1, pc, colors-$-8
	add r2, pc, msg-$-8
.l1:
	swi OS_Word
	add r1, r1, #5
	cmp r1, r2
	bne .l1
	ldr pc, [sp], #4

getkey:
.l3:MOV r0, #OSByte_ReadKey
	MOV r1, #5
	MOV r2, #0
	SWI OS_Byte
    cmp r2,#0x1b
    bne .l2

    mov r0,#OSByte_ClearEscCond
    SWI OS_Byte
    b .l3

.l2:cmp r2,#0xff
    bne .l4

    swi OS_Mouse
    tst r2,#7
    beq .l3

.l4:mov pc,lr

 if 0
debug_write_32:
	add r1, pc, text_string-$-8
	mov r2, #12
	swi OS_ConvertHex8
	add r0, pc, text_string-$-8
	swi OS_WriteO
	mov r0, #32
	swi OS_WriteC
	mov pc, lr
 end if

text_string: 	rb 12
dataindex: dw mdata

macro mentry dx,dy,ni {
     db -dx, dy
     dh dx*HSize/2-384   ;dx, dy, x0 = dx/160, niter
     db ni
}

;x-min = (x0+dx*HSize)/512, x-max = x0/512, y-max = dy*VSize/1024
mdata:    ;dx, dy, iterations
     mentry 7, 14, 7   ;1
     mentry 7, 11, 8   ;2
     mentry 7, 9, 9   ;3
     mentry 6, 8, 10  ;4
     mentry 4, 7, 11  ;5
     mentry 3, 7, 12   ;6
     mentry 3, 5, 13   ;7
     mentry 2, 4, 14   ;8
     mentry 2, 3, 15   ;9
     mentry 2, 4, 16   ;10
     mentry 2, 4, 25   ;11
     mentry 2, 5, 37   ;12
iter db 0

colors:
	db 8, 16, 0, 0, 128
	db 9, 16, 0, 128, 0
	;db 10, 16, 128, 0, 0
	db 11, 16, 128, 128, 0
	db 12, 16, 128, 0, 128
	db 13, 16, 0, 128, 128
	db 14, 16, 128, 128, 128
	db 15, 16, 128, 0, 0

msg     db " ************************************",13,10
        db " *  Superfast Mandelbrot generator  *",13,10
        db " *       Fullscreen, 1056x256       *",13,10
        db " *          16 colors, v1           *",13,10
        db " ************************************",13,10
        db "This Acorn Archimedes code was created",13,10
        db "by Litwr, 2022. It is based on code",13,10
        db "published for the BK0011 in 2021 by",13,10
        db "Stanislav Maslovski.",13,10
        db "The T-key gives us timings.",13,10
        db "Use the Q-key to quit.",0

    align 4
sqr0:
    rb 0x16b0-sqr0+(colors and 0xfffffffc)
sqr:rb 0x16b0
    rb 16
stack_base:
