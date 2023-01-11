;for fasm assembler
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;Thanks to reddie for some help with optimization
;
;128x256 Mandelbrot for the IBM PC (only the 8086 code), EGA (write mode 0), 16 color mode

         use16
         org 100h

NOCALC = 0
FASTTIMER = 1   ;200Hz instead of 18.21Hz standard

sqr = 800h + 1700h
initer = 7
idx	= -36       ;-.0703125
idy	= 18        ;.03515625
ix0	= -62*idx
imx	= 10*idx		; x move
sf4	= 436/4		; sf/4

start:
    mov ah,9
    mov dx,msg
    int 21h
    xor ah,ah
    int 16h   ;wait a kbd event
    and al,0dfh
    mov [benchmark],al
    
    ;mov ax,3  ;for debug
    mov ax,10h
    int 10h    ;640x350 4 colors
    mov dx,3ceh
    mov ax,5
    out dx,ax   ;mode 0
    ;mov ax,800h
    ;out dx,ax   ;mask, all bits open
if FASTTIMER
                MOV     AX,3508H        ;SAVE/SET INTR8 VECTOR
                INT     21H
                MOV     [SAVE8LO],BX
                MOV     [SAVE8HI],ES
                MOV     DX,intr8
                MOV     AX,2508H
                INT     21H
                cli
                MOV     AL,36H          ;SET TIMER HARDWARE
                OUT     43H,AL
                MOV     AL,5966 AND 0FFH
                OUT     40H,AL               ;1193180Hz/5966=FREQ OF INTR8=199.996648Hz
                MOV     AL,5966 SHR 8
                OUT     40H,AL
                sti
end if
	xor cx,cx   ;clr	r0		; 7 lower bits in high byte
	xor bx,bx   ;clr	r1		; higher 11+1 bits
	xor dx,dx   ;clr	r2		; operand-index
	mov si,sqr  ;mov	#sqr, r4	; for lower half-table
	mov di,si   ;mov	si, r5		; for upper half-table
fillsqr:
	mov [di],bx
    inc di
    inc di      ;mov	r1, (r5)+	; to upper half tbl
	inc dx      ;inc	r2		; R2 = x + 2^-9
	mov bp,dx   ;mov	r2, -(r6)
	shl dx,1    ;asl	r2		; R2 = 2*x + 2^-8
	xchg dl,dh  ;swab	r2		; LLLLLL00 00HHHHHH
	mov al,dl
    cbw         ;movb	r2, r3		; 00000000 00HHHHHH
	add cx,dx   ;add	r2, r0		; add up lower bits
	            ;adc	r1		; add carry to r1
	adc bx,ax   ;add	r3, r1		; R1:R0 = x^2 + 2^-8*x + 2^-16
    dec si
    dec si
	mov [si],bx ;mov	r1, -(r4)	; to lower half tbl
	mov dx,bp   ;mov	(r6)+, r2
	jc mandel   ;bcs	mdlbrt		; exit on overflow

	inc dx      ;inc	r2
	jmp fillsqr ;br	fsqr
mandel:
    mov sp,0x804
    mov [bcount],16
    xor ax,ax
if FASTTIMER
         mov [time],ax
         ;mov [time+2],ax
else
         int 1ah
         ;mov [time+2],cx
         mov [time],dx
end if
mandel1:
    mov ax,0a000h
    mov es,ax
    mov di,80*255+16+160  ;80*255+16 - bottom for top left, 160 is a +2 shift down 
    mov bx,16+160         ;16 - top left
	mov dx,[vdy]   ;mov	@#dya, r5
	xchg dl,dh  ;swab	r5
	shr dx,1    ;asr	r5		; r5 = 200*dy
loop0:
if NOCALC=0
	mov bp,[x0]   ;mov	#x0, r4
end if
loop1:
    push di
    push bx
loop2: ;r0 - si, r1 - di, r2 - cx, r3 - ax, r4 - bp, r5 - dx
if NOCALC=0
	add bp,[vdx] ;add	@#dxa, r4
	mov cx,[niter] ;mov	#niter, r2	; max iter. count
	mov si,bp    ;mov	r4, r0
	mov di,dx    ;mov	r5, r1
.l1:
    lea bx,[sqr+di]
    and bl,ch
	mov ax,[bx]     ;mov	sqr(r1), r3	; r3 = y^2
    add di,si       ;add	r0, r1		; r1 = x+y
    lea bx,[sqr+si]
    and bl,ch
	mov si,[bx]     ;mov	sqr(r0), r0	; r0 = x^2
	add si,ax       ;add	r3, r0		; r0 = x^2+y^2
	cmp si,sp    ;cmp	r0, r6		; if r0 >= 4.0 then
	jnc .l2         ;bge	2$		; overflow

    lea bx,[sqr+di]
    and bl,ch
	mov di,[bx]     ;mov	sqr(r1), r1	; r1 = (x+y)^2
	sub di,si       ;sub	r0, r1		; r1 = (x+y)^2-x^2-y^2 = 2*x*y
    add di,dx	    ;add	r5, r1		; r1 = 2*x*y+b, updated y
	sub si,ax       ;sub	r3, r0		; r0 = x^2
	sub si,ax       ;sub	r3, r0		; r0 = x^2-y^2
	add si,bp       ;add	r4, r0		; r0 = x^2-y^2+a, updated x
    dec cl
	jnz .l1        ;sob	r2, 1$		; to next iteration
.l2:
end if
    ;and cl,15
    mov al,cl
    mov si,[tcolor1]
    shr al,1
    rcr si,1
    mov bx,[tcolor2]
    shr al,1
    rcr bx,1
    mov di,[tcolor3]
    shr al,1
    rcr di,1
    mov cx,[tcolor4]
    shr al,1
    rcr cx,1
    jc .l3

    mov [tcolor4],cx
    mov [tcolor3],di
    mov [tcolor2],bx
    mov [tcolor1],si
    jmp loop2
.l3:
    mov [r4],bp
    mov [r5],dx
    mov dx,bx
    mov bp,di
    pop bx  ;top
    sub bx,2
    pop di  ;bottom
    sub di,2
    push dx
    mov dx,3c4h
    mov ax,802h  ;#3
    out dx,ax
    xchg cl,ch
    mov [es:bx],cx
    mov [es:di],cx
    mov ah,4    ;#2
    out dx,ax
    mov cx,bp
    xchg cl,ch
    mov [es:bx],cx
    mov [es:di],cx
    mov ah,2  ;#1
    out dx,ax
    pop cx
    xchg cl,ch
    mov [es:bx],cx
    mov [es:di],cx
    mov ah,1  ;#0
    out dx,ax
    mov cx,si
    xchg cl,ch
    mov [es:bx],cx
    mov [es:di],cx
    mov word [tcolor4],08000h
    mov dx,[r5]
    mov bp,[r4]
    test bl,15
    je .skip1
	jmp	loop1	; if not first word in line
.skip1:
    add bx,96
    sub di,64
	sub dx,[vdy]    ;sub	@#dya, r5		; update b
    je .skip2
	jmp loop0       ;bgt	loop0		; continue while b > 0
.skip2:
if NOCALC=0
	mov ax,[vmx]
    add [x0],ax       ;add	@#mxa, @#x0a	; shift x0

	; scale the params
	mov cx,3      ;mov	#3, r0
	mov di,vdx    ;mov	#dxa, r1
.l4:
	mov si,[di]         ;mov	(r1), r2
    lea bx,[sqr+sf4+si]
    and bl,0feh
	mov ax,[bx]         ;mov	sqr+sf4(r2), (r1)	; (x + sf/4)^2
    lea bx,[sqr-sf4+si]
    and bl,0feh
	sub ax,[bx]
    mov [di],ax         ;sub	sqr-sf4(r2), (r1)+ 	; (x + sf/4)^2 - (x - sf/4)^2 = x*sf
    add di,2
	loop .l4            ;sob	r0, 4$

	inc byte [niter]     ;inc	@#nitera	; increase the iteration count
end if
    cmp [benchmark],'B'
    jnz .l5

    dec [bcount]
    jnz mandel1
.l5:
if FASTTIMER
         ;cli
         mov dx,[time]
         ;mov cx,[time+2]
         ;sti
else
         xor ax,ax
         int 1ah
         sub dx,[time]
         ;sbb cx,[time+2]
         mov ax,dx
         shl dx,1
         shl dx,1
         add dx,ax
         shl dx,1   ;*10
         xor ax,ax   ;*65536
         mov bx,59659  ;1193180/20
         div bx
         shl dx,1
         cmp dx,59659
         jc .ft1

         inc ax
.ft1:    mov dx,ax
end if
    cmp [benchmark],'B'
    jz .showtime

    xor ah,ah
    int 16h   ;wait a kbd event
    and al,0dfh
    cmp al,'Q'
    jne .noquit

.exit:
if FASTTIMER
                PUSH    DS
                MOV     DX,[SAVE8LO]     ;RESTORE INTR8 VECTOR
                MOV     DS,[SAVE8HI]
                MOV     AX,2508H
                INT     21H
                cli
                POP     DS
                MOV     AL,36H          ;RESTORE TIMER HARDWARE
                OUT     43H,AL
                XOR     AL,AL
                OUT     40H,AL
                OUT     40H,AL
                STI
end if
    mov ax,3
    int 10h    ;std video
    int 20h
.noquit:
    cmp al,'T'
    je .showtime
    jmp mandel
.showtime:
    shr dx,1
    adc dx,0
    push dx
         mov dl,0dh
         call PR00.le
         xor ax,ax
         mov al,[niter]
         sub al,7
         call PR000
         mov dl,' '
         call PR00.le
    pop ax
    xor dx,dx
    mov bx,100
    div bx
    push dx
    call PR000
    mov dl,'.'
    call PR00.le
    pop ax
    call PR00
         xor ah,ah
         int 16h   ;wait a kbd event
    and al,0dfh
    cmp al,'Q'
    je .exit

.l11:    jmp mandel

PR0000:     ;prints ax
        mov si,1000
	CALL PR00.l0
PR000:
        mov si,100
	CALL PR00.l0
PR00:
        mov si,10
	CALL .l0
	mov dl,al
.l2:	add dl,'0'
.le:    mov ah,2
   	int 21h
	mov ax,cx
        retn

.l0:    mov dl,0ffh
.l4:	inc dl
        mov cx,ax
	sub ax,si
	jnc .l4

	mov ax,cx
	jmp .l2

if FASTTIMER
intr8:          inc [cs:time]
;                jnz .l1

;                inc [cs:time+2]
;.l1:
                dec [cs:INTR8COUNT]
                je  .lc

                PUSH    AX
                MOV     AL,20H
                OUT     20H,AL
                POP     AX
                IRET

.lc:            MOV     [cs:INTR8COUNT],11  ;200/11=18.1818 Hz instead of 1193180/65536=18.2065 Hz
                DB      0EAH
SAVE8LO         DW      0
SAVE8HI         DW      0
INTR8COUNT      DB      11    ;65536/5966 = 10.98491
end if

    align 2
vdx: dw	idx
vdy: dw	idy
vmx: dw	imx
x0:  dw ix0
niter: dw initer+0xfe00
r4:  dw 0
r5:  dw 0
tcolor1: dw 0
tcolor2: dw 0
tcolor3: dw 0
tcolor4: dw $8000
time dw 0 ;,0
benchmark db 0
bcount db 0

    align 2
msg     db "  **********************************",13,10
        db "  * Superfast Mandelbrot generator *",13,10
        db "  *        EGA 16 colors, v5       *",13,10
        db "  **********************************",13,10
        db "The original version was published for",13,10
        db "the BK0011 in 2021 by Stanislav Maslovski.",13,10
        db "This IBM PC EGA port was created by Litwr, 2021-23.",13,10
        db "The T-key gives us timings.",13,10
        db "Use the Q-key to quit",13,10
        db "Press B to enter benchmark mode$"

