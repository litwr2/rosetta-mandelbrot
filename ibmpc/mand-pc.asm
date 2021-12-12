;for fasm assembler
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;The next code was made by litwr in 2021
;Thanks to reddie for some help with optimization
;
;128x256 Mandelbrot for the IBM PC (only the 8086 code), EGA, 16 color mode

         use16
         org 100h

sqr = pe + 1700h
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

    ;mov ax,3  ;for debug
    mov ax,10h
    int 10h    ;640x350 4 colors
    mov dx,3ceh
    mov ax,5
    out dx,ax   ;mode 2
    ;mov ax,800h
    ;out dx,ax   ;mask, all bits open

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
         mov ah,2ch
         int 21h
         mov [time+2],cx
         mov [time],dx
    mov ax,0a000h
    mov es,ax
    mov di,80*255+16+160  ;80*255+16 - bottom for top left, 160 is a +2 shift down 
    mov bx,16+160         ;16 - top left
	mov dx,[vdy]   ;mov	@#dya, r5
	xchg dl,dh  ;swab	r5
	shr dx,1    ;asr	r5		; r5 = 200*dy
loop0:
	mov bp,[x0]   ;mov	#x0, r4
loop1:
    push di
    push bx
loop2: ;r0 - si, r1 - di, r2 - cx, r3 - ax, r4 - bp, r5 - dx
	add bp,[vdx] ;add	@#dxa, r4
	mov cx,[niter] ;mov	#niter, r2	; max iter. count
	mov si,bp    ;mov	r4, r0
	mov di,dx    ;mov	r5, r1
.l1:
    lea bx,[sqr+di]
    and bl,0feh
	mov ax,[bx]     ;mov	sqr(r1), r3	; r3 = y^2
    add di,si       ;add	r0, r1		; r1 = x+y
    lea bx,[sqr+si]
    and bl,0feh
	mov si,[bx]     ;mov	sqr(r0), r0	; r0 = x^2
	add si,ax       ;add	r3, r0		; r0 = x^2+y^2
	cmp si,0800h    ;cmp	r0, r6		; if r0 >= 4.0 then
	jnc .l2         ;bge	2$		; overflow

    lea bx,[sqr+di]
    and bl,0feh
	mov di,[bx]     ;mov	sqr(r1), r1	; r1 = (x+y)^2
	sub di,si       ;sub	r0, r1		; r1 = (x+y)^2-x^2-y^2 = 2*x*y
    add di,dx	    ;add	r5, r1		; r1 = 2*x*y+b, updated y
	sub si,ax       ;sub	r3, r0		; r0 = x^2
	sub si,ax       ;sub	r3, r0		; r0 = x^2-y^2
	add si,bp       ;add	r4, r0		; r0 = x^2-y^2+a, updated x
	loop .l1        ;sob	r2, 1$		; to next iteration
.l2:
    and cl,15
    ;mov bx,cx
    ;mov al,[pat+bx]
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

	inc word [niter]     ;inc	@#nitera	; increase the iteration count
l0:	mov ah,2ch
    int 21h   ;get time

    xor ah,ah
    int 16h   ;wait a kbd event
    and al,0dfh
    cmp al,'Q'
    jne noquit

    mov ax,3
    int 10h    ;std video
    int 20h
noquit:
    cmp al,'T'
    je showtime
    jmp mandel
showtime:
    push dx
    push cx
         mov dl,0dh
         call PR00.le
         mov ax,[niter]
         sub al,7
         call PR000
         mov dl,' '
         call PR00.le
    pop cx
    pop dx
    sub dl,byte [time]
         sub dh,byte [time+1]
         sub cl,byte [time+2]
         sub ch,byte [time+3]
         jns .l12

         add ch,24
.l12:    xor ax,ax    ;ch*3600
         xor bx,bx
         mov al,ch
         add al,al
         add al,ch    ;*3
         cbw
         mov bp,ax
         add ax,ax
         add ax,bp    ;*3
         mov bp,ax
         add ax,ax
         add ax,ax
         add ax,bp    ;*5
         mov bp,ax
         add ax,ax
         add ax,ax
         add ax,bp    ;*5
         add ax,ax
         rol bx,1
         add ax,ax
         rol bx,1
         add ax,ax
         rol bx,1
         add ax,ax
         rol bx,1     ;*16 = bx:ax
         push bx
         push ax
         mov al,cl    ;cl*60
         cbw
         mov bp,ax
         add ax,ax
         add ax,bp    ;*3
         mov bp,ax
         add ax,ax
         add ax,ax
         add ax,bp    ;*5
         add ax,ax
         add ax,ax    ;*4 = ax
         pop cx
         pop bx
         push dx
         cwd
         add cx,ax
         adc bx,dx
         pop dx
         push dx
         mov al,dh
         cbw
         cwd
         add cx,ax
         adc bx,dx
         pop dx
         jne .l11

         or dl,dl
         jns .l14

         dec cx
         add dl,100
.l14:    push dx
         mov ax,cx
         call PR0000
         mov dl,'.'
         call PR00.le
         pop ax
         xor ah,ah
         call PR00
         xor ah,ah
         int 16h   ;wait a kbd event
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

    align 2
vdx: dw	idx
vdy: dw	idy
vmx: dw	imx
x0:  dw ix0
niter: dw initer
r4:  dw 0
r5:  dw 0
tcolor1: dw 0
tcolor2: dw 0
tcolor3: dw 0
tcolor4: dw $8000
;pat:	db	0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15
time dw 0,0

    align 2
pe:
msg     db "  **********************************",13,10
        db "  * Superfast Mandelbrot generator *",13,10
        db "  *          EGA 16 colors         *",13,10
        db "  **********************************",13,10
        db "The original version was published for",13,10
        db "the BK0011 in 2021 by Stanislav Maslovski.",13,10
        db "This IBM PC EGA port was created by Litwr, 2021.",13,10
        db "The T-key gives us timings.",13,10
        db "Use the Q-key to quit$"

