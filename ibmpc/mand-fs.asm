;for fasm assembler
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;The next code was made by litwr in 2022
;Thanks to reddie for some help with optimization
;
;Fullscreen (640x350) Mandelbrot for the IBM PC (only the 8086 code), EGA (write mode 2), 16 colors

         use16
         org 100h

NOCALC = 0
FASTTIMER = 1  ;200Hz instead of 18.21Hz standard
debug = 0

VMode = 10h
HSize = 640
VSize = 350

sqr = 800h + 1700h
start:
    mov ah,9
    mov dx,msg
    int 21h
    xor ah,ah
    int 16h   ;wait a kbd event

  if debug = 1
    mov ax,3  ;for debug
  else
    mov ax,VMode
  end if
    int 10h    ;640x350 16 colors
    mov dx,3ceh
    mov ax,205h
  if debug = 0
    out dx,ax   ;mode 2
  end if
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
    mov si,[dataindex]
    lodsb
    cbw
    mov [vdx],ax
    lodsb
    cbw
    mov [vdy],ax
    lodsw
    mov [x0],ax
    mov al,[si]
    add byte [si],2
    inc si
    mov [niter],al
    cmp si,dataindex
    jne .l11

    mov si,mdata
.l11:
    mov [dataindex],si
    mov sp,0x804
    xor ax,ax
if FASTTIMER
         mov [time],ax
         ;mov [time+2],ax
else
         int 1ah
         ;mov [time+2],cx
         mov [time],dx
end if
    mov ax,0a000h
    mov es,ax
    mov di,80*349-1     ;bottom
    mov bx,80-1         ;top
    mov al,175
    mul byte [vdy]
    mov dx,ax
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
    and cl,15
    mov ax,[colorm]
    xchg dx,bx    ;faster than MOV on the 8088
    mov dx,3ceh
  if debug = 0
    out dx,ax
  end if
    xchg dx,bx
    pop bx  ;top
    pop di  ;bottom
    mov al,[es:bx]
    mov [es:bx],cl
    mov [es:di],cl
    rol byte [colorm+1],1
    jnc loop1

    test bl,15
    je .l8

.l9:dec bx
    dec di
    jmp loop1    ;if not first word in line

.l8:mov ax,bx
    mov cl,4
    shr ax,cl
    inc cl
    div cl
    or ah,ah
    jne .l9

    add bx,159
    dec di
	sub dx,[vdy]    ;sub	@#dya, r5		; update b
    ;jne loop0       ;bgt	loop0		; continue while b > 0
    je .l0
    jmp loop0
.l0:
    inc [iter]
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
    xor ah,ah
    int 16h   ;wait a kbd event
    and al,0dfh
    cmp al,'Q'
    jne noquit

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
noquit:
    cmp al,'T'
    ;jne mandel
    je showtime
    jmp mandel
showtime:
    shr dx,1
    adc dx,0
    push dx
  if debug = 0
    mov dx,3ceh
    mov ax,5
    out dx,ax   ;mode 0
    mov ax,0ff08h
    out dx,ax   ;bit mask = 0
  end if
         mov dl,0dh
         call PR00.le
         xor ax,ax
         mov al,[iter]
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
.l11:
  if debug = 0
    mov ax,205h
    mov dx,3ceh
    out dx,ax   ;mode 2
  end if
         jmp mandel

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
vdx: dw	0
vdy: dw	0
x0:  dw 0
niter: dw 0xfe00
r4:  dw 0
r5:  dw 0
time dw 0  ;,0
colorm: dw 108h  ;8 - the mask register index

    align 2
msg     db " ************************************",13,10
        db " *  Superfast Mandelbrot generator  *",13,10
        db " *      EGA Fullscreen, 640x350     *",13,10
        db " *   16 colors (write mode 2), v3   *",13,10
        db " ************************************",13,10
        db "This IBM PC EGA/VGA code was created by",13,10
        db "Litwr, 2022. It is based on code published",13,10
        db "for the BK0011 in 2021 by Stanislav",13,10
        db "Maslovski.",13,10
        db "The T-key gives us timings.",13,10
        db "Use the Q-key to quit$"

macro mentry dx,dy,ni {
     db -dx*639/HSize-1, dy*349/VSize+1
     dw dx*320-384   ;dx, dy, x0 = dx*HSize, niter
     db ni
}

iter db 0
;x-min = (x0+x*640)/512, x-max = x0/512, y-max = dy*VSize/2/512
mdata:
     mentry 9, 16, 7   ;1
     mentry 8, 15, 8   ;2
     mentry 7, 14, 9   ;3
     mentry 6, 13, 10  ;4
     mentry 4, 12, 11  ;5
     mentry 3,  7, 12  ;6
     mentry 3,  5, 13  ;7
     mentry 2,  4, 14  ;8
     mentry 2,  3, 15  ;9
     mentry 2,  4, 16  ;10
     mentry 2,  4, 25  ;11
     mentry 2,  4, 37  ;12
dataindex dw mdata

