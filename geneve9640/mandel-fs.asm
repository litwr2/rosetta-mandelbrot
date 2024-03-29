* for xas99 assembler
* General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
* Fullscreen (512x212) Mandelbrot for the Geneve 9640, 16 colors

VDP0 equ >F100
VDP1 equ VDP0+2
VDP2 equ VDP0+4
VDP3 equ VDP0+6

HSize equ 512
fastRAM equ 1
VDP equ 1
mram   equ >F020

   .defm svamx
       mov 1,2       ;in: r0 - page, r1 - address; changes: r0, r1, r2
       andi 1,>3fff
       srl 2,14
       sla 0,2
       a 2,0
       ai 0,>8e00
       swpb 0
       limi 0
       movb 0,@VDP1   ;R0l - >8e, R0h - video page
       swpb 0
       movb 0,@VDP1
       ori 1,>4000
       swpb 1
       movb 1,@VDP1   ;R3h - hi addr OR >40, R3l - lo addr
       swpb 1
       movb 1,@VDP1
       limi 4
   .endm

       DEF MANDEL
MANDEL: li 1,msg
        li 0,>27     *write
        clr 2
        xop @six,0
        bl @getkey

        clr 0
        li 1,8  ;graphic mode 8, 512x212, 16 colors
        xop @six,0

        li 0,>d
        li 1,1
        li 2,>77
        xop @six,0   ;set color 1

        li 0,>c
        clr 1
        xop @six,0  ;border color

       li 0,1      *allocate memory call
       li 1,7      *size
       li 2,1      *at
       seto 3      *fast
       xop @seven,0
       mov 0,0
       jeq !

err:   li 1,merr
       li 0,>27     *write
       clr 2
       xop @six,0
       blwp @0       *error

!      li 7,1
!      li 0,3        *memory mapper call
       mov 7,1       *virtual page #
       mov 7,2       *window #
       xop @seven,0
       mov 0,0
       jne err

       inc 7
       ci 7,8
       jne -!

	clr 0   ;cx; clr	r0		; 7 lower bits in high byte
	clr 1   ;bx; clr	r1		; higher 11+1 bits
	clr 2   ;dx; clr	r2		; operand-index
	li 4,sqrbase  ;si; mov	#sqr, r4	; for lower half-table
	mov 4,5  ;di; mov r4, r5		; for upper half-table
fillsqr:
	mov 1,*5+   ;mov [di],bx  ;inc di  ;inc di  ;mov	r1, (r5)+	; to upper half tbl
	inc 2       ;inc	r2		; R2 = x + 2^-9
	mov 2,6     ;mov bp,dx   ;mov	r2, -(r6)
	sla 2,1     ;shl dx,1    ;asl	r2		; R2 = 2*x + 2^-8
    movb 2,7    ;mov al,dl;    cbw         ;movb	r2, r3		; 00000000 00HHHHHH
    sra 7,8
	swpb 2      ;xchg dl,dh  ;swab	r2		; LLLLLL00 00HHHHHH
    dect 4      ;dec si; dec si
	a 2,0       ;add cx,dx   ;add	r2, r0		; add up lower bits       ;adc	r1		; add carry to r1
    jnc !

    inc 7
!:	a 7,1       ;adc bx,ax   ;add	r3, r1		; R1:R0 = x^2 + 2^-8*x + 2^-16
    mov 1,*4    ;mov [si],bx ;mov	r1, -(r4)	; to lower half tbl
	mov 6,2     ;mov dx,bp   ;mov	(r6)+, r2
	joc mdlbrt  ;bcs	mdlbrt		; exit on overflow

	inc 2       ;inc dx      ;inc	r2
	jmp fillsqr ;br	fsqr

merr text 'memory allocation error'
     byte 13,10,0
    even

mdlbrt:
    mov @dataindex,0
    li 1,mdata
    a 0,1
    mov *1+,@vdx
    mov *1+,@vdy
    mov *1+,@x0
    mov *1,@niter
    inct *1
    ai 0,8
    ci 0,8*dataentries
    jne !

    clr 0
!:  mov 0,@dataindex

  .ifeq fastRAM,1
       li 0,mram
       li 2,(efast-sfast)/2
       li 3,sfast
       li 5,savef
!:     mov *0,*5+
       mov *3+,*0+
       dec 2
       jne -!
  .endif
       li 15,VDP3
       bl @waitvdp
       li 11,VDP0

       limi 0   ;timer
       clr 2
       mov 2,@tihi
       mov 2,@tilo
       clr 12
       sbo 0
       li 2,>3fff
       mov 2,@prevti
       inct 12
       ldcr 2,14
       dect 12
       sbz 0
       li 2,tick
       mov @6,@tickn+2
       mov 2,@6
       limi 4

     li 12,1   ;dot even/odd
     clr 8  ;scrbase
	 mov @vdy,4
     li 7,106
     mpy 7,4      ;r5 = 106*dy
loop0:
     li 9,lbuf+256  ;scridx
     mov @x0,4   ;mov	#x0, r4
loop2 equ $
     .ifeq fastRAM,1
     b @mram
     .else
     b @sfast
     .endif
slowcode:
     jne lx1

     mov 2,14
     jmp loop2
lx1:
     li 12,1
     sla 2,4
     soc 2,14
     swpb 14
     dec 9
     movb 14,*9
     ci 9,lbuf
     jne loop2

     clr 0
     mov 8,1
     .svamx
     li 0,lbuf
!:   movb *0+,*11   *unroll??
     movb *0+,*11
     movb *0+,*11
     movb *0+,*11
     movb *0+,*11
     movb *0+,*11
     movb *0+,*11
     movb *0+,*11
     ci 0,lbuf+256
     jne -!

     li 1,>d300
  .ifeq VDP,0
     s 8,1
     clr 0
     .svamx
     li 0,lbuf
!:   movb *0+,*11   *unroll??
     movb *0+,*11
     movb *0+,*11
     movb *0+,*11
     movb *0+,*11
     movb *0+,*11
     movb *0+,*11
     movb *0+,*11
     ci 0,lbuf+256
     jne -!
  .else
     li 0,>2291   ;34, 128+17
     limi 0
     movb 0,@VDP1
     swpb 0
     movb 0,@VDP1

     s 8,1
     movb 8,*15
     li 0,>e0
     movb 0,*15   ;Y o

     movb 0,*15
     movb 0,*15   ;X d

     movb 1,*15
     movb 0,*15   ;Y d

     li 3,>201
     movb 0,*15
     movb 3,*15   ;X s

     swpb 3
     movb 3,*15
     movb 0,*15   ;Y s

     movb 0,*15
     movb 0,*15

     swpb 0  
     movb 0,*15   ;E0 - YMMM
     limi 4
  .endif

     ai 8,256
     s @vdy,5
     jne loop0

     mov @tickn+2,@6  ;stop timer
  .ifeq fastRAM,1
       li 0,mram
       li 2,(efast-sfast)/2
       li 5,savef
!:     mov *5+,*0+
       dec 2
       jne -!
  .endif

   inc @counter
   bl @getkey
   andi 1,>5f00
   ci 1,>5100  *Q
   jeq exit

   ci 1,>5400  *T
*   jne mdlbrt
   jeq !
   b @mdlbrt
!:
       li 1,space+2    *home
       li 0,>27     *write
       li 2,1       *length
       xop @six,0

       mov @counter,10
       bl @PR00

       li 1,space
       li 0,>27     *write
       li 2,1       *length
       xop @six,0

       mov @tihi,8
       mov @tilo,9
       a 8,8         *x4
       a 9,9
       jnc !

       inc 8
!      a 8,8
       a 9,9
       jnc !

       inc 8
!      clr 7
       li 3,1875      *1875=46875/25, 46875=3000000/64
       div 3,7
       div 3,8
       ci 9,1875/2
       jle !

       inc 8
!      li 10,-1
       li 3,100
!      inc 10
       s 3,8
       joc -!

       dec 7
       joc -!

       a 3,8
       bl @PR0000

       li 1,space+1
       li 0,>27     *write
       li 2,1       *length
       xop @six,0

       mov 8,10
       bl @PR00

       bl @getkey
   andi 1,>5f00
   ci 1,>5100  *Q
   jeq exit
       b @mdlbrt
exit:
       clr 0
       li 1,1  ;text mode 2
       xop @six,0
       blwp @0

svax:   ;in: R0 - page, R1 - addr; changed: R0, R1, R2
       .svamx
       b *11

waitvdp:    ;use: R0, R1
    li 0,>28f   *>8f = >80 + 15
    limi 0
    movb 0,@VDP1
    swpb 0
    movb 0,@VDP1
    nop     *delay??
    movb @VDP1,1
    li 0,>8f   *>8f = >80 + 15
    movb 0,@VDP1
    swpb 0
    movb 0,@VDP1
    limi 4
    andi 1,>100
    jne waitvdp
    b *11

PR00: mov 11,@retsav
      b @PRE

PR0000: mov 11,@retsav    *prints R10; USE: R0,R1,R2,R5
       li 1,1000  *mov #1000,r3
	   bl @digit   *CALL @#0$
       li 1,100   *mov #100,r3
	   bl @digit  
PRE:   li 1,10
	   bl @digit
       mov 10,5    *mov r2,r0
       mov @retsav,11
l12:   mov 5,2
       swpb 2
       ai 2,>3000    *add #'0,r0
       li 1,string
       movb 2,*1
       li 0,>27     *write
       li 2,1       *length
       xop @six,0
       b *11

retsav equ MANDEL+2
string equ MANDEL+4

digit: li 5,65535  *mov #65535,r0
!:	   inc 5       *inc r0
	   c 1,10      *cmp r10,r1
	   jgt l12

	   s 1,10       *sub r1,r10
	   jmp -!

tick:  mov 2,@tick2
       mov 12,@tick12
       clr 12      *USE: R2,R12
       sbo 0
       stcr 2,15
       sbz 0
       srl 2,1
       mov @prevti,12
       mov 2,@prevti
       s 2,12
       andi 12,>3fff
       a 12,@tilo
       jnc !

       inc @tihi
!      mov @tick12,12
       mov @tick2,2
tickn: b @0

prevti equ MANDEL+6
tihi equ MANDEL+8                  *@tihi@
tilo equ MANDEL+10
tick2 equ MANDEL+12
tick12 equ MANDEL+14
savef equ MANDEL+16               *its size is up to 0x60

sfast equ $
     a @vdx,4  ;r4 += dx
     mov @niter,2
     mov 4,10    ;r10 - r0
     mov 5,1
!:   mov @sqrbase(1),3     ;r3 = sqr(r1)
     a 10,1       ;r1 += r0
	 mov @sqrbase(10),10     ;r0 = sqr(r0)
	 a 3,10       ;r0 += r3
     ci 10,>800    ;if r0 >= 4.0 then
	 jhe !

	 mov @sqrbase(1),1     ;r1 = sqr(r1)
	 s 10,1       ;r1 -= r0
     a 5,1	    ;r1 += r5
	 s 3,10       ;r0 -= r3
	 s 3,10       ;r0 -= r3
	 a 4,10       ;r0 += r4
     dec 2
     jne -!

!:   andi 2,15
     dec 12
     b @slowcode
efast equ $

getkey: li 0,4
        li 1,>ff00
        xop @five,0
        jne getkey
        b *11

five data 5
six data 6
seven data 7
vdx bss 2
vdy bss 2
x0  bss 2
niter bss 2

  .defm mentry
     data -#1, #2
     data #1*HSize/2-384   ;dx, dy, x0 = dx*HSize, niter
     data #3
  .endm

;x-min = (x0+dx*HSize)/512, x-max = x0/512, y-max = dy*VSize/1024
dataentries equ 12
counter data 0
dataindex data 0
mdata:
     .mentry 9, 18, 7  ;1
     .mentry 7, 15, 8  ;2
     .mentry 6, 13, 9  ;3
     .mentry 5, 11, 10  ;4
     .mentry 4, 10, 11  ;5
     .mentry 4, 8, 12  ;6
     .mentry 4, 6, 13  ;7
     .mentry 3, 5, 14  ;8
     .mentry 3, 5, 15  ;9
     .mentry 3, 5, 16  ;10
     .mentry 3, 5, 25  ;11
     .mentry 3, 6, 37  ;12

space  data >202e   *space,dot
       byte 1      *home cursor

   even

msg     text "****************************"
        byte 13,10
        text "*   Superfast Mandelbrot   *"
        byte 13,10
        text "*   fullscreen generator   *"
        byte 13,10
        text "*        512x212, v2       *"
        byte 13,10
        text "****************************"
        byte 13,10
        text "This Geneve 9640 code was"
        byte 13,10
        text "created by Litwr, 2022-23. It"
        byte 13,10
        text "is based on code published"
        byte 13,10
        text "for the BK0011 in 2021 by"
        byte 13,10
        text "Stanislav Maslovski."
        byte 13,10
        text "The T-key gives us timings."
        byte 13,10
        text "Use the Q-key to quit"
        byte 0

sqrbase equ msg + >16b0
lbuf equ sqrbase + >1700

     END

