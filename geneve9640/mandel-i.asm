* for xas99 assembler
* General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
* 128x256 Mandelbrot for the Geneve 9640, 16 colors, interlaced (on the 256x384 raster)

VDP0 equ >F100
VDP1 equ VDP0+2
VDP2 equ VDP0+4
VDP3 equ VDP0+6

NOCALC equ 0
fastRAM equ 1
VDP equ 1
mram   equ >F020

initer equ 7
idx	equ -36       *-.0703125
idy	equ 18        *.03515625
ix0	equ -62*idx
imx	equ 10*idx		*x move
sf4	equ 436/4		*sf/4

   .defm svam
       ;li 0,>8e    ;use 16KB VRAM always
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
   andi 1,>5f00
   mov 1,@benchmark

        clr 0
        li 1,6    *graphic mode 6, 256x212, 16 colors
        xop @six,0

        li 0,>d
        li 1,1
        li 2,>77
        xop @six,0   *set color 1

        li 0,>c
        clr 1
        xop @six,0  *border color

        li 0,>36
        li 1,9       *read reg
        xop @six,0
        andi 0,>7f    *256x192
        ori 0,>c     *interlaced
        mov 0,2
        li 0,>35
        li 1,9     *write reg
        xop @six,0

        li 0,>35
        li 1,2     *write reg
        li 2,>3f
        xop @six,0

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

         li 0,>28e   ;clean screen
         clr 1
         bl @sva
         li 0,16384+8192
!:       movb 1,@VDP0
         dec 0
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
  li 1,16
  mov 1,@bcount

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
mdlbrt1:
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
       li 15,VDP0
       bl @waitvdp
       li 11,VDP3

     li 12,1   ;dot even/odd
     clr 13   ;line even/odd
     clr 8  ;scrbase

	 mov @vdy,5
     sla 5,7      ;r5 = 128*dy
loop0:
    li 9,lbuf+64  ;scridx
     .ifeq NOCALC,0
	 mov @x0,4   ;mov	#x0, r4
     .endif
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

     mov 8,1
     ai 13,>8000
     jeq oddli

     li 0,>8e
     .svam

     li 0,lbuf
!:   movb *0+,*15   *unroll??
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     ci 0,lbuf+64
     jne -!

  .ifeq VDP,0
     li 1,>3f80
     xor 8,1
     li 0,>28e
     .svam
     li 0,lbuf
!:   movb *0+,*15   *unroll??
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     ci 0,lbuf+64
     jne -!
  .else
     li 0,>2291   ;34, 128+17
     limi 0
     movb 0,@VDP1
     swpb 0
     movb 0,@VDP1

     mov 8,1
     a 1,1
     movb 1,*11
     li 0,>e0
     movb 0,*11   ;Y o

     li 2,>7f00
     movb 0,*11
     movb 0,*11   ;X d

     s 1,2
     movb 2,*11
     li 3,>180
     movb 3,*11   ;Y d

     swpb 3
     movb 3,*11
     movb 0,*11   ;X s

     swpb 3
     movb 3,*11
     movb 0,*11   ;Y s

     movb 0,*11
     movb 0,*11

     swpb 0  
     movb 0,*11   ;E0 - YMMM
     limi 4
  .endif
     s @vdy,5    ;sub	@#dya, r5
     jmp loop0
oddli:
     li 0,>28e
     .svam
     li 0,lbuf
!:   movb *0+,*15   *unroll??
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     ci 0,lbuf+64
     jne -!
  .ifeq VDP,0
     li 1,>3f80
     xor 8,1
     li 0,>8e
     .svam
     li 0,lbuf
!:   movb *0+,*15   *unroll??
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     movb *0+,*15
     ci 0,lbuf+64
     jne -!
  .else
     li 0,>2291   ;34, 128+17
     limi 0
     movb 0,@VDP1
     swpb 0
     movb 0,@VDP1

     mov 8,1
     a 1,1
     movb 1,*11
     li 3,>180
     movb 3,*11   ;Y o

     li 0,>e0
     movb 0,*11
     li 2,>7f00
     movb 0,*11   ;X d

     s 1,2
     movb 2,*11
     movb 0,*11   ;Y d

     swpb 3
     movb 3,*11
     movb 0,*11   ;X s

     swpb 3
     movb 3,*11
     movb 0,*11   ;Y s

     movb 0,*11
     movb 0,*11

     swpb 0  
     movb 0,*11   ;E0 - YMMM
     limi 4
  .endif
     ai 8,128
     s @vdy,5
     jeq !
     b @loop0
!:
  .ifeq fastRAM,1
       li 0,mram
       li 2,(efast-sfast)/2
       li 5,savef
!:     mov *5+,*0+
       dec 2
       jne -!
  .endif

  .ifeq NOCALC,0
    a @vmx,@x0       ;add	@#mxa, @#x0a	; shift x0

	; scale the params
	li 0,3      ;mov	#3, r0
	li 1,vdx    ;mov	#dxa, r1
!:	mov *1,2         ;mov	(r1), r2
	mov @sqrbase+sf4(2),*1         ; (x + sf/4)^2
    s @sqrbase-sf4(2),*1+         ; (x + sf/4)^2 - (x - sf/4)^2 = x*sf
    dec 0
	jne -!
  .endif

	inc @niter     ;inc	@#nitera	; increase the iteration count
   mov @benchmark,1
   ci 1,>4200
   jne !

   dec @bcount
   jeq !
   b @mdlbrt1

!: mov @tickn+2,@6  ;stop timer
   ci 1,>4200
   jeq !

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

       mov @niter,10
       ai 10,-7
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

sva:   ;in: R1 - addr, changed: R0,R1
       .svam
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
     .ifeq NOCALC,0
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
     .endif
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
vdx data idx
vdy data idy
vmx data imx
x0  data ix0
niter data initer

benchmark data 0
bcount data 0

space  data >202e   *space,dot
       byte 1      *home cursor

   even

msg     text "****************************"
        byte 13,10
        text "*   Superfast Mandelbrot   *"
        byte 13,10
        text "*        generator         *"
        byte 13,10
        text "*     interlaced, v2       *"
        byte 13,10
        text "****************************"
        byte 13,10
        text "The original version was"
        byte 13,10
        text "published for the BK0011 in"
        byte 13,10
        text "2021 by Stanislav Maslovski."
        byte 13,10
        text "This Geneve 9650 port was"
        byte 13,10
        text "created by Litwr, 2022-23."
        byte 13,10
        text "The T-key gives us timings."
        byte 13,10
        text "Use the Q-key to quit"
        byte 13,10
        text "Press B to enter benchmark mode"
        byte 0

sqrbase equ msg + >16b0
lbuf equ sqrbase + >1700

     END

