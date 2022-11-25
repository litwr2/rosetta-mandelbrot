* for xas99 assembler
* General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
* 128x256 Mandelbrot for the Geneve 9640, 16 colors, interlaced (256x384 raster)

NOCALC equ 0

initer equ 7
idx	equ -36       *-.0703125
idy	equ 18        *.03515625
ix0	equ -62*idx
imx	equ 10*idx		*x move
sf4	equ 436/4		*sf/4

       DEF MANDEL
mram   equ >F020


MANDEL: li 1,msg
        li 0,>27     *write
        clr 2
        xop @six,0
        bl @getkey

        li 0,0
        li 1,6  ;graphic mode 6, 256x212, 16 colors
        xop @six,0   ;???

*        li 0,>36
*        li 1,9      *read reg
*        xop @six,0
*        andi 0,>73
*        ori 0,>c    *256x192, interlaced
*        mov 2,0
*        li 0,>35
*        li 1,9     *write reg
*        xop @six,0

*        li 0,>35
*        li 1,2
*        li 2,>1f  *;$1f - page 0, $3f - page 1
*        xop @six,0

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

*       limi 0
*       li 0,mram
*       li 2,(efast-sfast)/2
*       li 3,sfast
*       li 5,savef
*!      mov *0,*5+
*       mov *3+,*0+
*       dec 2
*       jne -!
*       limi 4

	li 0,0   ;cx; clr	r0		; 7 lower bits in high byte
	li 1,0   ;bx; clr	r1		; higher 11+1 bits
	li 2,0   ;dx; clr	r2		; operand-index
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
mdlbrt:
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

    li 8,255     ;X
	mov @vdy,5
    sla 5,7      ; r5 = 128*dy
loop0:
    li 9,0       ;Y
  .ifeq NOCALC,0
	mov @x0,4   ;mov	#x0, r4
  .endif
loop2:
  .ifeq NOCALC,0
	a @vdx,4 ;add	@#dxa, r4
	mov @niter,2 ;mov	#niter, r2	; max iter. count
	mov 4,10    ;mov	r4, r0
	mov 5,1    ;mov	r5, r1
!:  mov @sqrbase(1),3     ;mov	sqr(r1), r3	; r3 = y^2
    a 10,1       ;add	r0, r1		; r1 = x+y
	mov @sqrbase(10),10     ;mov	sqr(r0), r0	; r0 = x^2
	a 3,10       ;add	r3, r0		; r0 = x^2+y^2
    ci 10,>800    ;cmp	r0, r6		; if r0 >= 4.0 then
	jhe !         ;bge	2$		; overflow

	mov @sqrbase(1),1     ;mov	sqr(r1), r1	; r1 = (x+y)^2
	s 10,1       ;sub	r0, r1		; r1 = (x+y)^2-x^2-y^2 = 2*x*y
    a 5,1	    ;add	r5, r1		; r1 = 2*x*y+b, updated y
	s 3,10       ;sub	r3, r0		; r0 = x^2
	s 3,10       ;sub	r3, r0		; r0 = x^2-y^2
	a 4,10       ;add	r4, r0		; r0 = x^2-y^2+a, updated x
    dec 2
	jne -!        ;sob	r2, 1$		; to next iteration
!:
  .endif
    andi 2,15
*    mov 4,10   ;save R4

    li 0,>e
    mov 2,3
    swpb 3
    mov 8,1   ;X
    mov 9,2   ;Y
*    li 4,0
    li 6,0
    xop @six,0   ;putpixel

    ;li 0,>e
    ;mov 2,3
    ;swpb 3
    li 1,255   ;X
    s 8,1
    ;mov 9,2   ;Y
*   li 4,0
    xop @six,0   ;putpixel

*    mov 10,4   ;restore R4

    inc 9
    ci 9,128
    jne loop2

    dec 8
	s @vdy,5    ;sub	@#dya, r5		; update b
    jne loop0       ;bgt	loop0		; continue while b > 0

   mov @tickn+2,@6  ;stop timer

  .ifeq NOCALC,0
    a @vmx,@x0       ;add	@#mxa, @#x0a	; shift x0

	; scale the params
	li 0,3      ;mov	#3, r0
	li 1,vdx    ;mov	#dxa, r1
!:
	mov *1,2         ;mov	(r1), r2
	mov @sqrbase+sf4(2),*1         ;mov	sqr+sf4(r2), (r1)	; (x + sf/4)^2
    s @sqrbase-sf4(2),*1+         ;sub	sqr-sf4(r2), (r1)+ 	; (x + sf/4)^2 - (x - sf/4)^2 = x*sf
    dec 0
	jne -!            ;sob	r0, 4$

	inc @niter     ;inc	@#nitera	; increase the iteration count
  .endif

   bl @getkey
   andi 1,>5f00
   ci 1,>5100  *Q
   jeq exit

   ci 1,>5400  *T
   jne mdlbrt
*       b @mram

slowcode:
*       li 0,mram
*       li 2,(efast-sfast)/2
*       li 5,savef
*!      mov *5+,*0+
*       dec 2
*       jne -!

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

       li 1,space+2
       li 0,>27     *write
       li 2,1       *length
       xop @six,0

       a 3,8
       bl @PR0000

       li 1,space+1
       li 0,>27     *write
       li 2,1       *length
       xop @six,0

       mov 8,10
       bl @PR00

       bl @getkey
       b @mdlbrt
exit:
       li 0,0
       li 1,1  ;text mode 2
       xop @six,0
       blwp @0

sva:   limi 0
       movb 0,@>f102   ;R0l - >8e, R0h - video page
       swpb 0
       movb 0,@>f102
       nop
       movb 1,@>f102   ;R1l - hi addr OR >40, R1h - lo addr
       swpb 1
       movb 1,@>f102
       limi 4
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

six    data 6
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
savef equ MANDEL+16               *its size is 0x60 ??

sfast: 

       b @slowcode
efast

getkey: li 0,4
        li 1,>ff00
        xop @five,0
        jne getkey
        b *11

five data 5
seven data 7
vdx data idx
vdy data idy
vmx data imx
x0  data ix0
niter data initer

space  data >202e   *space,dot
       byte 1      *home cursor

msg     text "****************************"
        byte 13,10
        text "*   Superfast Mandelbrot   *"
        byte 13,10
        text "*        generator         *"
        byte 13,10
        text "*     interlaced, v1       *"
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
        text "created by Litwr, 2022."
        byte 13,10
        text "The T-key gives us timings."
        byte 13,10
        text "Use the Q-key to quit"
        byte 0

merr text 'memory allocation error'
     byte 13,10,0

sqrbase equ msg + >16b0
*buf equ sqrbase + >1700
*prompt byte >5f,8

     END

