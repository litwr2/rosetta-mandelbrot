;for VASM assembler, Motorola syntax
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;The next code was made by litwr in 2021
;
;128x256 Mandelbrot for the Amiga (only the 68000 code), 16 colors

OldOpenLibrary	= -408
CloseLibrary	= -414
Forbid = -132
WaitPort =	-384
ReplyMsg =	-378
GetMsg =	-372
FindTask =	-294
OpenDevice =	-444
CloseDevice =	-450
RawDoFmt =	-522
AddIntServer = -168
RemIntServer = -174
INTB_VERTB = 5     ;for vblank interrupt
NT_INTERRUPT = 2   ;node type

;graphics
Text =	-60
Move =	-240
LoadRGB4 = 	-192
SetAPen	=	-342 
SetBPen	=	-348  ;??

;DOS
Write = -48
Read = -42
MODE_OLD = 1005
Open = -30
Close = -36

;intuition
OpenScreen =	-198
CloseScreen =	-66
OpenWindow =	-204
CloseWindow =	-72
ShowTitle =	-282
RAWKEY  	=	$00000400      ;console device
MOUSEBUTTONS	=	$00000008
BACKDROP	=	$00000100
BORDERLESS	=	$00000800
ACTIVATE	=	$00001000
RMBTRAP		=	$00010000
CUSTOMSCREEN	=	$000F
SCREENQUIET	= 	$0100

;console.device
RawKeyConvert =	-$30
IECLASS_RAWKEY = $01

movepenq macro
     ;move.l GRAPHICS_BASE(a3),a6
	 movea.l RASTER_PORT(a3),a1
     moveq #\1,d0
     moveq #\2,d1
     jsr Move(a6)
endm

color macro
     ;move.l GRAPHICS_BASE(a3),a6
     movea.l RASTER_PORT(a3),a1
     moveq #\1,d0
     jsr SetAPen(a6)
endm

mouseleft_char = 1  ;?? remove!
mouseright_char = 2
ScreenHeight = 256
ScreenWidth = 128

initer	= 7
idx	=	-36       ;-.0703125
idy	=	18        ;.03515625, 1 = 1/512
ix0	=	-62*idx
imx	=	10*idx		; x move
sf4	=	436/4		; sf/4

	section Code
    basereg SOD,a3

         lea.l SOD,A3

         include "system1.s"
start:
   	clr	d0		;clr r0; 7 lower bits in high byte
	clr	d1		;clr r1; higher 11+1 bits
	clr	d2		;clr r2; operand-index
	lea.l sqrbase(a3),a4	;mov	#sqr, r4; for lower half-table
	movea.l a4,a5		;mov	r4, r5; for upper half-table
fillsqr:
	move d1,(a5)+   ;mov r1, (r5)+; to upper half tbl
	addq #1,d2		;inc r2; R2 = x + 2^-9
	movea d2,a6         ;mov	r2, -(r6)
    ror #7,d2        ; R2 = 2*x + 2^-8 ; LLLLLL00 00HHHHHH
    move.b d2,d3    
    ext d3          ;movb	r2, r3		; 00000000 00HHHHHH
	add d2,d0       ;add	r2, r0		; add up lower bits
	                ;adc	r1		; add carry to r1
	addx d3,d1       ;add	r3, r1		; R1:R0 = x^2 + 2^-8*x + 2^-16
    bcs mandel0

	move d1,-(a4)   ;mov	r1, -(r4)	; to lower half tbl
	move a6,d2   ;mov	(r6)+, r2
	addq #1,d2      ;inc	r2
	bra.s fillsqr

mandel0:
    move d1,-(a4)   ;mov	r1, -(r4)	; to lower half tbl
mandel:
    clr.l time(a3)
    moveq #-2,d6   ;-2=$fe
    move #$800,a2
    movea.w #16,a5	;screen top
    movea.w #4096,a6 ;screen bottom
    lea.l sqrbase(a3),a4
	move dy(a3),d5
	asl #7,d5		; r5 = 128*dy
loop0:
	move x0(a3),d4
loop2:
	add dx(a3),d4   ;add	@#dxa, r4		; update a
	move niter(a3),d2	; max iter. count
	move d4,d0		; r0 = x = a
	move d5,d1		; r1 = y = b
loc1:
    move d1,d7
    and.b d6,d7
	move (a4,d7.w),d3 ;mov	sqr(r1), r3	; r3 = y^2
	add d0,d1       ;add	r0, r1		; r1 = x+y
    move d0,d7
    and.b d6,d7
	move (a4,d7.w),d0    ;mov	sqr(r0), r0	; r0 = x^2
	add d3,d0       ;add	r3, r0		; r0 = x^2+y^2
	cmp a2,d0      ;cmp	r0, r6		; if r0 >= 4.0 then
	bcc	loc2		; overflow

    move d1,d7
    and.b d6,d7
	move (a4,d7.w),d1 ;mov	sqr(r1), r1	; r1 = (x+y)^2
	sub d0,d1       ;sub	r0, r1		; r1 = (x+y)^2-x^2-y^2 = 2*x*y
	add d5,d1       ;add	r5, r1		; r1 = 2*x*y+b, updated y
	sub d3,d0       ;sub	r3, r0		; r0 = x^2
	sub d3,d0       ;sub	r3, r0		; r0 = x^2-y^2
	add d4,d0       ;add	r4, r0		; r0 = x^2-y^2+a, updated x
    subi #1,d2
	bne loc1        ;sob	r2, 1$		; to next iteration  ??dbra
loc2:
	and #15,d2      ;bic	#177770, r2	; get bits of color
    move.b d2,d0
    move.l tcolor1(a3),d1
    lsr d0
    roxr.l d1
    move.l tcolor2(a3),d2
    lsr d0
    roxr.l d2
    move.l tcolor3(a3),d3
    lsr d0
    roxr.l d3
    move.l tcolor4(a3),d7
    lsr d0
    roxr.l d7
    bcs.s loc3

    move.l d7,tcolor4(a3)
    move.l d3,tcolor3(a3)
    move.l d2,tcolor2(a3)
    move.l d1,tcolor1(a3)
    bra loop2
loc3:
    subq.l #4,a6   ;?? .w
    subq.l #4,a5
    move.l BITPLANE1_PTR(a3),d0
    move.l d1,(a5,d0.l)
    move.l d1,(a6,d0.l)
    move.l BITPLANE2_PTR(a3),d0
    move.l d2,(a5,d0.l)
    move.l d2,(a6,d0.l)
    move.l BITPLANE3_PTR(a3),d0
    move.l d3,(a5,d0.l)
    move.l d3,(a6,d0.l)
    move.l BITPLANE4_PTR(a3),d0
    move.l d7,(a5,d0.l)
    move.l d7,(a6,d0.l)
    move.l #$80000000,tcolor4(a3)
    move a5,d0
    and.b #$f,d0
	bne	loop2		; if not first word in line

    adda #32,a5
	sub dy(a3),d5          ;sub	@#dya, r5
	bne loop0

	move mx(a3),d0
    add d0,x0(a3)          ;add @#mxa, @#x0a	; shift x0

	; scale the params
	move #2,d0         ;mov	#3, r0
	lea.l dx(a3),a1     ;mov	#dxa, r1
loc4:
	move (a1),d2        ;mov	(r1), r2		; x
    move d2,d3
    add #sf4,d2
    and.b #$fe,d2
	move (a4,d2.w),(a1) ;mov	sqr+sf4(r2), (r1)	; (x + sf/4)^2
    sub #sf4,d3
    and.b #$fe,d3
    move (a4,d3.w),d1
	sub d1,(a1)+          ;sub	sqr-sf4(r2), (r1)+ 	; (x + sf/4)^2 - (x - sf/4)^2 = x*sf
	dbra d0,loc4          ;sob	r0, 4$

	addq #1,niter(a3)     ;inc	@#nitera	; increase the iteration count
    move.l time(a3),d5
    bsr getkey
    andi.b #$df,d0
    cmpi.b #"Q",d0
    bne.s noquit
    rts
noquit:
    cmpi.b #"T",d0
    bne mandel

    lsl.l d5
    move niter(a3),d0
    subq #7,d0
    move d0,data(a3)
    move d5,data+2(a3)
    lea.l data(a3),a1
    lea.l fmt(a3),a0
    lea.l stuffChar(pc),a2
    move.l #-1,charCount(a3)
    move.l a3,-(sp)
    lea.l msg(a3),a3
    movea.l 4.w,a6
    jsr RawDoFmt(a6)
    movea.l (sp)+,a3
    movea.l RASTER_PORT(a3),a1
    movea.l GRAPHICS_BASE(a3),a6
    movepenq 0,6
    color 2
    move.l charCount(a3),d0
    lea.l msg(a3),a0
    lea.l -2(a0,d0.w),a2
    move.b (a2),d1
    move.b #".",(a2)+
    move.b (a2),d2
    move.b d1,(a2)+
    move.b d2,(a2)
    addq #1,d0
    jsr Text(a6)
    bsr getkey
	bra mandel

getkey:
	move KEYB_OUTBUFFER(A3),D0
	cmp KEYB_INBUFFER(A3),D0	; Is buffer empty
	bne KEYB_STILLKEYSINBUFFER	; No ??

	bsr KEYB_GETKEYS		; Empty, Wait on a key from
	bra.s getkey

    endb a3

stuffChar:  move.b  d0,(a3)+        ;Put data to an output string, used by RawDoFmt
            addq.l #1,charCount
            rts

rasteri      addq.l #1,(a1)
;If you set your interrupt to priority 10 or higher then a0 must point at $dff000 on exit
      moveq #0,d0  ; must set Z flag on exit!
      rts

VBlankServer:
      dc.l  0,0                   ;ln_Succ,ln_Pred
      dc.b  NT_INTERRUPT,0        ;ln_Type,ln_Pri
      dc.l  0                     ;ln_Name
      dc.l  time,rasteri          ;is_Data,is_Code

SOD:
dx	dc.w	idx
dy	dc.w	idy
mx	dc.w	imx
x0     dc.w   ix0
niter  dc.w    initer
tcolor1 dc.l 0
tcolor2 dc.l 0
tcolor3 dc.l 0
tcolor4 dc.l $80000000

doslib        dc.l 0
charCount     dc.l 0
mouseleft dc.b 0     ;?? remove
mouseright dc.b 0

    align 1
SCREEN_DEFS:
	DC.W	0,0		; X-Y position
	DC.W	ScreenWidth
	DC.W	ScreenHeight
	DC.W	4		; Depth
	DC.B	0,1		; Pen colors
	DC.W	0		; V_HIRES
	DC.W	SCREENQUIET	;CUSTOMSCREEN
	DC.L	FONT_ATTR	; use Topaz 8 as standard font
	DC.L	0 ;SCREEN_NAME
	DC.L	0
	DC.L	0

WINDOW_DEFS:
	dc.w	0,0		; X-Y position
	dc.w	ScreenWidth		; Current width
	dc.w	ScreenHeight		; Current higth
	dc.b	0,1
	dc.l	RAWKEY+MOUSEBUTTONS		; Report only raw keys
	dc.l	BACKDROP+BORDERLESS+ACTIVATE+RMBTRAP
	dc.l	0	;Intuition Direct Communications Message Port
	dc.l	0
	DC.L	0	;REQUESTER_NAME	; Window name
SCREEN_HANDLE:
	dc.l	0	;custom screen pointer
	dc.l	0
	dc.w	ScreenWidth		; Min width 
	dc.w	ScreenHeight		; Min higth
	dc.w	ScreenWidth		; Max width
	dc.w	ScreenHeight		; Max higth
	dc.w	CUSTOMSCREEN	; A normal window
	EVEN

;---  Topaz font  ---

FONT_ATTR:
	DC.L	FONT_NAME	; Name
	DC.W	8		; Size
	DC.B	0
	DC.B	0
	DC.W	8		; Size

COLORS:
	DC.W	$000,$ee0,$0e0,$FFF  ;black, green, yellow, white
	DC.W	$E00,$e0e,$0EE,$00e  ;red, magenta, cyan, blue
	DC.W	$800,$808,$088,$008  ;red, magenta, cyan, blue
	DC.W	$080,$880,$777,$AAA  ;darkgreen, yellow, darkgray, gray


FONT_NAME		DC.B	'topaz.font',0
CONSOLE_NAME		DC.B	'console.device',0
INTUITION_NAME		DC.B	'intuition.library',0
GRAPHICS_NAME		DC.B	'graphics.library',0
dosname  dc.b "dos.library",0

     CNOP 0,4
CONSOLE_DEVICE:     DC.L	0
INTUITION_BASE:     DC.L	0
GRAPHICS_BASE:      DC.L	0
TASK_OLDWINDOW:     DC.L	0

BITPLANE1_PTR:      DC.L	0
BITPLANE2_PTR:      DC.L	0
BITPLANE3_PTR:      DC.L	0
BITPLANE4_PTR:      DC.L	0
TASK_PTR:           DC.L	0
ERROR_STACK:        DC.L	0

KEYB_BUFFER:		DCB.B	KB2_SIZE,0
KEYB_OUTBUFFER:		DC.W	0
KEYB_INBUFFER:		DC.W	0

IO_REQUEST:		DCB.B	32,0
KEY_BUFFER:		DCB.B	80,0
KEY_PORT:		DC.L	0
KEY_MSG:		DC.L	0
RASTER_PORT:		dc.l	0
VIEW_PORT:		dc.l 0
wbmsg: dc.l 0
MY_EVENT:	DC.L	0	; Insert after each event
EVENT_IECLASS:	DC.B	IECLASS_RAWKEY
		DC.B	0	; SUBCLASS - A Joke
IECODE:		DC.W	0	; RAWKEY - Inserted
IEQUAL:		DC.W	0	; QUALIFIER - SHIFT, CTRL, ETC.
IEADDR:		DC.L	0	; IAddress
		DC.L	0
		DC.L	0	; TimeStamp
WINDOW_HANDLE:	DC.L	0
time dc.l 0
fmt     dc.b "%d %d",0   ;even number of bytes!
CONHANDLE   DC.L 0
data = CONHANDLE
msg     dc.b "  **********************************",13,10
        dc.b "  * Superfast Mandelbrot generator *",13,10
        dc.b "  *          16 colors, v2         *",13,10
        dc.b "  **********************************",13,10
        dc.b "The original version was published for",13,10
        dc.b "the BK0011 in 2021 by Stanislav Maslovski.",13,10
        dc.b "This Amiga port was created by Litwr, 2021.",13,10
        dc.b "The T-key gives us timings.",13,10
        dc.b "Use the Q-key to quit.",13,10
        dc.b "Press Enter now"
CONWINDOW	DC.B	'CON:10/10/400/100/Superfast Mandelbrot',0    ;must be after msg!!

         align 1
t1:
sz = $1530
  if t1-CONHANDLE+sz<$16b0
     fail ERROR
  endif
         DCB.B	sz,0   ;its size + size of CONHANDLE... must be more than $16B0
sqrbase: DCB.B	$16b0,0

