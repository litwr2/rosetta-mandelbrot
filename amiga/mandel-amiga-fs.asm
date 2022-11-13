;for VASM assembler, Motorola syntax
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;The next code was made by litwr in 2021
;
;320x256 (Fullscreen) Mandelbrot for the Amiga (only the 68000 code), 16/32 colors

QCOLORS = 32 ;16
HSize = 320

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
ScreenWidth = 320

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
    move dataindex(pc),d0
    lea.l data(pc),a0
    move.b (a0,d0.w),dx+1(a3)
    move.b 1(a0,d0.w),dy+1(a3)
    move.w 2(a0,d0.w),x0(a3)
    move.b 4(a0,d0.w),niter+1(a3)
    addq.b #2,4(a0,d0.w)
    clr.l time(a3)
    moveq #-2,d6   ;-2=$fe
    movea.l #ScreenWidth/8,a2
    movea.l a2,a5	;screen top
    movea.l #ScreenWidth/8*ScreenHeight,a6 ;screen bottom
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
	cmp #$800,d0      ;cmp	r0, r6		; if r0 >= 4.0 then
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
	and #QCOLORS-1,d2      ; get bits of color
    lea.l tcolor1(a3),a0
  if QCOLORS=16
    movem.l (a0)+,d0/d1/d3/d7
  else
    movem.l (a0)+,d0/d1/d3/d6/d7
    lsr d2
    roxr.l d6
  endif
    lsr d2
    roxr.l d1
    lsr d2
    roxr.l d0
    lsr d2
    roxr.l d3
    lsr d2
    roxr.l d7
    bcs.s loc3

  if QCOLORS=32
    movem.l d0/d1/d3/d6/d7,-(a0)
    moveq #-2,d6
  else
    movem.l d0/d1/d3/d7,-(a0)
  endif
    bra loop2
loc3:
    subq.l #4,a6   ;?? .w
    subq.l #4,a5
    lea.l BITPLANE1_PTR(a3),a0
    move.l (a0)+,d2
    move.l d1,(a5,d2.l)
    move.l d1,(a6,d2.l)
    move.l (a0)+,d2
    move.l d0,(a5,d2.l)
    move.l d0,(a6,d2.l)
    move.l (a0)+,d2
    move.l d3,(a5,d2.l)
    move.l d3,(a6,d2.l)
    move.l (a0)+,d2
    move.l d7,(a5,d2.l)
    move.l d7,(a6,d2.l)
  if QCOLORS=32
    move.l (a0),d2
    move.l d6,(a5,d2.l)
    move.l d6,(a6,d2.l)
    moveq #-2,d6
    move.l #$80000000,tcolor5(a3)
  else
    move.l #$80000000,tcolor4(a3)
  endif
    subq.l #4,a2
    move.l a2,d0
	bne	loop2		; if not first word in line

    movea.l #ScreenWidth/8,a2
    adda.l #ScreenWidth/4,a5
	sub dy(a3),d5          ;sub	@#dya, r5
	bne loop0

	addq #1,iter(a3)      ;increase the iteration count
    move dataindex(pc),d0
    add #6,d0
    cmpi #12*6,d0
    bne loc7
    
    moveq #0,d0
loc7:
    move d0,dataindex(a3)
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
    move iter(a3),d0
    move d0,datae(a3)
    move d5,datae+2(a3)
    lea.l datae(a3),a1
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
dx	dc.w	-1
dy	dc.w	0
x0     dc.w   0
niter  dc.w    0
tcolor1 dc.l 0
tcolor2 dc.l 0
tcolor3 dc.l 0
  if QCOLORS=16
tcolor4 dc.l $80000000
  else
tcolor4 dc.l 0
tcolor5 dc.l $80000000
  endif

doslib        dc.l 0
charCount     dc.l 0
mouseleft dc.b 0     ;?? remove
mouseright dc.b 0

  macro mentry
     dc.b -\1, \2
     dc.w \1*HSize/2-1510/\1   ;dx, dy, x0 = dx*HSize, niter
     dc.b \3,0
  endm

dataindex dc.w 0
iter dc.w 0
data mentry 9, 18, 17 ;1
     mentry 7, 15, 18 ;2
     mentry 6, 13, 19 ;3
     mentry 5, 11, 20 ;4
     mentry 4, 10, 21 ;5
     mentry 4,  8, 22 ;6
     mentry 4,  6, 23 ;7
     mentry 3,  5, 24 ;8
     mentry 3,  5, 25 ;9
     mentry 3,  5, 26 ;10
     mentry 3,  5, 27 ;11
     mentry 4,  5, 37 ;12

    align 1
SCREEN_DEFS:
	DC.W	0,0		; X-Y position
	DC.W	ScreenWidth
	DC.W	ScreenHeight
  if QCOLORS=16
	DC.W	4		; Depth
  else
	DC.W	5		; Depth
  endif
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
;	DC.W	$000,$ee0,$0e0,$FFF  ;black, green, yellow, white
;	DC.W	$E00,$e0e,$0EE,$00e  ;red, magenta, cyan, blue
;	DC.W	$800,$808,$088,$008  ;red, magenta, cyan, blue
;	DC.W	$080,$880,$777,$AAA  ;darkgreen, yellow, darkgray, gray
;  if QCOLORS == 32
;    DC.W	$fb7,$7bf,$bf7,$7fb
;  endif
	DC.W	$000,$ff0,$0f0,$FFF  ;black, green, yellow, white
	DC.W	$800,$808,$088,$008  ;red, magenta, cyan, blue
	DC.W	$080,$880,$888,$400  ;darkgreen, yellow, darkgray, darkred
	DC.W	$f00,$f0f,$0ff,$00f  ;red, magenta, cyan, blue
  if QCOLORS == 32
    DC.W	$404,$044,$004,$440
  endif


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
  if QCOLORS=32
BITPLANE5_PTR:      DC.L	0
  endif
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
fmt     dc.b "%d %02d",0   ;even number of bytes!

   align 1
CONHANDLE   DC.L 0
datae = CONHANDLE
msg     dc.b "  **********************************",10
        dc.b "  * Superfast Mandelbrot generator *",10
        dc.b "  *     320x256, "
  if QCOLORS=32
        dc.b "32"
  else
        dc.b "16"
  endif
        dc.b" colors, v3     *",10
        dc.b "  **********************************",10
        dc.b "This code for the Amiga was created by",10
        dc.b "Litwr in 2022. It is based on code",10
        dc.b "published for the BK0011 in 2021 by",10
        dc.b "Stanislav Maslovski.",10
        dc.b "The T-key gives us timings.",10
        dc.b "Use the Q-key to quit.",10
        dc.b "Press Enter now"
endmsg

         align 1
CONWINDOW	DC.B	'CON:10/10/400/110/Superfast Fullscreen Mandelbrot',0

         align 1
t1:
sz = $1520
  if t1-CONHANDLE+sz<$16b0
     fail ERROR
  endif
         DCB.B	sz,0   ;its size + size of CONHANDLE... must be more than $16B0
sqrbase: DCB.B	$16b0,0

