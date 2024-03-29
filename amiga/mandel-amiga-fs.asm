;for VASM assembler, Motorola syntax
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;The next code was made by litwr in 2021
;
;320x256 (Fullscreen) Mandelbrot for the Amiga (PAL, only the 68000 code), 16/32 colors

QCOLORS = 32 ;16
HSize = 320
BLITTER=0    ;it seems the blitter can only do a slight accelleration and the blitter code is larger

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

dmaconr = 2 ;blitter
bltcon0 = $40
bltafwm = $44
bltamod	= $64
bltdmod	= $66
bltdpt = $54
bltapt = $50
bltsize	= $58
bltcon = $96
intena = $9a
intenar = $1c
ports = $dff000

BlitWait macro
	;tst dmaconr(a6)			;for the A1000 compatibility
.\@
	btst #6,dmaconr(a6)
	bne.s .\@
endm

movepenq macro
     ;movea.l GRAPHICS_BASE(pc),a6
	 movea.l RASTER_PORT(pc),a1
     moveq #\1,d0
     moveq #\2,d1
     jsr Move(a6)
endm

color macro
     ;movea.l GRAPHICS_BASE(pc),a6
     movea.l RASTER_PORT(pc),a1
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

         include "system2.s"
start:
   	clr	d0		;clr r0; 7 lower bits in high byte
	clr	d1		;clr r1; higher 11+1 bits
	clr	d2		;clr r2; operand-index
	lea.l sqrbase(pc),a4	;mov	#sqr, r4; for lower half-table
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
    add #6,d0
    cmpi #12*6,d0
    bne loc7

    moveq #0,d0
loc7:
    move d0,dataindex(a3)
    clr.l time(a3)
    moveq #-2,d6   ;-2=$fe
    movea.l #ScreenWidth/8,a2
    movea.l a2,a5	;screen top
  if BLITTER=0
    movea.l #ScreenWidth/8*ScreenHeight,a6 ;screen bottom
  endif
    lea.l sqrbase(pc),a4
	move dy(pc),d5
	asl #7,d5		; r5 = 128*dy
loop0:
	move x0(pc),d4
loop2:
	add dx(pc),d4   ;r4 += dx, d4 - r4
	move niter(pc),d2	;d2 = r2  ;max iter. count
	move d4,d0		;d0 - r0
	move d5,d1		;d1 - r1
loc1:  ;d7 is free
    move d1,d3
    and.b d6,d3
	move (a4,d3.w),d3 ;d3 = r3 = sqr(r1)
	add d0,d1       ;r1 += r0
    and.b d6,d0
	move (a4,d0.w),d0    ;r0 = sqr(r0)
	add d3,d0       ;r0 += r3
	cmp #$800,d0       ;if r0 >= 4.0 then
	bcc	loc2

    and.b d6,d1
	move (a4,d1.w),d1 ;r1 = sqr(r1)
	sub d0,d1       ;r1 -= r0
	sub d3,d0       ;r0 -= r3
	sub d3,d0       ;r0 -= r3
	add d4,d0       ;r0 += r4
	add d5,d1       ;r1 += r5
	dbra d2,loc1
loc2:
    addi #1,d2
	;and #QCOLORS-1,d2      ;get bits of color
    lea.l tcolor1(pc),a0
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
  if BLITTER=0
    subq.l #4,a6   ;?? .w
  endif
    subq.l #4,a5
    lea.l BITPLANE1_PTR(pc),a0
    move.l (a0)+,d2
    move.l d1,(a5,d2.l)
  if BLITTER=0
    move.l d1,(a6,d2.l)
  endif
    move.l (a0)+,d2
    move.l d0,(a5,d2.l)
  if BLITTER=0
    move.l d0,(a6,d2.l)
  endif
    move.l (a0)+,d2
    move.l d3,(a5,d2.l)
  if BLITTER=0
    move.l d3,(a6,d2.l)
  endif
    move.l (a0)+,d2
    move.l d7,(a5,d2.l)
  if BLITTER=0
    move.l d7,(a6,d2.l)
  endif
  if QCOLORS=32
    move.l (a0),d2
    move.l d6,(a5,d2.l)
  if BLITTER=0
    move.l d6,(a6,d2.l)
  endif
    moveq #-2,d6
    move.l #$80000000,tcolor5(a3)
  else
    move.l #$80000000,tcolor4(a3)
  endif
    subq.l #4,a2
    move.l a2,d0
	bne	loop2		; if not first word in line

  if BLITTER=1
    move.l BITPLANE1_PTR(pc),a0
    lea.l (a0,a5.l),a1
    move.l #ScreenWidth*(ScreenHeight-1)/8,d2
    sub.l a5,d2
    lea.l (a0,d2.l),a0
    lea.l $dff000,a6
    move.w #$8400,bltcon(a6)        ;set the highest priority for the blitter
	move.l #$9f00000,bltcon0(a6)	;A->D copy, no shifts, ascending mode
	move.l #$ffffffff,bltafwm(a6)	;no masking of first/last word
	clr bltamod(a6)		;A modulo=bytes to skip between lines
	clr bltdmod(a6)	;D modulo
	move.l a1,bltapt(a6)	;source graphic top left corner
	move.l a0,bltdpt(a6)	;destination top left corner
	move #64+ScreenWidth/16,bltsize(a6)	;rectangle size, starts blit

    move.l BITPLANE2_PTR(pc),a0
    lea.l (a0,a5.l),a1
    lea.l (a0,d2.l),a0
    BlitWait
	move.l a1,bltapt(a6)	;source graphic top left corner
	move.l a0,bltdpt(a6)	;destination top left corner
	move #64+ScreenWidth/16,bltsize(a6)	;rectangle size, starts blit

    move.l BITPLANE3_PTR(pc),a0
    lea.l (a0,a5.l),a1
    lea.l (a0,d2.l),a0
    BlitWait
	move.l a1,bltapt(a6)	;source graphic top left corner
	move.l a0,bltdpt(a6)	;destination top left corner
	move #64+ScreenWidth/16,bltsize(a6)	;rectangle size, starts blit
  if QCOLORS=32
    move.l BITPLANE5_PTR(pc),a0
    lea.l (a0,a5.l),a1
    lea.l (a0,d2.l),a0
    BlitWait
	move.l a1,bltapt(a6)	;source graphic top left corner
	move.l a0,bltdpt(a6)	;destination top left corner
	move #64+ScreenWidth/16,bltsize(a6)	;rectangle size, starts blit
  endif
    move.l BITPLANE4_PTR(pc),a0
    lea.l (a0,a5.l),a1
    lea.l (a0,d2.l),a0
    BlitWait
    move.w #$400,bltcon(a6) ;set the normal priority for the blitter
	move.l a1,bltapt(a6)	;source graphic top left corner
	move.l a0,bltdpt(a6)	;destination top left corner
	move #64+ScreenWidth/16,bltsize(a6)	;rectangle size, starts blit
  endif
    movea.l #ScreenWidth/8,a2
    adda.l #ScreenWidth/4,a5
	sub dy(pc),d5
	bne loop0

	addq #1,iter(a3)      ;increase the iteration count
    move.l time(pc),d5
    bsr getkey
    andi.b #$df,d0
    cmpi.b #"Q",d0
    bne.s noquit
    rts
noquit:
    cmpi.b #"T",d0
    bne mandel

    lsl.l d5
    move iter(pc),d0
    lea.l datae+2(pc),a1
    move d5,(a1)
    move d0,-(a1)
    lea.l fmt(pc),a0
    lea.l stuffChar(pc),a2
    move.l #-1,charCount(a3)
    move.l a3,-(sp)
    lea.l datae+4(pc),a3
    movea.l 4.w,a6
    jsr RawDoFmt(a6)
    movea.l (sp)+,a3
    movea.l RASTER_PORT(pc),a1
    movea.l GRAPHICS_BASE(pc),a6
    movepenq 0,6
    color 2
    move.l charCount(pc),d0
    lea.l datae+4(pc),a0
    lea.l -2(a0,d0.w),a2
    move.b (a2),d1
    move.b #".",(a2)+
    move.b (a2),d2
    move.b d1,(a2)+
    move.b d2,(a2)
    addq #1,d0
    jsr Text(a6)
    bsr getkey
    andi.b #$df,d0
    cmpi.b #"Q",d0
    bne mandel
    rts

getkey:
	move KEYB_OUTBUFFER(pc),D0
	cmp KEYB_INBUFFER(pc),D0	; Is buffer empty
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
     dc.w \1*HSize/2-384   ;dx, dy, x0 = dx*HSize, niter
     dc.b \3,0
  endm

dataindex dc.w 0
iter dc.w 0
data mentry 9, 18, 16 ;1
     mentry 7, 15, 17 ;2
     mentry 6, 13, 18 ;3
     mentry 5, 11, 19 ;4
     mentry 4, 10, 20 ;5
     mentry 4,  8, 21 ;6
     mentry 4,  6, 22 ;7
     mentry 3,  5, 23 ;8
     mentry 3,  5, 24 ;9
     mentry 3,  5, 25 ;10
     mentry 3,  5, 26 ;11
     mentry 4,  5, 36 ;12

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
CONHANDLE   DC.L 0
CONWINDOW	DC.B	'CON:10/10/400/110/Superfast Fullscreen Mandelbrot',0
datae = msg
msg     dc.b "  **********************************",10
        dc.b "  * Superfast Mandelbrot generator *",10
        dc.b "  *     320x256, "
  if QCOLORS=32
        dc.b "32"
  else
        dc.b "16"
  endif
        dc.b" colors, v7     *",10
        dc.b "  **********************************",10
        dc.b "This code for the Amiga was created by",10
        dc.b "Litwr in 2022-24. It is based on code",10
        dc.b "published for the BK0011 in 2021 by",10
        dc.b "Stanislav Maslovski.",10
        dc.b "The T-key gives us timings.",10
        dc.b "Use the Q-key to quit.",10
        dc.b "Press Enter now"
endmsg

         align 1
         DCB.B	$16b0,0
sqrbase: DCB.B	$16b0,0

