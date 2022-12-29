;for MPW assembler
;
;General Mandelbrot calculation idea was taken from https://www.pouet.net/prod.php?which=87739
;The next code was made by litwr in 2022
;
;512x256 Mandelbrot for the Macintosh (only the 68000 code)
;512x342 2 colors, 4x1 texture bricks to simulate 8 colors
;it doesn't work on the color Macintosh

NOCALC equ 0

lutsz equ 11768

         INCLUDE 'Traps.a'         ;define Toolbox traps
         INCLUDE 'SysEqu.a'        ;define ScrnBase
         INCLUDE 'QuickEqu.a'

vOff    equ 38

initer	equ 7
idx	equ	-36       ;-.0703125
idy	equ	18        ;.03515625, 1 = 1/512
ix0	equ	-62*idx
imx	equ	10*idx		; x move
sf4	equ	436/4		; sf/4

         MAIN
start    PEA -4(A5)
         _InitGraf
         _InitFonts
         _InitWindows
         _InitCursor

         ;movea.l GrafGlobals(a5),a0
         ;move.l ScreenBits+bounds+4(a0),d0
         lea WindowSize(pc),a0
         SUBQ #4,SP
         CLR.L -(SP)     ;storage
         move.l a0,-(sp)
         PEA WindowName(pc)
         ST -(SP)        ;visible
         CLR.W -(SP)     ;window type
         MOVE.L #-1,-(SP);behind window
         SF -(SP)        ;has close box - no
         CLR.L -(SP)     ;refcon
         _NewWindow
         lea WindPtr(pc),a6
         move.l (sp),(a6)
         _SetPort
         ;move #srcXor,-(sp)
         ;_TextMode
         move #4,-(sp)   ;Monaco font (monospace)
         _TextFont

         move #lutsz,d0
         _NewPtr
         tst d0
         bne ExitErr

         lea.l MemPtr(pc),a1
         move.l a0,(a1)

         lea.l msg(pc),a2
         move.l #$a0000,a4
@l2      move.l a4,-(sp)
         _MoveTo
         move (a2)+,d0
         lea.l msg1(pc),a6
         lea.l (a6,d0.w),a0
         move.l a0,-(sp)
         _DrawString
         add.l #$c0000,a4
         cmpa.l a6,a2
         bne.s @l2

         bsr getkey

   	clr	d0		;clr r0; 7 lower bits in high byte
	clr	d1		;clr r1; higher 11+1 bits
	clr	d2		;clr r2; operand-index
         movea.l MemPtr(pc),a4
         lea.l $16b0(a4),a4
         movea.l a4,a6
fillsqr:
	move d1,(a6)+   ;mov r1, (r5)+; to upper half tbl
	addq #1,d2		;inc r2; R2 = x + 2^-9
	movea d2,a0         ;mov	r2, -(r6)
    ror #7,d2        ; R2 = 2*x + 2^-8 ; LLLLLL00 00HHHHHH
    move.b d2,d3    
    ext d3          ;movb	r2, r3		; 00000000 00HHHHHH
	add d2,d0       ;add	r2, r0		; add up lower bits
	                ;adc	r1		; add carry to r1
	addx d3,d1       ;add	r3, r1		; R1:R0 = x^2 + 2^-8*x + 2^-16
    bcs.s mandel0

	move d1,-(a4)   ;mov	r1, -(r4)	; to lower half tbl
	move a0,d2   ;mov	(r6)+, r2
	addq #1,d2      ;inc	r2
	bra.s fillsqr

mandel0:
    move d1,-(a4)   ;mov	r1, -(r4)	; to lower half tbl
    lea.l cpat0(pc),a1
    lea.l pat0(pc),a0
    move.l a0,(a1)+
    lea.l pat1(pc),a0
    move.l a0,(a1)
mandel:
    _HideCursor
         lea stime(pc),a6
         move.l Ticks,(a6)

    moveq #-2,d6   ;-2=$fe
    move #$800,a2
    move.l WindPtr(pc),a3
    move 6(a3),d1   ;BytesInRow
    movea.l 2(a3),a3  ;bitmap pointer
    move #vOff+255,d0
    mulu d1,d0
    lea.l 64(a3,d0.l),a6  ;screen bottom
    move #vOff,d0
    mulu d1,d0
    lea.l 64(a3,d0.w),a3  ;screen top
   
    ;move.l Scrnbase,a3
    ;move #voff+255,d0
    ;move 6(a3),d1  ;BytesInRow
    ;mulu d1,d0
    ;lea.l 64(a3,d0.l),a6  ;screen bottom
    ;move #voff,d0
    ;mulu d1,d0
    ;lea.l 64(a3,d0.l),a3  ;screen top

         movea.l MemPtr(pc),a4   ;sqrbase
         lea.l $16b0(a4),a4
	move dy(pc),d5
	asl #7,d5		; r5 = 128*dy
loop0:
  lea.l dotcnt(pc),a0
  move #64,(a0)
  if NOCALC=0 then
	move x0(pc),d4
  endif
loop2:
  if NOCALC=0 then
	add dx(pc),d4   ;add	@#dxa, r4		; update a
	move niter(pc),d2	; max iter. count
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
	bcc.s loc2		; overflow

    move d1,d7
    and.b d6,d7
	move (a4,d7.w),d1 ;mov	sqr(r1), r1	; r1 = (x+y)^2
	sub d0,d1       ;sub	r0, r1		; r1 = (x+y)^2-x^2-y^2 = 2*x*y
	add d5,d1       ;add	r5, r1		; r1 = 2*x*y+b, updated y
	sub d3,d0       ;sub	r3, r0		; r0 = x^2
	sub d3,d0       ;sub	r3, r0		; r0 = x^2-y^2
	add d4,d0       ;add	r4, r0		; r0 = x^2-y^2+a, updated x
    subi #1,d2
	bne.s loc1        ;sob	r2, 1$		; to next iteration  ??dbra
loc2:
  endif
	andi #7,d2      ;bic	#177770, r2	; get bits of color
    move.l cpat0(pc),a0
    move.b (a0,d2.w),d1
    move.l cpat1(pc),a0
    move.b (a0,d2.w),d2
    lea.l tcolor0(pc),a0
    move.l (a0),d0
    lsr.b #1,d2
    roxr.l #1,d0
    lsr.b #1,d2
    roxr.l #1,d0
    lsr.b #1,d2
    roxr.l #1,d0
    lsr.b #1,d2
    roxr.l #1,d0
    move.l 4(a0),d3   ;tcolor1
    lsr.b #1,d1
    roxr.l #1,d3
    lsr.b #1,d1
    roxr.l #1,d3
    lsr.b #1,d1
    roxr.l #1,d3
    lsr.b #1,d1
    roxr.l #1,d3
    bcs.s @l18

    move.l d0,(a0)+
    move.l d3,(a0)
    bra loop2
@l18
    move.l d3,-(a6)
    move.l d0,-(a3)
    move.l #$80000000,4(a0)
    lea.l dotcnt(pc),a0
    subq.w #4,(a0)
    bne loop2

    lea.l cpat0(pc),a0
    move.l (a0),d1
    move.l 4(a0),(a0)
    move.l d1,4(a0)
    move.l WindPtr(pc),a0
    move 6(a0),d0   ;BytesInRow
    lea.l 64(a3,d0.w),a3
    neg d0
    lea.l 64(a6,d0.w),a6
	sub dy(pc),d5          ;sub	@#dya, r5
	bne loop0

  if NOCALC=0 then
    lea.l mx(pc),a0
	move (a0),d0
    add d0,2(a0)          ;add @#mxa, @#x0a	; shift x0

	; scale the params
	move #2,d0         ;mov	#3, r0
	lea.l dx(pc),a1     ;mov	#dxa, r1
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

    addq #2,a1
	addq #1,(a1)     ;inc	@#nitera	; increase the iteration count
  endif

         lea.l stime(pc),a6
         move.l Ticks,d6
         sub.l (a6),d6
         _ShowCursor
         bsr getkey
    andi.b #$df,d0
    cmpi.b #'Q',d0
    beq ExitOk

    cmpi.b #'T',d0
    bne mandel

    pea TextFrame(pc)
    _EraseRect

         lea.l txtpos(pc),a0
         move.l #$a000a,(a0)

    clr.l d5
    move niter(pc),d5
    subq #7,d5
    bsr.s PR000

         move.b #32,d1  ;space
         bsr print1

         move.l d6,d3
         lsl.l #2,d6     ;60 Hz
         add.l d3,d6
         divu #3,d6
         swap d6
         lsr #2,d6
         swap d6
         negx.l d6
         neg.l d6

         lea msg4(pc),a3
         moveq.l #10,d4
         divu d4,d6
         swap d6
         move.b d6,(a3)+
         clr d6
         swap d6
         divu d4,d6
         swap d6
         move.b d6,(a3)+
         clr d6
         swap d6
         move.b #'.'-'0',(a3)+
@l12     tst d6
         beq.s @l11

         divu d4,d6
         swap d6
         move.b d6,(a3)+
         clr d6
         swap d6
         bra.s @l12

@l11     add.b #'0',-(a3)
         moveq #0,d0
         move.b (a3),d1
		 bsr.s print1
		 lea msg4(pc),a6
         cmpa.l a6,a3
         bne.s @l11

         MOVE.L #$FFFF,d0
         _FlushEvents
         bsr.s getkey
         bra mandel
ExitOk
         movea.l MemPtr(pc),a0
         _DisposPtr
ExitErr
         _ExitToShell          ;return to Desktop/Shell

PR0000     ;prints d5, uses d1
       ;_SystemTask
       divu #1000,d5
       bsr.s p0
       clr d5
       swap d5
PR000
       divu #100,d5
       bsr.s p0
       clr d5
       swap d5
PR00
       divu #10,d5
       bsr.s p0
       swap d5

p0     eori.b #'0',d5
       moveq #0,d1
       move.b d5,d1

print1 lea.l txtpos(pc),a6
       move.l (a6),-(sp)   ;prints D1, uses D1, A6
       _MoveTo
       move d1,-(sp)
       _DrawChar
       clr -(sp)
       move d1,-(sp)
       _CharWidth

       move 2(a6),d1
       add (sp)+,d1
       move d1,2(a6)
       rts

stime  dc.l 0
dotcnt ds.w 1
WindPtr    DS.L 1
txtpos    DS.l 1
WindowSize DC.W vOff,0,vOff+256,512
TextFrame DC.w 0,2,12,92
MemPtr     ds.l 1

getkey  _SystemTask      ;waits a key/mouse event and returns a char in D0
        subq #2,sp
        move #$ffff,-(sp)
		pea EventRecord(pc)
        _GetNextEvent
        btst #0,(sp)+
        beq.s getkey

        move EventRecord(pc),d3
        cmpi #mButDwnEvt,d3
        bne.s @l2

        subq #4,sp
        move.l sp,-(sp)
        _GetMouse
        move.l (sp),d0
        addq #2,sp
        move.l d0,-(sp)
        pea WindowSize(pc)
        _PtInRect
        tst.b (sp)+
        beq.s getkey

        clr d0
        rts

@l2     cmpi #keyDwnEvt,d3
        bne.s getkey

        move.l EventRecord+evtMessage(pc),d0
        rts

dx	dc.w	idx
dy	dc.w	idy
mx	dc.w	imx
x0     dc.w  ix0
niter  dc.w  initer
tcolor0 ds.l 1
tcolor1 dc.l $80000000    ;must be after tcolor0
cpat0 ds.l 1
cpat1 ds.l 1

EventRecord ds.b 16
msg dc.w 0,msg2-msg1,msg3-msg1,msg4-msg1,msg5-msg1,msg6-msg1,msg7-msg1,msg8-msg1,msg9-msg1

msg1     dc.b '  **********************************'
msg2     dc.b '  * Superfast Mandelbrot generator *'
msg3     dc.b '  *         2 colors, v1           *'
msg4     dc.b '  **********************************'
msg5     dc.b 'The original version was published for'
msg6     dc.b 'the BK0011 in 2021 by Stanislav Maslovski.'
msg7     dc.b 'This Apple Macintosh port was created by Litwr, 2022.'
msg8     dc.b 'The T-key gives us timings.'
msg9     dc.b 'Use the Q-key to quit'

WindowName DC.B 'Superfast Mandelbrot Generator v1'

pat0 dc.b	15,1,2, 3, 5,10,14,0
pat1 dc.b	15,4,9,12,14, 5, 1,0
        END

