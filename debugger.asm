;=====================================================
; This is a set of commands that add more powerful
; debugging commands to the xKIM monitor.
;
; 10/13/2020 - Bob Applegate, bob@corshamtech.com
;
; Adds basic commands:
;    Load Intel HEX file
;    Examine/modify memory
;    Breakpoint handling:
;       List current breakpoints
;       Clear an existing breakpoint
;       Set a breakpoint (execution only)
;    Single step
;    Examine/modify registers
;    Continue from current PC
;    Run starting at specified address
;    Disassembly of code
;    Hex dump of area of memory
;
; Addresses and values can be zero or more hex digits
; in length. 0 = $0000, etc.
;
; Commands are entered, return pressed, then commands
; are interpreted.  Error messages aren't necessarily
; too useful but give a hint.
;
; DONE
; ====
; B = Lists breakpoints
; BD = Disable breakpoints
; BE = Enable breakpoints
; BC [<addr>] = with addr, clears just one breakpoint.
;             Else clears all.
; BS <addr> = Sets breakpoint at addr.
; H <addr> <addr> = Hex dump specified range.
; J <addr> = Jump to specified address.
; R = Displays all registers.
;
; PARTIALLY DONE
; ==============
;
; NOT STARTED
; ===========
; L <filename> = Load intel hex file from SD card.
; L = Load Intel hex file from console.
; E <addr> = Examine and start modifying at addr.
;            RET leaves existing value, '.' ends.
; S = Single step current instruction at PC.
; C = Continue execution at PC; runs until breakpoint.
; D [<addr1> [addr2]] = Disassemble at current PC or
;            at specified address or specified range.
; R <reg> <value> = Set register to value.
; 
; Consider buying a KIM-1 expansion board or a KIM
; Clone from us:
;
;    www.corshamtech.com
;
		include	"kim.inc"
		include	"xkim.inc"
;
;=====================================================
;
; First, define some common ASCII characters
;
NULL		equ	$00
BELL		equ	$07
BS		equ	$08
LF		equ	$0a
CR		equ	$0d
;
BUFSIZE		equ	40
false		equ	0
true		equ	~false
TRUE		equ	true
FALSE		equ	false
;
;=====================================================
; Options
;
LOW_RAM		equ	TRUE
;
	if	LOW_RAM
BASE		equ	$0400
HIGHEST		equ	$13ff
	else
BASE		equ	$d000
HIGHEST		equ	$deff
	endif
;
;=====================================================
; The debugger
;
		code
		org	BASE
Debugger	jsr	putsil
		db	CR,LF,LF
		db	"Corsham Tech 6502 Debugger v0.0"
		db	CR,LF
		db	"Running at address ",0
		lda	#Debugger>>8
		jsr	PRTBYT
		lda	#Debugger&$ff
		jsr	PRTBYT
		jsr	putsil
		db	" to ",0
		lda	#CODE_END>>8
		jsr	PRTBYT
		lda	#CODE_END&$ff
		jsr	PRTBYT
		jsr	CRLF
		jsr	CRLF
;
; Save the stack pointer for cleaning up later
;
		tsx
		stx	initialSP
		jsr	InstallVecs	;install handlers
		jsr	BreakInit	;initialize breakpoints
;
; After an interrupt which has saved the registers, the
; debugger goes here.  Not sure if there is anything
; to do other than fall into the main loop yet.
;
WARM		jsr	BreakRemove	;undo breakpoints
;
; Main loop.  Present a prompt, get command, process it
;
MainLoop	ldx	initialSP
		txs			;make stack sane
		jsr	putsil
		db	"DBG> ",0
		jsr	GetLine		;get command line
		beq	MainLoop	;branch on no data
;
; Parse the command.  This is NOT a very fancy parser
; and it is very intolerant of weirdness line leading
; spaces.  No leading spaces, only one space between
; parameters, etc.
;
		ldx	#0		;offset into line
		stx	cmdOffset
		jsr	SkipSpaces
		lda	buffer,x	;get command
		pha
		jsr	SkipSpaces2	;skip over command
		pla
;
		ldx	#0
cmdloop		pha			;save command
		lda	Commands,x	;get entry
		beq	cmdnotfound	;not found
		pla			;restore command
		cmp	Commands,x
		beq	cmdfound
		inx
		inx
		inx
		bne	cmdloop		;check next command
;
cmdfound	lda	Commands+1,x	;LSB of handler
		sta	Temp16
		lda	Commands+2,x	;MSB
		sta	Temp16+1
		jmp	(Temp16)	;jump to handler
;
; Unknown command
;
cmdnotfound	pla			;clean stack
		jsr	putsil
		db	"Huh?",CR,LF,0
		jmp	vecHelp
;
; Vectors to command handlers.  Each entry has a one byte
; ASCII character followed by a pointer to the handler for
; that command.  Most handlers end with a JMP MainLoop.
;
Commands	db	'?'
		dw	vecHelp
		db	'B'
		dw	Breakpoints
		db	'D'
		dw	Disassem
		db	'E'
		dw	MemEdit
		db	'H'
		dw	HexDump
		db	'J'
		dw	Jump
		db	'Q'
		dw	ExitDbg
		db	'R'
		dw	Registers
		db	0		;MUST BE LAST!
;
;=====================================================
; Provides a bit of help.
;
vecHelp		jsr	putsil
		db	CR,LF
		db	"? = This list",CR,LF
		db	"B = List breakpoints",CR,LF
		db	"BD = Disable breakpoints",CR,LF
		db	"BE = Enable breakpoints",CR,LF
		db	"BC [<addr>] = Clear one or all breakpoints",CR,LF
		db	"BS <addr> = Sets breakpoint at address",CR,LF
		db	"D <addr> <addr> = Disassemble 65C02 code",CR,LF
		db	"H <addr> <addr> = Do hex dump",CR,LF
		db	"J <addr> = Jump to address",CR,LF
		db	"R [<name> <value>] = Registers",CR,LF
		db	"Q = Quit back to xKIM",CR,LF
		db	0
		jmp	MainLoop
;
;=====================================================
; Exit the debugger.
;
ExitDbg		jsr	RestoreVecs	;undo our handlers
		ldx	initialSP
		txs
		jmp	extKim		;return to xKIM
;
;=====================================================
; Jump to addresss which is on the command line.  It
; jumps to the specified address with undefined
; register values.  Breakpoints are active.
;
Jump		jsr	ChkParam	;check for param
		jsr	GetHex
		jsr	BreakInstall	;enable breakpoints
		jmp	(Temp16)
;
;=====================================================
; Does a disassembly of the specified range.
;
Disassem	jsr	ChkParam	;missing start?
		jsr	GetHex		;start address
		jsr	XferPOINT	;put into POINT
		jsr	SkipSpaces
		jsr	ChkParam	;missing end?
		jsr	GetHex
		jsr	XferEA
;
; Now call the disassemble subroutine to do the work.
;
disassemloop	jsr	doDisassem	;disassemble one
;
; The length of the instruction is in A, so add that
; to POINT so it points to the next instruction.
;
		clc
		adc	POINTL
		sta	POINTL
		bcc	disassem1
		inc	POINTH
;
; Keep disassembling if POINTL <= EA
;
disassem1	lda	EAH
		cmp	POINTH
		bcc	disassemdun
		bne	disassemloop
		lda	EAL
		cmp	POINTL
		bcs	disassemloop
disassemdun	jmp	MainLoop
;
;=====================================================
; Does a hex dump of the specified range.
;
HexDump		jsr	ChkParam	;missing start?
		jsr	GetHex		;start address
		jsr	XferSA
		jsr	SkipSpaces
		jsr	ChkParam	;missing end?
		jsr	GetHex
		jsr	XferEA
;
; Now call the xKIM hex dump function to do the work.
;
		jsr	xkHexDump
		jmp	MainLoop
;
;=====================================================
; Does a memory edit starting at the specified address
;
MemEdit		jsr	ChkParam	;missing start?
		jsr	GetHex		;start address
		jsr	XferPOINT
;
; Now call the xKIM function to do the work.
;
		jsr	xkMemEdit
		jmp	MainLoop
;
;=====================================================
; A generic checker/handler for missing parameter.
; If there is no parameter next on the command line,
; this prints and error and jumps back to MainLoop.
;
ChkParam	ldx	cmdOffset
		lda	buffer,x	;get current char
		bne	noparmret	;non-zero is good
		jsr	putsil
		db	"Missing parameter",CR,LF,0
		jmp	MainLoop
noparmret	rts
;
;=====================================================
; Get a line of text into buffer.  Returns the number
; of input characters in A.  Z reflects A's status.
; Does NO checking on input data except for length!
;
GetLine		ldx	#0
getline1	stx	tempX
		jsr	xkGETCH		;get next key
		ldx	tempX
		cmp	#CR		;end of the input?
		beq	getLnDone
		cmp	#BS		;backspace?
		beq	getLnDel
		cpx	#BUFSIZE-1	;check size
		beq	getline1	;at length limit
		sta	buffer,x	;else save it
		inx
		bne	getline1
;
getLnDel	dex			;back up one
		bpl	getline1
		inx			;can't go past start
		beq	getline1
;
getLnDone       lda	#0		;terminate line
		sta	buffer,x
		txa
		pha
		jsr	CRLF
		pla
		rts
;
;=====================================================
; Skips spaces in buffer starting at cmdOffset.
; Stops on first non-space and leaves the offset
; in cmdOffset.  SkipSpaces2 advances one position
; first before starting the search.
;
; A is destroyed.  Should fix this.
;
SkipSpaces2	inc	cmdOffset
SkipSpaces	ldx	cmdOffset
		lda	buffer,x
		cmp	#0
		beq	skipsp1
		cmp	#' '		;a space?
		beq	SkipSpaces2
skipsp1		rts
;
;=====================================================
; Scans the input line to assemble a hex number.  Can
; be anywhere from 1 to X digits long, but only the
; last four are used.  Ie, 123456 returns $3456.
; Return value is in Temp16.
;
GetHex		lda	#0
		sta	Temp16
		sta	Temp16+1	;clear result
gethex5		ldx	cmdOffset
		lda	buffer,x
		ldy	#HexDigEnd-HexDigits-1
gethex1		cmp	HexDigits,y
		beq	gethex2		;found
		dey
		bpl	gethex1
;
; Not valid hex, so return whatever is in the result.
;
		rts
;
; Found it.  Offset, which is the value, is in Y.
; Shift existing value 4 bits left.
;
gethex2		tya
		ldy	#4
gethex3		asl	Temp16
		rol	Temp16+1
		dey
		bne	gethex3
		clc
		adc	Temp16		;add in digit
		sta	Temp16
		bcc	gethex4
		inc	Temp16+1
gethex4		inc	cmdOffset
		bne	gethex5		;get next digit
;
HexDigits	db	"0123456789ABCDEF"
HexDigEnd	equ	*
;
;=====================================================
; Dislays or modifies register values
;
Registers
 if 1
DumpRegisters	jsr	putsil
		db	"PC:",0
		lda	PCH
		jsr	PRTBYT
		lda	PCL
		jsr	PRTBYT
;
		jsr	putsil
		db	" A:",0
		lda	ACC
		jsr	PRTBYT
;
		jsr	putsil
		db	" X:",0
		lda	XREG
		jsr	PRTBYT
;
		jsr	putsil
		db	" Y:",0
		lda	YREG
		jsr	PRTBYT
;
		jsr	putsil
		db	" SP:",0
		lda	SPUSER
		jsr	PRTBYT
;
; Last is the condition register.  For this, print the
; actual flags.  Lower case for clear, upper for set.
;
		jsr	putsil
		db	" Flags:",0
	if	FULL_STATUS
;
; N - bit 7
;
		lda	#$80	;bit to test
		ldx	#'N'	;set ACII char
		jsr	testbit
;
; V - bit 6
;
		lda	#$40	;bit to test
		ldx	#'V'	;set ACII char
		jsr	testbit
;
		lda	#'-'	;unused bit
		jsr	xkOUTCH
;
; B - bit 4
;
		lda	#$10	;bit to test
		ldx	#'B'	;set ACII char
		jsr	testbit
;
; D - bit 3
;
		lda	#$08	;bit to test
		ldx	#'D'	;set ACII char
		jsr	testbit
;
; I - bit 2
;
		lda	#$04	;bit to test
		ldx	#'I'	;set ACII char
		jsr	testbit
;
; Z - bit 1
;
		lda	#$02	;bit to test
		ldx	#'Z'	;set ACII char
		jsr	testbit
;
; C - bit 0
;
		lda	#$01	;bit to test
		ldx	#'C'	;set ACII char
;
; Fall through...
;
;*********************************************************
; Given a bit mask in A and an upper case character
; indicating the flag name in X, see if the flag is set or
; not.  Output upper case if set, lower case if not.
;
testbit		and	PREG	;is bit set?
		bne	testbit1	;yes
		txa
		ora	#$20	;make lower case
		jmp	xkOUTCH
testbit1	txa
		jmp	xkOUTCH
	else	;FULL_STATUS
		lda	PREG
		jmp	PRTBYT
	endif	;FULL_STATUS
 endif
		jmp	MainLoop


;
;=====================================================
; This transfers the 16 bit value in Temp16 into SAL
; and SAH.  Preserves registers.
;
XferSA		pha
		lda	Temp16
		sta	SAL
		lda	Temp16+1
		sta	SAH
		pla
		rts
;
;=====================================================
; This transfers the 16 bit value in Temp16 into EAL
; and EAH.  Preserves registers.
;
XferEA		pha
		lda	Temp16
		sta	EAL
		lda	Temp16+1
		sta	EAH
		pla
		rts
;
;=====================================================
; This transfers the 16 bit value in Temp16 into
; POINTL and POINTH.  Preserves registers.
;
XferPOINT	pha
		lda	Temp16
		sta	POINTL
		lda	Temp16+1
		sta	POINTH
		pla
		rts
;
;=====================================================
; These functions print 1, 2 or 3 spaces.
;
space3		jsr	space
space2		jsr	space
space		lda	#' '
		jmp	xkOUTCH


;
;=====================================================
; Functions to save and restore the KIM vectors.
; InstallVecs also points to the debugger's
; interrupt handler.
;
InstallVecs	ldx	#5
savevec1	lda	NMIV,x
		sta	savedVectors,x
		dex
		bpl	savevec1
;
; Install pointers to our handlers
;
		lda	#DefaultNMI&$ff
		sta	NMIV
		lda	#DefaultNMI>>8
		sta	NMIV+1
;
		lda	#DefaultIRQ&$ff
		sta	IRQV
		lda	#DefaultIRQ>>8
		sta	IRQV+1
		rts
;
;*********************************************************
RestoreVecs	ldx	#5
restvec1	lda	savedVectors,x
		sta	NMIV,x
		dex
		bpl	restvec1
		rts
;
;*********************************************************
; Default handler.  Save the state of the machine for
; debugging.  This is taken from the KIM monitor SAVE
; routine.
;
DefaultNMI
DefaultIRQ	sta	ACC
		pla
		sta	PREG
		pla
		sta	PCL
		pla
		sta	PCH
		sty	YREG
		stx	XREG
		tsx
		stx	SPUSER
		jsr	DumpRegisters
		jsr	CRLF
		jmp	WARM
;
		include	"break.asm"	;breakpoints
		include	"dis.asm"	;disassembler
NEXTFREE	equ	*
;
;=====================================================
; Non zero-page data.  This is all uninitialized; the
; code must set these up as needed.
;
initialSP	ds	1		;SP on entry
cmdOffset	ds	1
buffer		ds	BUFSIZE
tempX		ds	1		;generic storage
savedVectors	ds	6
Temp16		ds	2		;16 bit register
storeX		ds	1
storeY		ds	1
;
CODE_END	equ	*
;
	if	* >= HIGHEST
		error	Overran memory
	endif
;
;=====================================================
; Force auto-run
;
		code
		org	AutoRun
		dw	Debugger
;
		end


