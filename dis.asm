;=====================================================
; dis.asm
;
; This is the disassembler for the debugger.

;
; This is coded to support the WDC 65C02 chip.  In the
; future I might have a 6502 mode.
;
; This is prime for optimizations!  Right now it is a
; brute-force, table-lookup model, without any
; elegance whatsoever.  To do this better, start with
; either Woz's 6502 disassembler and/or this great
; web page: https://llx.com/Neil/a2/opcodes.html
;
; The 6502 was more-or-less well laid out but when
; WDC added new instructions they kind of squeezed
; them in wherever there was room.  Ie, no clean
; algoithm to decode them.
;
;=====================================================
; Addressing mode constants.  If this gets changed,
; then addmodeLen must be adjusted too.  Sorry for the
; awful names, I just wanted them all to be the same
; length.
;
AM_BADD		equ	0	;illegal instruction
AM_ACCU		equ	1	;Accumulator
AM_IMME		equ	2	;Immediate: #value
AM_IMPL		equ	3	;Implied: nothing
AM_ABSO		equ	4	;Absolute: 16 bit address
AM_RELA		equ	5	;Relative
AM_ZERO		equ	6	;zero page: zp
AM_INDI		equ	7	;Indirect: (address)
AM_AIWX		equ	8	;Absolute Indexed with X: address,X
AM_AIWY		equ	9	;Absolute Indexed with Y: address,Y
AM_ZIIX		equ	10	;Zero Page Indexed Indirect: (zp,X)
AM_ZIWX		equ	11	;Zero Page Indexed with X: zp,X
AM_ZIWY		equ	12	;Zero Page Indexed with Y: zp,Y
AM_ZPIN		equ	13	;Zero Page Indirect: (zp)
AM_IIWY		equ	14	;Zero Page Indexed Indirect with Y: (zp),Y
AM_AIIX		equ	15	;Absolute Indexed Indirect with X: (addr,X)
;
;=====================================================
; Disassembles one instruction pointed to by PCL.
; Returns C set if an illegal instruction, otherwise
; returns C clear and A contains the number of bytes
; in the instruction.
;
doDisPC		lda	PCL
		sta	POINTL
		lda	PCH
		sta	POINTH
;
;=====================================================
; Disassembles one instruction pointed to by POINT.
; Returns C set if an illegal instruction, otherwise
; returns C clear and A contains the number of bytes
; in the instruction.
;
doDisassem	ldy	#0
		lda	(POINTL),y	;get instruction
		sta	opcode		;save for later
		tax
		lda	addmodeTbl,x	;get addr mode
		cmp	#AM_BADD	;illegal opcode?
		bne	dodissem1
;
; Bad opcode, so return with a length of 1 and C set.
;
		lda	#1
		sec
		rts
;
; A good instruction, so format and output the address,
; the opcode, and any additional bytes.  When done,
; the cursor must always end in the same column.
;
dodissem1	sta	addmode
		jsr	PRTPNT		;print address
		jsr	space2
;
; Print the opcode and any data bytes
;
		ldx	addmode
		lda	addmodeLen,x	;get length
		tax
		ldy	#0
dodissem2	lda	(POINTL),y	;get byte
		stx	storeX
		sty	storeY
		jsr	PRTBYT
		jsr	space
		ldy	storeY
		ldx	storeX
		iny
		dex
		bne	dodissem2
;
; While Y is less than 3, print three spaces
;
dodissem3	cpy	#3
		beq	dodissem4
		sty	storeY
		jsr	space3
		ldy	storeY
		iny
		bne	dodissem3
dodissem4	jsr	space2
;
; Now it is time to display the opcode.  Most
; opcodes are three bytes long, but some WDC65C02
; opcodes are four bytes.  Print three, then 
; determine if a fourth is required.
;
		ldx	opcode
		lda	mnemoniclookup,x
		tax
		lda	MNENS,x
		sta	Temp16
		lda	MNENS+1,x
		sta	Temp16+1
;
; Temp16 now contains the 16 bit compressed mnemonic.
; Decode and print.
;
		jsr	disprletter	;display first
		jsr	disshift5
		jsr	disprletter	;display second
		jsr	disshift5
		jsr	disprletter	;display third
;
; The WDC65C02 has four instructions that have a bit
; number appended to the opcode: RMB, SMB, BBR and BBS.
;
		lda	opcode
		and	#$0f
		cmp	#$07
		beq	disweird	;it's a weird opcode
		cmp	#$0f
		bne	disnormal
;
; It's one of the weird opcodes.
;
disweird	lda	opcode
		lsr	a
		lsr	a
		lsr	a
		lsr	a
		ora	#'0'
		jsr	xkOUTCH
		jsr	space
		jmp	disargs
disnormal	jsr	space2
;
; The opcode has been printed, so now it's time to print
; the arguments (if any) in the proper format.
;
disargs		lda	addmode
		asl	a		;make index
		tax
		lda	disaddjump,x
		sta	Temp16
		lda	disaddjump+1,x
		sta	Temp16+1
		jmp	(Temp16)	;go to handler
;
;=====================================================
; Using the addressing mode as an offset, this table
; contains pointers to the handlers for each mode.
;
disaddjump	dw	dishanImp
		dw	dishanA
		dw	dishanImm
		dw	dishanImp
		dw	dishanAbs
		dw	dishanRel
		dw	dishanZero
		dw	dishanInd
		dw	dishanAIWX
		dw	dishanAIWY
		dw	dishanZIIX
		dw	dishanZIWX
		dw	dishanZIWY
		dw	dishanZPIN
		dw	dishanIIWY
		dw	dishanAIIX
;
;=====================================================
; Handler for implied and bad modes.  There are no
; additional bytes to ouput.
;
dishanImp	lda	#0
;
;=====================================================
; This is a common entry point.  A contains the number
; of characters output by the handler, then this spaces
; over so each line ends at the same column.
;
; (xxxx),X
;
disspacer	tax		;space counter
disspacer2	stx	storeX
		jsr	space
		ldx	storeX
		inx
		cpx	#9
		bne	disspacer2
;
; Finish up by loading the instruction length into A
; and clearning carry.
;
		ldx	addmode
		lda	addmodeLen,x	;get length
		clc
		rts
;
;=====================================================
; This handles the ACCUMULATOR mode by outputting an A.

dishanA		lda	#'A'
		jsr	xkOUTCH
		lda	#1
		jmp	disspacer
;
;=====================================================
; Handles the immediate mode.
;
dishanImm	lda	#'#'
		jsr	xkOUTCH
		ldy	#1
		lda	(POINTL),y
		jsr	PRTBYT
		lda	#3
		jmp	disspacer
;
;=====================================================
; Handles the absolute mode.
;
dishanAbs	ldy	#2
		lda	(POINTL),y
		jsr	PRTBYT
		ldy	#1
		lda	(POINTL),y
		jsr	PRTBYT
		lda	#4
		jmp	disspacer
;
;=====================================================
; Relative.  The second byte of the instruction has an
; 8 bit signed offset from the byte following the
; instruction.  So get the current instruction's
; address, add two, then add in the offset.
;
dishanRel	lda	POINTL		;move to Temp16
		sta	Temp16
		lda	POINTH
		sta	Temp16+1
;
		clc
		lda	#2		;now add two
		adc	Temp16
		sta	Temp16
		bcc	dishanrel1
		inc	Temp16+1
;
dishanrel1	ldy	#1		;now add in offset
		lda	#0
		sta	storeX		;for sign extension
		lda	(POINTL),y
		bpl	dishanpos	;branch if positive
		dec	storeX		;make ext negative
dishanpos	clc
		adc	Temp16
		sta	Temp16

		lda	storeX
		adc	Temp16+1
		sta	Temp16+1
;
		lda	Temp16+1
		jsr	PRTBYT
		lda	Temp16
		jsr	PRTBYT
;
		lda	#4
		jmp	disspacer
;
;=====================================================
; Handles zero page
;
dishanZero	jsr	disprZaddr
		lda	#2
		jmp	disspacer
;
;=====================================================
; Indirect.  Just like absolute except address is in
; parens.  (addr)
;
dishanInd	jsr	disprpaddr
		lda	#')'
		jsr	xkOUTCH
		lda	#6
		jmp	disspacer
;
;=====================================================
; addr,X
;
dishanAIWX	jsr	dispraddr
		jsr	putsil
		db	",X",0
		lda	#6
		jmp	disspacer
;
;=====================================================
; addr,Y
;
dishanAIWY	jsr	dispraddr
		jsr	putsil
		db	",Y",0
		lda	#6
		jmp	disspacer
;
;=====================================================
; (zp,X)
;
dishanZIIX	jsr	disprpZaddr
		jsr	putsil
		db	",X)",0
		lda	#6
		jmp	disspacer
;
;=====================================================
; zp,X
;
dishanZIWX	jsr	disprZaddr
		jsr	putsil
		db	",X",0
		lda	#4
		jmp	disspacer
;
;=====================================================
; zp,Y
;
dishanZIWY	jsr	disprZaddr
		jsr	putsil
		db	",Y",0
		lda	#4
		jmp	disspacer
;
;=====================================================
; (zp)
;
dishanZPIN	jsr	disprpZaddr
		lda	#')'
		jsr	xkOUTCH
		lda	#4
		jmp	disspacer
;
;=====================================================
; (zp),Y
;
dishanIIWY	jsr	disprpZaddr
		jsr	putsil
		db	"),Y",0
		lda	#6
		jmp	disspacer
;
;=====================================================
; (ADDR,x)
;
dishanAIIX	jsr	disprpaddr
		jsr	putsil
		db	",X)",0
		lda	#8
		jmp	disspacer
;
;=====================================================
; This prints a "(" followed by the 16 bit address
; after the opcode.
;
disprpaddr	lda	#'('
		jsr	xkOUTCH
;
;=====================================================
; This prints the two bytes following the opcode.
;
dispraddr	ldy	#2
		lda	(POINTL),y
		jsr	PRTBYT
		ldy	#1
		lda	(POINTL),y
		jmp	PRTBYT
;
;=====================================================
; This prints a "(" followed by the 8 bit address
; after the opcode.
;
disprpZaddr	lda	#'('
		jsr	xkOUTCH
;
;=====================================================
; This prints the byte following the opcode.
;
disprZaddr	ldy	#1
		lda	(POINTL),y
		jmp	PRTBYT
;
;=====================================================
; This takes the bottom five bits of Temp16, adds
; back the '?' and prints it.
;
disprletter	lda	Temp16
		and	#%00011111
		clc
		adc	#'?'
		jmp	xkOUTCH
;
;=====================================================
; This shifts Temp16 right by five bits.
;
disshift5	ldy	#5
disshift6	lsr	Temp16+1
		ror	Temp16
		dey
		bne	disshift6
		rts
;
;=====================================================
; Using the addressing mode as an index, this gives
; the number of bytes for this mode.
;
addmodeLen	db	1,1,2,1,3,2,2,3
		db	3,3,2,2,2,2,2,3

;
;=====================================================
; This table uses the opcode as an index to get the
; addressing mode.  That can be used to get the number
; of bytes.
;
addmodeTbl	db	AM_IMPL, AM_ZIIX, AM_BADD, AM_BADD	;00-03
		db	AM_ZERO, AM_ZERO, AM_ZERO, AM_ZERO	;04-07
		db	AM_IMPL, AM_IMME, AM_ACCU, AM_BADD	;08-0B
		db	AM_ACCU, AM_ACCU, AM_ACCU, AM_RELA	;0C-0F
;
		db	AM_RELA, AM_IIWY, AM_ZERO, AM_BADD	;10-13
		db	AM_ZERO, AM_ZIWX, AM_ZIWX, AM_ZERO	;14-17
		db	AM_IMPL, AM_AIWY, AM_ACCU, AM_BADD	;18-1B
		db	AM_ACCU, AM_AIWX, AM_AIWX, AM_RELA	;1C-1F
;
		db	AM_ABSO, AM_ZIIX, AM_BADD, AM_BADD	;20-23
		db	AM_ZERO, AM_ZERO, AM_ZERO, AM_ZERO	;24-27
		db	AM_IMPL, AM_IMME, AM_ACCU, AM_BADD	;28-2B
		db	AM_ABSO, AM_ABSO, AM_ABSO, AM_RELA	;2C-2F
;
		db	AM_RELA, AM_IIWY, AM_ZPIN, AM_BADD	;30-33
		db	AM_ZIWX, AM_ZIWX, AM_ZIWX, AM_ZERO	;34-37
		db	AM_IMPL, AM_AIWY, AM_ACCU, AM_BADD	;38-3B
		db	AM_AIWX, AM_AIWX, AM_AIWX, AM_RELA	;3C-3F
;
		db	AM_IMPL, AM_ZIIX, AM_BADD, AM_BADD	;40-43
		db	AM_BADD, AM_ZERO, AM_ZERO, AM_ZERO	;44-47
		db	AM_IMPL, AM_IMME, AM_ACCU, AM_BADD	;48-4B
		db	AM_ABSO, AM_ABSO, AM_ABSO, AM_RELA	;4C-4F
;
		db	AM_RELA, AM_IIWY, AM_ZPIN, AM_BADD	;50-53
		db	AM_BADD, AM_ZIWX, AM_ZIWX, AM_ZERO	;54-57
		db	AM_IMPL, AM_AIWY, AM_IMPL, AM_BADD	;58-5B
		db	AM_BADD, AM_AIWX, AM_AIWX, AM_RELA	;5C-5F
;
		db	AM_IMPL, AM_ZIIX, AM_BADD, AM_BADD	;60-63
		db	AM_ZERO, AM_ZERO, AM_ZERO, AM_ZERO	;64-67
		db	AM_IMPL, AM_IMME, AM_ACCU, AM_BADD	;68-6B
		db	AM_INDI, AM_ACCU, AM_ACCU, AM_RELA	;6C-6F
;
		db	AM_RELA, AM_IIWY, AM_ZPIN, AM_BADD	;70-73
		db	AM_ZIWX, AM_ZIWX, AM_ZIWX, AM_ZERO	;74-77
		db	AM_IMPL, AM_AIWY, AM_IMPL, AM_BADD	;78-7B
		db	AM_AIIX, AM_AIWX, AM_AIWX, AM_RELA	;7C-7F
;
		db	AM_RELA, AM_ZIIX, AM_BADD, AM_BADD	;80-83
		db	AM_ZERO, AM_ZERO, AM_ZERO, AM_ZERO	;84-87
		db	AM_IMPL, AM_IMME, AM_IMPL, AM_BADD	;88-8B
		db	AM_ABSO, AM_ABSO, AM_ABSO, AM_ABSO	;8C-8F
;
		db	AM_RELA, AM_IIWY, AM_ZPIN, AM_BADD	;90-93
		db	AM_ZIWX, AM_ZIWX, AM_ZIWY, AM_ZERO	;94-97
		db	AM_IMPL, AM_AIWY, AM_IMPL, AM_BADD	;98-9B
		db	AM_ACCU, AM_AIWX, AM_AIWX, AM_RELA	;9C-9F
;
		db	AM_IMME, AM_ZIIX, AM_IMME, AM_BADD	;A0-A3
		db	AM_ZERO, AM_ZERO, AM_ZERO, AM_ZERO	;A4-A7
		db	AM_IMPL, AM_IMME, AM_IMPL, AM_BADD	;A8-AB
		db	AM_ABSO, AM_ABSO, AM_ABSO, AM_RELA	;AC-AF
;
		db	AM_RELA, AM_IIWY, AM_ZPIN, AM_BADD	;B0-B3
		db	AM_ZIWX, AM_ZIWX, AM_ZIWY, AM_ZERO	;B4-B7
		db	AM_IMPL, AM_AIWY, AM_IMPL, AM_BADD	;B8-BB
		db	AM_AIWX, AM_AIWX, AM_AIWY, AM_RELA	;BC-BF
;
		db	AM_IMME, AM_ZIIX, AM_BADD, AM_BADD	;C0-C3
		db	AM_ZERO, AM_ZERO, AM_ZERO, AM_ZERO	;C4-C7
		db	AM_IMPL, AM_IMME, AM_IMPL, AM_IMPL	;C8-CB
		db	AM_ABSO, AM_ABSO, AM_ABSO, AM_RELA	;CC-CF
;
		db	AM_RELA, AM_IIWY, AM_ZPIN, AM_BADD	;D0-D3
		db	AM_BADD, AM_ZIWX, AM_ZIWX, AM_ZERO	;D4-D7
		db	AM_IMPL, AM_AIWY, AM_IMPL, AM_IMPL	;D8-DB
		db	AM_BADD, AM_AIWX, AM_AIWX, AM_RELA	;DC-DF
;
		db	AM_IMME, AM_ZIIX, AM_BADD, AM_BADD	;E0-E3
		db	AM_ZERO, AM_ZERO, AM_ZERO, AM_ZERO	;E4-E7
		db	AM_IMPL, AM_IMME, AM_IMPL, AM_BADD	;E8-EB
		db	AM_ABSO, AM_ABSO, AM_ABSO, AM_RELA	;EC-EF
;
		db	AM_RELA, AM_IIWY, AM_ZPIN, AM_BADD	;D0-D3
		db	AM_BADD, AM_ZIWX, AM_ZIWX, AM_ZERO	;D4-D7
		db	AM_IMPL, AM_AIWY, AM_IMPL, AM_BADD	;D8-DB
		db	AM_BADD, AM_AIWX, AM_AIWX, AM_RELA	;DC-DF
;
;=====================================================
; Using the opcode as an index, this returns the
; offset into the mnemonic table to get the 16 bit
; compressed mnemonic.
;
mnemoniclookup	db	MNEM_BRK&$ff, MNEM_ORA&$ff	;00-0F
		db	MNEM_BAD&$ff, MNEM_BAD&$ff
		db	MNEM_TSB&$ff, MNEM_ORA&$ff
		db	MNEM_ASL&$ff, MNEM_RMB&$ff
		db	MNEM_PHP&$ff, MNEM_ORA&$ff
		db	MNEM_ASL&$ff, MNEM_BAD&$ff
		db	MNEM_TSB&$ff, MNEM_ORA&$ff
		db	MNEM_ASL&$ff, MNEM_BBR&$ff
;
		db	MNEM_BPL&$ff, MNEM_ORA&$ff	;10-1F
		db	MNEM_ORA&$ff, MNEM_BAD&$ff
		db	MNEM_TRB&$ff, MNEM_ORA&$ff
		db	MNEM_ASL&$ff, MNEM_RMB&$ff
		db	MNEM_CLC&$ff, MNEM_ORA&$ff
		db	MNEM_INC&$ff, MNEM_BAD&$ff
		db	MNEM_TRB&$ff, MNEM_ORA&$ff
		db	MNEM_ASL&$ff, MNEM_BBR&$ff
;
		db	MNEM_JSR&$ff, MNEM_AND&$ff	;20-2F
		db	MNEM_BAD&$ff, MNEM_BAD&$ff
		db	MNEM_BIT&$ff, MNEM_AND&$ff
		db	MNEM_ROL&$ff, MNEM_RMB&$ff
		db	MNEM_PLP&$ff, MNEM_AND&$ff
		db	MNEM_ROL&$ff, MNEM_BAD&$ff
		db	MNEM_BIT&$ff, MNEM_AND&$ff
		db	MNEM_ROL&$ff, MNEM_BBR&$ff
;
		db	MNEM_BMI&$ff, MNEM_AND&$ff	;30-3F
		db	MNEM_AND&$ff, MNEM_BAD&$ff
		db	MNEM_BIT&$ff, MNEM_AND&$ff
		db	MNEM_ROL&$ff, MNEM_RMB&$ff
		db	MNEM_SEC&$ff, MNEM_AND&$ff
		db	MNEM_DEC&$ff, MNEM_BAD&$ff
		db	MNEM_BIT&$ff, MNEM_AND&$ff
		db	MNEM_ROL&$ff, MNEM_BBR&$ff
;
		db	MNEM_RTI&$ff, MNEM_EOR&$ff	;40-4F
		db	MNEM_BAD&$ff, MNEM_BAD&$ff
		db	MNEM_BAD&$ff, MNEM_EOR&$ff
		db	MNEM_LSR&$ff, MNEM_RMB&$ff
		db	MNEM_PHA&$ff, MNEM_EOR&$ff
		db	MNEM_LSR&$ff, MNEM_BAD&$ff
		db	MNEM_JMP&$ff, MNEM_EOR&$ff
		db	MNEM_LSR&$ff, MNEM_BBR&$ff
;
		db	MNEM_BVC&$ff, MNEM_EOR&$ff	;50-5F
		db	MNEM_EOR&$ff, MNEM_BAD&$ff
		db	MNEM_BAD&$ff, MNEM_EOR&$ff
		db	MNEM_LSR&$ff, MNEM_RMB&$ff
		db	MNEM_CLI&$ff, MNEM_EOR&$ff
		db	MNEM_PHY&$ff, MNEM_BAD&$ff
		db	MNEM_BAD&$ff, MNEM_EOR&$ff
		db	MNEM_LSR&$ff, MNEM_BBR&$ff
;
		db	MNEM_RTS&$ff, MNEM_ADC&$ff	;60-6F
		db	MNEM_BAD&$ff, MNEM_BAD&$ff
		db	MNEM_STZ&$ff, MNEM_ADC&$ff
		db	MNEM_ROR&$ff, MNEM_RMB&$ff
		db	MNEM_PLA&$ff, MNEM_ADC&$ff
		db	MNEM_ROR&$ff, MNEM_BAD&$ff
		db	MNEM_JMP&$ff, MNEM_ADC&$ff
		db	MNEM_ROR&$ff, MNEM_BBR&$ff
;
		db	MNEM_BVS&$ff, MNEM_ADC&$ff	;70-7F
		db	MNEM_ADC&$ff, MNEM_BAD&$ff
		db	MNEM_STZ&$ff, MNEM_ADC&$ff
		db	MNEM_ROR&$ff, MNEM_RMB&$ff
		db	MNEM_SEI&$ff, MNEM_ADC&$ff
		db	MNEM_PLY&$ff, MNEM_BAD&$ff
		db	MNEM_JMP&$ff, MNEM_ADC&$ff
		db	MNEM_ROR&$ff, MNEM_BBR&$ff
;
		db	MNEM_BRA&$ff, MNEM_STA&$ff	;80-8F
		db	MNEM_BAD&$ff, MNEM_BAD&$ff
		db	MNEM_STY&$ff, MNEM_STA&$ff
		db	MNEM_STX&$ff, MNEM_SMB&$ff
		db	MNEM_DEY&$ff, MNEM_BIT&$ff
		db	MNEM_TXA&$ff, MNEM_BAD&$ff
		db	MNEM_STY&$ff, MNEM_STA&$ff
		db	MNEM_STX&$ff, MNEM_BBS&$ff
;
		db	MNEM_BCC&$ff, MNEM_STA&$ff	;90-9F
		db	MNEM_STA&$ff, MNEM_BAD&$ff
		db	MNEM_STY&$ff, MNEM_STA&$ff
		db	MNEM_STX&$ff, MNEM_SMB&$ff
		db	MNEM_TYA&$ff, MNEM_STA&$ff
		db	MNEM_TXS&$ff, MNEM_BAD&$ff
		db	MNEM_STZ&$ff, MNEM_STA&$ff
		db	MNEM_STZ&$ff, MNEM_BBS&$ff
;
		db	MNEM_LDY&$ff, MNEM_LDA&$ff	;A0-AF
		db	MNEM_LDX&$ff, MNEM_BAD&$ff
		db	MNEM_LDY&$ff, MNEM_LDA&$ff
		db	MNEM_LDX&$ff, MNEM_SMB&$ff
		db	MNEM_TAY&$ff, MNEM_LDA&$ff
		db	MNEM_TAX&$ff, MNEM_BAD&$ff
		db	MNEM_LDY&$ff, MNEM_LDA&$ff
		db	MNEM_LDX&$ff, MNEM_BBS&$ff
;
		db	MNEM_BCS&$ff, MNEM_LDA&$ff	;B0-BF
		db	MNEM_LDA&$ff, MNEM_BAD&$ff
		db	MNEM_LDY&$ff, MNEM_LDA&$ff
		db	MNEM_LDX&$ff, MNEM_SMB&$ff
		db	MNEM_CLV&$ff, MNEM_LDA&$ff
		db	MNEM_TXS&$ff, MNEM_BAD&$ff
		db	MNEM_LDY&$ff, MNEM_LDA&$ff
		db	MNEM_LDX&$ff, MNEM_BBS&$ff
;
		db	MNEM_CPY&$ff, MNEM_CMP&$ff	;C0-CF
		db	MNEM_BAD&$ff, MNEM_BAD&$ff
		db	MNEM_CPY&$ff, MNEM_CMP&$ff
		db	MNEM_DEC&$ff, MNEM_SMB&$ff
		db	MNEM_INY&$ff, MNEM_CMP&$ff
		db	MNEM_DEX&$ff, MNEM_WAI&$ff
		db	MNEM_CPY&$ff, MNEM_CMP&$ff
		db	MNEM_DEC&$ff, MNEM_BBS&$ff
;
		db	MNEM_BNE&$ff, MNEM_CMP&$ff	;D0-DF
		db	MNEM_CMP&$ff, MNEM_BAD&$ff
		db	MNEM_BAD&$ff, MNEM_CMP&$ff
		db	MNEM_DEC&$ff, MNEM_SMB&$ff
		db	MNEM_CLD&$ff, MNEM_CMP&$ff
		db	MNEM_PHX&$ff, MNEM_STP&$ff
		db	MNEM_BAD&$ff, MNEM_CMP&$ff
		db	MNEM_DEC&$ff, MNEM_BBS&$ff
;
		db	MNEM_CPX&$ff, MNEM_SBC&$ff	;E0-EF
		db	MNEM_BAD&$ff, MNEM_BAD&$ff
		db	MNEM_CPX&$ff, MNEM_SBC&$ff
		db	MNEM_INC&$ff, MNEM_SMB&$ff
		db	MNEM_INX&$ff, MNEM_SBC&$ff
		db	MNEM_NOP&$ff, MNEM_BAD&$ff
		db	MNEM_CPX&$ff, MNEM_SBC&$ff
		db	MNEM_INC&$ff, MNEM_BBS&$ff
;
		db	MNEM_BEQ&$ff, MNEM_SBC&$ff	;F0-FF
		db	MNEM_SBC&$ff, MNEM_BAD&$ff
		db	MNEM_BAD&$ff, MNEM_SBC&$ff
		db	MNEM_INC&$ff, MNEM_SMB&$ff
		db	MNEM_SED&$ff, MNEM_SBC&$ff
		db	MNEM_PLX&$ff, MNEM_BAD&$ff
		db	MNEM_BAD&$ff, MNEM_SBC&$ff
		db	MNEM_INC&$ff, MNEM_BBS&$ff
;
;=====================================================
; This table is used to look up the ASCII opcode for
; the mnemonics.  Even four byte values have the first
; three bytes in this table, then the decoding code
; will add the fourth byte.
;
; The values here are compressed by subtracting '?'
; from each ASCII character, and since there are only
; upper case characters, 5 bits is easy to hold all
; legal characters.  Compress three bytes into 16 bits
; to conserve space.  First character is in the lowest
; five bits.  Pretty common practice.
;
; So, LDA is:
;
;    'L' - '?' = $0D = 01101
;    'D' - '?' = $05 = 00101
;    'A' - '?' = $02 = 00010
;
; So the 16 bit value is: 00010 00101 01101 or $08AD
;
; No, I did not do these all by hand.  10 minutes of C
; coding produced a program which took a list of the
; mnemonics and cranked out all the DW lines.
;
; I was lazy and the contents of mnemoniclookup
; assume this table starts on a page boundary.  If
; this bothers you, then fix all the entries in
; mnemoniclookup.
;
		org	(*+$ff)&$ff00	;page boundary
MNENS		equ	*
MNEM_BAD	dw	0	;illegal, yup, all zeros
MNEM_ADC	dw	$10a2
MNEM_AND	dw	$15e2
MNEM_ASL	dw	$3682
MNEM_BBR	dw	$4c63
MNEM_BBS	dw	$5063
MNEM_BCC	dw	$1083
MNEM_BCS	dw	$5083
MNEM_BEQ	dw	$48c3
MNEM_BIT	dw	$5543
MNEM_BMI	dw	$29c3
MNEM_BNE	dw	$19e3
MNEM_BPL	dw	$3623
MNEM_BRA	dw	$0a63
MNEM_BRK	dw	$3263
MNEM_BVC	dw	$12e3
MNEM_BVS	dw	$52e3
MNEM_CLC	dw	$11a4
MNEM_CLD	dw	$15a4
MNEM_CLI	dw	$29a4
MNEM_CLV	dw	$5da4
MNEM_CMP	dw	$45c4
MNEM_CPX	dw	$6624
MNEM_CPY	dw	$6a24
MNEM_DEC	dw	$10c5
MNEM_DEX	dw	$64c5
MNEM_DEY	dw	$68c5
MNEM_EOR	dw	$4e06
MNEM_INC	dw	$11ea
MNEM_INX	dw	$65ea
MNEM_INY	dw	$69ea
MNEM_JMP	dw	$45cb
MNEM_JSR	dw	$4e8b
MNEM_LDA	dw	$08ad
MNEM_LDX	dw	$64ad
MNEM_LDY	dw	$68ad
MNEM_LSR	dw	$4e8d
MNEM_NOP	dw	$460f
MNEM_ORA	dw	$0a70
MNEM_PHA	dw	$0931
MNEM_PHP	dw	$4531
MNEM_PHX	dw	$6531
MNEM_PHY	dw	$6931
MNEM_PLA	dw	$09b1
MNEM_PLP	dw	$45b1
MNEM_PLX	dw	$65b1
MNEM_PLY	dw	$69b1
MNEM_RMB	dw	$0dd3
MNEM_ROL	dw	$3613
MNEM_ROR	dw	$4e13
MNEM_RTI	dw	$2ab3
MNEM_RTS	dw	$52b3
MNEM_SBC	dw	$1074
MNEM_SEC	dw	$10d4
MNEM_SED	dw	$14d4
MNEM_SEI	dw	$28d4
MNEM_SMB	dw	$0dd4
MNEM_STA	dw	$0ab4
MNEM_STP	dw	$46b4
MNEM_STX	dw	$66b4
MNEM_STY	dw	$6ab4
MNEM_STZ	dw	$6eb4
MNEM_TAX	dw	$6455
MNEM_TAY	dw	$6855
MNEM_TRB	dw	$0e75
MNEM_TSB	dw	$0e95
MNEM_TSX	dw	$6695
MNEM_TXA	dw	$0b35
MNEM_TXS	dw	$5335
MNEM_TYA	dw	$0b55
MNEM_WAI	dw	$2858
;
;=====================================================
; Data
;
opcode		ds	1
addmode		ds	1

