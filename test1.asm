; Assemble with -x option

	include	"xkim.inc"
	code
	org	$0200
Test	lda	#0
loop	inc	a
	cmp	#2
	bne	loop
	nop
	nop
	stz	0
	smb	0,0	;set bit zero
	bbs	0,0,Test

	org	AutoRun
	dw	Test
	end
