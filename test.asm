DISPLAY_FIELD:
	LD HL,$5821
	LD B,22
	LD C,30
.FILL_ROW:
	LD (HL),%00100000
	INC HL
	DEC C
	JR NZ,.FILL_ROW

	INC HL
	INC HL
	LD C,30
	DEC B
	JR NZ,.FILL_ROW

	RET