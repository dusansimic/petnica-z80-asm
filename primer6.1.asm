	ORG	32768
	LD	HL,22528
	LD	B,24
REPEAT:
	LD	C,8
	LD	A,0
LOOP:
	LD	(HL),A
	INC	HL
	LD	(HL),A
	INC	HL
	LD	(HL),A
	INC	HL
	LD	(HL),A
	INC	HL
	ADD	A,%1000
	DEC	C
	JP	NZ,LOOP
	LD	C,8
	DEC	B
	JP	NZ,REPEAT
	RET
