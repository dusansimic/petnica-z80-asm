WALL:	EQU	%00001000
PLAYER:	EQU	%00010000
FLOOR:	EQU	%00111000

	ORG	32768
MAIN:
	CALL	DRAW_BORDER
	LD	HL,22895
	LD	BC,$FDFE
	LD	(HL),PLAYER
CITAJ:
	HALT
	LD	BC,$FBFE
	IN	A,(C)
	BIT	0,A
	RET	Z
	BIT	1,A
	CALL	Z,MOVE_UP
	LD	BC,$FDFE
	IN	A,(C)
	BIT	0,A
	CALL	Z,MOVE_LEFT
	BIT	1,A
	CALL	Z,MOVE_DOWN
	BIT	2,A
	CALL	Z,MOVE_RIGHT
	JP	CITAJ

	RET

DRAW_BORDER:
	LD	C,32
	LD	HL,22528
DRAW_BORDER_TOP:
	LD	(HL),WALL
	INC	HL
	DEC	C
	JP	NZ,DRAW_BORDER_TOP

	LD	C,32
	LD	HL,23264
DRAW_BORDER_BOTTOM:
	LD	(HL),WALL
	INC	HL
	DEC	C
	JP	NZ,DRAW_BORDER_BOTTOM

	LD	C,24
	LD	HL,22528
	LD	DE,32
DRAW_BORDER_LEFT:
	LD	(HL),WALL
	ADD	HL,DE
	DEC	C
	JP	NZ,DRAW_BORDER_LEFT

	LD	C,24
	LD	HL,22559
	LD	DE,32
DRAW_BORDER_RIGHT:
	LD	(HL),WALL
	ADD	HL,DE
	DEC	C
	JP	NZ,DRAW_BORDER_RIGHT

	RET

MOVE_LEFT:
	PUSH	AF
	DEC	HL
	LD	A,WALL
	CP	(HL)
	JP	Z,ML_SKIP
	LD	(HL),PLAYER
	INC	HL
	LD	(HL),FLOOR
	DEC	HL
	POP	AF
	RET
ML_SKIP:
	INC	HL
	POP	AF
	RET

MOVE_RIGHT:
	PUSH	AF
	INC	HL
	LD	A,WALL
	CP	(HL)
	JP	Z,MR_SKIP
	LD	(HL),PLAYER
	DEC	HL
	LD	(HL),FLOOR
	INC	HL
	POP	AF
	RET
MR_SKIP:
	DEC	HL
	POP	AF
	RET

MOVE_UP:
	PUSH	AF
	LD	DE,-32
	LD	BC,32
	ADD	HL,DE	; Kao smanji... idi gore pa vidi boju
	LD	A,WALL
	CP	(HL)
	JP	Z,MU_SKIP
	LD	(HL),PLAYER
	ADD	HL,BC	; Sad idi dole da bi se pomerio
	LD	(HL),FLOOR
	ADD	HL,DE
	POP	AF
	RET
MU_SKIP:
	ADD	HL,BC
	POP	AF
	RET

MOVE_DOWN:
	PUSH	AF
	LD	DE,32
	LD	BC,-32
	ADD	HL,DE	; Kao smanji... idi gore pa vidi boju
	LD	A,WALL
	CP	(HL)
	JP	Z,MD_SKIP
	LD	(HL),PLAYER
	ADD	HL,BC	; Sad idi dole da bi se pomerio
	LD	(HL),FLOOR
	ADD	HL,DE
	POP	AF
	RET
MD_SKIP:
	ADD	HL,BC
	POP	AF
	RET