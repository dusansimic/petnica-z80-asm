	LD	HL,22895	; Adresa atributa na sredini 12. reda.
	LD	A,%00010000	; Vrednost koja će se upisati u atribut.
	LD	(HL),A		; Smeštanje registra A na adresu (HL).
	INC	HL
	LD 	A,%00100000
	LD	(HL),A
	INC	HL
	LD 	A,%00001000
	LD	(HL),A
	RET			; Povratak u BASIC.
