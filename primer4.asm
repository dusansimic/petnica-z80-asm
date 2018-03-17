	LD	HL,22895	; Adresa atributa na sredini 12. reda.
	LD	(HL),%00010000		; Smeštanje registra A na adresu (HL).
	INC	HL
	LD	(HL),%00100000
	INC	HL
	LD	(HL),%00001000
	RET			; Povratak u BASIC.
