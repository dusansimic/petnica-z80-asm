	LD	HL,22895	; Adresa atributa na sredini 12. reda.
	LD	A,%00001000	; Vrednost koja će se upisati u atribut.
	LD	(HL),A		; Smeštanje registra A na adresu (HL).
	RET			; Povratak u BASIC.
