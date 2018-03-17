;----------------------------------------------------------------------
; Primer 10:
; Pomeranje trepćućeg crveno/zelenog atributa levo-desno i gore/dole
; po celom ekranu. Kontrola igrača se vrši pritiskom na tastere 
; J (levo), K (dole), L (desno), I (gore), prekid programa H (halt).
;
; Primer ilustruje iste principe sa proverama adresa i pisanjem
; čistih univerzalnih rutina, samo za nešto složeniji slučaj. Takođe
; ilustruje kako se kombinuju dva očitavanja stanja tastera sa dve 
; različite I/O adrese kako bi se u jedan bajt stavila zgodna
; kombinacija bitova stanja pritisnutih tastera za upravljanje igračem.
;----------------------------------------------------------------------

PLAYER:	EQU	%10010100	; Definicija asemblerske konstante za igrača. 
BOARD:	EQU	%00111000	; Definicija asemblerske konstante za tablu.

	ORG	32768

	CALL	INITIALIZE
	
	; Glavna petlja:
LOOP:
	HALT			; Sačekaj interapt (1/50s).
	CALL	READ_KEYS

	BIT	0,A		; Test stanja bita 0 = taster I (gore).
	CALL	Z, MOVE_UP	; Ako je pritisnut pomeri se gore.
	BIT	2,A		; Test stanja bita 2 = taster K (dole).
	CALL	Z,MOVE_DOWN	; Ako je pritisnut pomeri se dole.
	BIT	3,A		; Test stanja bita 3 = taster J (levo).
	CALL	Z,MOVE_LEFT	; Ako je pritisnut pomeri se levo.
	BIT	1,A		; Test stanja bita 1 = taster L (desno).
	CALL	Z,MOVE_RIGHT	; Ako je pritisnut pomeri se desno.
	
	BIT	4,A		; Test stanja bita 4 = taster H.
	RET	Z		; Povratak u BASIC ako je pritisnut.
	
	JP	LOOP		; Ako nije pritisnuto S nazad na početak.

;----------------------------------------------------------------------
; Inicijalizacija parametara.
;----------------------------------------------------------------------

INITIALIZE:

	LD	HL,22895	; Adresa atributa na sredini 12. reda.
	LD	(HL),PLAYER	; Upiši atribut na početnu poziciju.
	RET

;----------------------------------------------------------------------
; Očitavanje stanja tastera. U očitano stanje HJKL<ENTER> reda se bit
; koji reprezentuje <ENTER> zameni stanjem tastera I iz očitanog stanja
; YUIOP reda. Krajnji rezultat u registru A je 111HJKLI.
;----------------------------------------------------------------------

READ_KEYS:

	LD	BC,$BFFE	; Adresa porta na kome se nalaze HJKL<ENTER>.
	IN	A,(C)		; Pročitaj trenutno stanje tastera HJKL<ENTER>.
	LD	BC,$DFFE	; Adresa porta na kome se nalaze YUIOP.
	IN	D,(C)		; Pročitaj trenutno stanje tastera YUIOP.
	BIT	2,D		; Proveri da li je pritisnut taster I (bit 3 treba da bude 0).
	RET	NZ		; Ako nije pritisnut registar A već sadrži šta treba, kraj rutine.
	RES	0,A		; Ako jeste resetuj bit 0 u registru A.
	RET

;----------------------------------------------------------------------
; Pomeranje igrača na gore, ali samo ako već nije skroz gore. Ako jeste
; onda su bitovi koji određuju red u HL registru xxxxxx00 000xxxxx i
; pomeranje se preskače.
;----------------------------------------------------------------------

MOVE_UP:

	PUSH	AF		; Stavi AF na stek da se sačuva pročitano stanje tastera.
	PUSH	BC
	
	; Provera da li smo u prvom redu:
	
	LD	A,H		; Prebaci gornji deo adrese u A.
	AND	%00000011	; Maskiraj najniža dva bita.
	LD	B,A		; Privremeno skloni A u B.
	LD	A,L		; Prebaci donji deo adrese u A.
	AND	%11100000	; Maskiraj najviša dva bita.
	OR	B		; Kombinuj oba dela maskiranih bitova.
	CP	%00000000	; Uporedi sa nulom.
	JP	Z,MU_EXIT		; Ako je jednako preskoči pomeranje.
	
	; Ako nije prvi red pomeri igrača na gore:
	
	LD	(HL),BOARD	; Obriši igrača (postavi boju pozadine).
	LD	BC,-32		; Pripremi pomeraj unazad na atribut iznad.
	ADD	HL,BC		; Izračunaj adresu atributa iznad.
	LD	(HL),PLAYER	; Postavi boju igrača na novu adresu.

	; Izlaz:
MU_EXIT:
	POP	BC
	POP	AF		; Vrati AF sa steka da se osveži pročitano stanje tastera.

	RET
	
;----------------------------------------------------------------------
; Pomeranje igrača na dole, ali samo ako već nije skroz dole. Ako jeste
; onda su bitovi koji određuju red u HL registru xxxxxx10 111xxxxx i
; pomeranje se preskače.
;----------------------------------------------------------------------

MOVE_DOWN:

	PUSH	AF		; Stavi AF na stek da se sačuva pročitano stanje tastera.
	PUSH	BC
	
	; Provera da li smo u poslednjem redu:
	
	LD	A,H		; Prebaci gornji deo adrese u A.
	AND	%00000011	; Maskiraj najniža dva bita.
	LD	B,A		; Privremeno skloni A u B.
	LD	A,L		; Prebaci donji deo adrese u A.
	AND	%11100000	; Maskiraj najviša dva bita.
	OR	B		; Kombinuj oba dela maskiranih bitova.
	CP	%11100010	; Uporedi sa rekombinovanim brojem poslednjeg reda.
	JP	Z,MD_EXIT		; Ako je jednako preskoči pomeranje.
	
	; Ako nije poslednji red pomeri igrača na dole:
	
	LD	(HL),BOARD	; Obriši igrača (postavi boju pozadine).
	LD	BC,32		; Pripremi pomeraj unapred na atribut ispod.
	ADD	HL,BC		; Izračunaj adresu atributa ispod.
	LD	(HL),PLAYER	; Postavi boju igrača na novu adresu.
	
	; Izlaz:
MD_EXIT:
	POP	BC
	POP	AF		; Vrati AF sa steka da se osveži pročitano stanje tastera.

	RET
	
;----------------------------------------------------------------------
; Pomeranje igrača u levo, ali samo ako već nije skroz levo. Ako jeste
; onda su najnižih pet bitova u registarskom paru HL nule. Ovo se svodi
; na test samo registra L da li je xxx00000 i ako jeste pomeranje se
; preskače.  
;----------------------------------------------------------------------

MOVE_LEFT:

	PUSH	AF		; Stavi AF na stek da se sačuva pročitano stanje tastera.

	; Provera da li smo već na levoj ivici:

	LD	A,L		; Prebaci registar L u registar A.
	AND	A,%00011111	; Maskiraj tako da se izdvoji donjih pet bitova.
	JP	Z,ML_EXIT		; Ako je rezultat nula to je leva ivica pa preskoči pomeranje.
	
	; Ako nije leva ivica pomeri igrača u levo:
	
	LD	(HL),BOARD	; Obriši igrača (postavi boju pozadine).
	DEC	HL		; Pređi na prethodnu adresu.
	LD	(HL),PLAYER	; Postavi boju igrača na novu adresu.
	
	; Izlaz:
ML_EXIT:
	POP	AF		; Vrati AF sa steka da se osveži pročitano stanje tastera.
	RET

;----------------------------------------------------------------------
; Pomeranje igrača u desno, ali samo ako već nije skroz desno. Ako jeste
; onda su najnižih pet bitova u registarskom paru HL jedinice.Ovo se svodi
; na test samo registra L da li je xxx11111 i ako jeste pomeranje se
; preskače.
;----------------------------------------------------------------------

MOVE_RIGHT:
	
	PUSH	AF		; Stavi AF na stek da se sačuva pročitano stanje tastera.

	; Provera da li smo već na desnoj ivici:

	LD	A,L		; Prebaci registar L u registar A.
	AND	A,%00011111	; Maskiraj tako da se izdvoji donjih pet bitova.
	CP	%00011111	; Uporedi A sa 00011111, Z flag se setuje ako je jednakost.
	JP	Z,MR_EXIT		; Ako je Z flag setovan to je desna ivica pa preskoči pomeranje.
	
	; Ako nije desna ivica pomeri igrača u desno:
	
	LD	(HL),BOARD	; Obriši igrača (postavi boju pozadine).
	INC	HL		; Pređi na prethodnu adresu.
	LD	(HL),PLAYER	; Postavi boju igrača na novu adresu.
	
	; Izlaz:
MR_EXIT:
	POP	AF		; Vrati AF sa steka da se osveži pročitano stanje tastera.
	RET