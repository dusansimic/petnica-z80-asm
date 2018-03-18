;	=========================================
;	Title:	TRON
;	Author:	Dušan Simić
;	Date:	1118 <- internal joke about microprocessor labeling
;	=========================================


	wall:		equ	%01000000
	player1:	equ	%00001000
	player1trail:	equ	%01001000
	player2:	equ	%00010000
	player2trail:	equ	%01010000
	floor:		equ	%00000000
	org	32768

main:
	ld	hl,22891
	ld	(player1Position),hl
	ld	hl,22898
	ld	(player2Position),hl
	ld	a,$0f
	ld	(player1LastKey),a
	ld	(player2LastKey),a
	call	drawBorder
	call	drawFloor
	call	drawPlayers
	call	mainGameLoop
	ret

mainGameLoop:
	ld	a,(gameSpeed)
	ld	b,a
waitLoop:
	halt
	call readKeys1
	call readKeys2

	ld	a,$fb		; Citaj Q
	in	a,($fe)
	and	%00000001	; Testiraj dal je Q
	ret	z		; Ako je Q izadji

	dec	b
	jp	nz,waitLoop
	
	call	movePlayer1
	call	movePlayer2

	ld	a,(hasColision)
	cp	1

	jr	nz,mainGameLoop	; Ako se nije spucao

gameover:
	ld	bc,$fbfe
	in	a,(c)
	bit	0,a
	ret	z
	bit	3,a
	jp	z,main
	jr	gameover

	ret

;------------------------------------------
; Citaj kijeve i stavi ih u varijable da bi
; movePlayers znao kuda da cera
;------------------------------------------

readKeys1:
	push	bc
	
	; Citaj za igraca 1
	ld	a,$fb		; Citaj W
	in	a,($fe)
	and	%00000010	; Izoluj W
	sla	a		; Meraj levo na 3. bit
	sla	a
	ld	b,a		; Meci u b za kasnije
	ld	a,$fd		; Citaj ASD
	in	a,($fe)
	and	%00000111	; Izoluj ASD
	or	b		; Spoji ih u jedan
	cp	%00001111	; Proveri dal je ijedan stisnut
	jr	z,_keepLastKeys1 ; Ako nije nijedan kliknut preskoci cuvanje
				 ; da bi imao prosle
	ld	(player1LastKey),a ; Ovo ga mece u varijablu
_keepLastKeys1:
	pop	bc
	ret

readKeys2:
	push	bc

	; Citaj za igraca 2
	ld	a,$df		; Citaj I
	in	a,($fe)
	and	%00000100	; Izoluj I
	sla	a		; Meraj levo na 3. bit
	ld	b,a		; Mecu u b za kasnije
	ld	a,$bf		; Citaj JKL
	in	a,($fe)
	and	%00001110	; Izoluj JKL
	srl	a		; Meraj desno da stane I
	or	b		; Spoji ih u jedan
	cp	%00001111	; Proveri dal je ijedan stisnut
	jr	z,_keepLastKeys2 ; Ako nije nijedan kliknut preskoci cuvanje
				 ; da bi imao prosle
	ld	(player2LastKey),a ; Ovo ga mece u varijablu
_keepLastKeys2:
	pop bc
	ret

;------------------------------------------
; Crta ivicu crne boje koja je kao svetla
; da bi se razlikovala od poda koja je kao
; tamna
;------------------------------------------

drawBorder:
	ld	a,wall
	out	($fe),a
	ld	b,32
	ld	hl,22528	; Gornji levi cosak
_drawBorderTop:
	ld	(hl),wall
	inc	hl
	djnz	_drawBorderTop

	ld	b,32
	ld	hl,23264	; Gornji desni cosak
_drawBorderBottom:
	ld	(hl),wall
	inc	hl
	djnz	_drawBorderBottom

	ld	b,24
	ld	hl,22528	; Gornji levi cosak
	ld	de,32
_drawBorderLeft:
	ld	(hl),wall
	add	hl,de
	djnz	_drawBorderLeft

	ld	c,24
	ld	hl,22559	; Gornji desni cosak
	ld	de,32
_drawBorderRight:
	ld	(hl),wall
	add	hl,de
	djnz	_drawBorderRight

	ret

;------------------------------------------
; Crtaj pod kao tamno crnom bojom
;------------------------------------------

drawFloor:
	ld	hl,22561	; Gornji levi cosak bez ivica
	ld	c,23
_loopDrawFloor:
	dec	c
	ld	b,30
	jp	z,_exitDrawFloor
_loopDrawRow:
	ld	(hl),floor
	inc	hl
	djnz	_loopDrawRow
	inc	hl
	inc	hl
	jr	_loopDrawFloor
_exitDrawFloor:
	ret

;------------------------------------------
; Crtaj igrace (plava i narandzasta)
; Ali jbg tesko napraviti narandzastu pa
; nek bude samo crvena
;------------------------------------------

drawPlayers:
	push	hl
	ld	hl,(player1Position)	; Levo stavi plavog
	ld	(hl),player1
	ld	hl,(player2Position)	; Desno stavi crvenog
	ld	(hl),player2
	pop	hl
	ret

;------------------------------------------
; Setaj igrace po podu na osnovu lastKey
; varijabli
;------------------------------------------

movePlayer1:
	push	af
	push	bc
	push	de
	push	hl
	ld	de,32			; Za pomeranje dole
	ld	bc,-32			; Za pomeranje gore
	ld	hl,(player1Position)	; Ucitaj poziciju igraca 1
	ld	a,(player1LastKey)	; Ucitaj dugmice igraca 1
	bit	3,a			; Ako je W
	jr	z,_moveUp1
	bit	0,a			; Ako je A
	jr	z,_moveLeft1
	bit	1,a			; Ako je S
	jr	z,_moveDown1
	bit	2,a			; Ako je D
	jr	z,_moveRight1

	jr	_exitMove1
_moveUp1:
	add	hl,bc			; Idi gore da proveri dal je pod
	ld	a,floor
	cp	(hl)			; Proveri da li je pod
	jp	nz,_colision1		; Ako nije pod onda nemoj ici, inace ga pomeraj
	ld	(hl),player1
	add	hl,de
	ld	(hl),player1trail
	add	hl,bc
	ld	(player1Position),hl	; Cuvaj novu poziciju
	jr	_exitMove1
_moveLeft1:
	dec	hl
	ld	a,floor
	cp	(hl)
	jr	nz,_colision1
	ld	(hl),player1
	inc	hl
	ld	(hl),player1trail
	dec	hl
	ld	(player1Position),hl
	jr	_exitMove1
_moveDown1:
	add	hl,de
	ld	a,floor
	cp	(hl)
	jr	nz,_colision1
	ld	(hl),player1
	add	hl,bc
	ld	(hl),player1trail
	add	hl,de
	ld	(player1Position),hl
	jr	_exitMove1
_moveRight1:
	inc	hl
	ld	a,floor
	cp	(hl)
	jr	nz,_colision1
	ld	(hl),player1
	dec	hl
	ld	(hl),player1trail
	inc	hl
	ld	(player1Position),hl
	jr	_exitMove1
_colision1:
	ld	a,1
	ld	(hasColision),a
_exitMove1:
	pop	hl
	pop	de
	pop	bc
	pop	af
	ret

movePlayer2:
	push	af
	push	bc
	push	de
	push	hl
	ld	de,32			; Za pomeranje dole
	ld	bc,-32			; Za pomeranje gore
	ld	hl,(player2Position)
	ld	a,(player2LastKey)
	bit	3,a
	jr	z,_moveUp2
	bit	2,a
	jr	z,_moveLeft2
	bit	1,a
	jr	z,_moveDown2
	bit	0,a
	jr	z,_moveRight2

	jr	_exitMove2
_moveUp2:
	add	hl,bc			; Idi gore da proveri dal je pod
	ld	a,floor
	cp	(hl)			; Proveri da li je pod
	jr	nz,_colision2		; Ako nije pod onda nemoj ici, inace ga pomeraj
	ld	(hl),player2
	add	hl,de
	ld	(hl),player2trail
	add	hl,bc
	ld	(player2Position),hl	; Cuvaj novu poziciju
	jr	_exitMove2
_moveLeft2:
	dec	hl
	ld	a,floor
	cp	(hl)
	jr	nz,_colision2
	ld	(hl),player2
	inc	hl
	ld	(hl),player2trail
	dec	hl
	ld	(player2Position),hl
	jr	_exitMove2
_moveDown2:
	add	hl,de
	ld	a,floor
	cp	(hl)
	jr	nz,_colision2
	ld	(hl),player2
	add	hl,bc
	ld	(hl),player2trail
	add	hl,de
	ld	(player2Position),hl
	jr	_exitMove2
_moveRight2:
	inc	hl
	ld	a,floor
	cp	(hl)
	jr	nz,_colision2
	ld	(hl),player2
	dec	hl
	ld	(hl),player2trail
	inc	hl
	ld	(player2Position),hl
	jr	_exitMove2
_colision2:
	ld	a,1
	ld	(hasColision),a
_exitMove2:
	pop	hl
	pop	de
	pop	bc
	pop	af
	ret


;-----------------------------------
;		DATA
;-----------------------------------

; Game settings
gameSpeed:		db	5

; Game variables
player1Position:	dw	22891
player1LastKey:		db	$0f
player2Position:	dw	22898
player2LastKey:		db	$0f
hasColision:		db	0