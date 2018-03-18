readKeys:

	push	bc

	; Read IJKL keys and merge all to register A:

	ld	a,$df		; Read from $DFFE I/O address for YUIOP row
	in	a,($fe)
	and	%00000100	; Isolate bit 2 = I key
	srl	a		; Move to position 0
	srl	a
	ld	b,a		; Save for later use to merge I with JKL
	ld	a,$bf		; Read from $BFFE I/O address for HJKL(ENTER) row
	in	a,($fe)
	and 	%00001110	; Isolate bits 3,2 and 1 = JKL keys
	or	b		; Merge with I key, now we have JKLI as bits 3210

	ld	a,$fb		; Read from $FBFE I/O address for TREWQ
	in	a,($fe)
	and	%00000010	; Isolate bit 1 = W key
	sll	a		; Move to position 4
	sll	a
	sll	a
	ld	b,a		; Save for later use to merge W with ASD
	ld	a,$fd		; Read from $FDFE I/O address for GFDSA row
	in	a,($fe)
	and	%00000111
	srl	a
	srl	a
	srl	a
	or	b
	cp	%11111111
	jr	z,_keepLastKeys	; If no directional keys are pressed skip storing zero
				; value over the current/last value, which will allow for
				; PacMan style constant movement based on last direction 

	ld	(lastKeys),a	; If directional keys are pressed save the new 
				; result for later use
_keepLastKeys:

	pop	bc

	ret