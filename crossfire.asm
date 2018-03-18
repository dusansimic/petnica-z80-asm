;===============================================================================
; Crossfire game. Ivan Glisin, December 2015
; Modified 2016-03-19, Petnica Science Center
;===============================================================================

	org	32768

main:	
	call	clearScreen
	call	initializeScreen
	call	initializeGame
	call	mainGameLoop
	ret

; ------------------------------------------------------------------------------
; Main game loop.
; ------------------------------------------------------------------------------

mainGameLoop:

	; Prepare the board and draw current content. This is done before the
	; interrupt delay loop so that the human eye can see the positions and
	; move the player box accordingly. Moving this set of calls (as initially 
	; done) after the interrupt delay causes human reaction to be late by a 
	; fraction of a second causing missed turns, so it is here to avoid that:

	call	eraseTails
	call	removeExitingBeams
	call	insertNewBeams	
	call	drawBoard

	; Wait and scan for key presses:

	ld	a,(gameSpeed)
	ld	b,a

waitLoop:

	halt			; Wait for the interrupt
	call	readKeys	; Read directional control keys IJKL

	; Check for the in	a,($fe)Q key to exit program:

	ld	a,$fb		; Read from $FBFE I/O address for TREWQ row
	in	a,($fe)
	and	%00000001	; Test for Q key (bit 0)
	ret	z		; If pressed (bit is zero) then exit

	; Check wait condition:

	dec	b		; Decrement wait counter
	jp	nz,waitLoop	; Keep waiting if not reached 0

	; The rest of the game functions performing game logic:

	call	movePlayer	
	call	moveBeams
	call	detectCatch
	call	detectCollision
	
	jr	z,mainGameLoop	; No collision detected

	ret

; ------------------------------------------------------------------------------
; Game initialization. Resets all parameters to the initial values.
; ------------------------------------------------------------------------------

initializeGame:

	; Initialize random number generator:

	ld	a,r
	ld	(_rnd+4),a

	; Set starting beam length to 2, level 1:

	ld	a,2
	ld	(maxBeamLength),a

	; Prepare remaining level duration. This will be reinitialized
	; after each level increase in detectCatch routine:
	
	ld	a,(levelDuration)
	ld	(levelRemaining),a

	; Initialize player and target positions:

	call	generateRandomPosition
	ld	(playerPosition),bc
	ld	(playerLastPos),bc

	call	generateRandomPosition
	ld	(targetPosition),bc
	ld	(targetLastPos),bc

	; Prepare beam gap counter for the current game difficulty
	; (user can change that):

	ld	a,(beamGapSize)
	ld	(beamGapCounter),a 

	; Clear pressed keys, initially player does not moveme:

	ld	a,$0f
	ld	(lastKeys),a

	; Initialize head positions to inactive:

	ld	b,44
	ld	hl,vHeadsPos

_clearHeadsLoop:
	
	ld	(hl),$ff
	inc	hl
	dec	b
	jr	nz,_clearHeadsLoop

	; Initialize tail lengths to zero:

	ld	b,44
	ld	hl,vBeamsLen

_clearTailsLoop:

	ld	(hl),0
	inc	hl
	dec	b
	jr	nz,_clearTailsLoop

	ret

; ------------------------------------------------------------------------------
; Detects when player catches the target.
; ------------------------------------------------------------------------------

detectCatch:

	; Compare player and target position:

	ld	bc,(playerPosition)
	ld	de,(targetPosition)
	
	ld	a,b			; Compare rows
	cp	d
	ret	nz			; Return if no collision

	ld	a,c			; Compare columns
	cp	e
	ret	nz			; Return if no collision

	; In case of collision save current position to last position:

	ld	bc,(targetPosition)
	ld	(targetLastPos),bc

	; Move target to the new position if player has hit the target.
	; Position can be the same as current player position but since
	; the player continuously moves it will leave target behind and
	; no collision will be detected until player returns to hit target:

	call	generateRandomPosition
	ld	(targetPosition),bc

	; Check if condition for increasing beam lenghth (and game level) is reached:

	ld	a,(levelRemaining)
	dec	a
	ld	(levelRemaining),a
	ret	nz			; No need to go to the next level yet

	; Increase beam length (and game level) if condition is reached,
	; but only if beam lenght has not reached 11 (level 10):

	ld	a,(maxBeamLength)	; Load current beam length
	cp	11			; Compare to the limit
	ret	z			; Maximal length reached, return
	inc	a			; If not, increase beam length
	ld	(maxBeamLength),a
	ld	a,(levelDuration)	; Reinitialize level duration
	ld	(levelRemaining),a
	
	ret

; ------------------------------------------------------------------------------
; Detects collision between the player and the chaser. It is based on checking
; if player color attributes are on the expected position, so player color
; attributes MUST BE unique. If player color attributes are not detected it will
; be indicated by border color changes, and Z flag will be set.
; ------------------------------------------------------------------------------

detectCollision:

	ld	bc,(playerLastPos)	; Last player position
	ld	a,c			; Center row
	add	a,5
	ld	c,a
	call	calcAttrAddress		; Calculate attribute address
	ld	a,(playerColor)		; Compare with player color
	cp	(hl)

	ret	z			; No collision, player is there

	; Collision alert:

	ld	c,5		; Color/black border alternations

_alertLoop:

	ld	a,2		; Change border to color
	out	(254),a
	halt
	halt
	halt
	halt
	halt
	ld	a,0		; Change border to black
	out	(254),a
	halt
	halt
	halt
	halt
	halt
	dec	c
	jr	nz,_alertLoop

	or	1		; Reset Z flag to indicate collision

	; Reset screen colors, but with no CLS:

	ld	a,%00111000	; Paper and ink colors
	ld	(23693),a	; PAPER
	ld	a,1		; Border color
	call	8859		; BORDER

	ret

; ------------------------------------------------------------------------------
; Reads keys as as I(up), K(down), J(left), L(right), merges result into a
; single byte where JKLI are bits 3210, then stores to the "lastKeys" variable.
; ------------------------------------------------------------------------------

readKeys:

	push	bc

	; Read IJKL keys and merge all to register A:

	ld	a,$df		; Read from $FBFE I/O address for YUIOP row
	in	a,($fe)
	and	%00000100	; Isolate bit 2 = I key
	srl	a		; Move to position 0
	srl	a
	ld	b,a		; Save for later use to merge I with JKL
	ld	a,$bf		; Read from $FBFE I/O address for HJKL(ENTER) row
	in	a,($fe)
	and 	%00001110	; Isolate bits 3,2 and 1 = JKL keys
	or	b		; Merge with I key, now we have JKLI as bits 3210
	cp	%00001111	; Compare to no keys pressed value ($0f)
	jr	z,_keepLastKeys	; If no directional keys are pressed skip storing zero
				; value over the current/last value, which will allow for
				; PacMan style constant movement based on last direction 
 
	ld	(lastKeys),a	; If directional keys are pressed save the new 
				; result for later use
_keepLastKeys:

	pop	bc

	ret

; ------------------------------------------------------------------------------
; Moves the player based on the keyboard input. Direction is controlled
; as I(up), K(down), J(left), L(right) - if more than one key got pressed 
; the first is processed and the rest is ignored. The board size is 22x22.
; ------------------------------------------------------------------------------

movePlayer:

	push	bc

	; Set current position as last position. If nothing happens here both 
	; positions will remain the same, indicating player has not been moved:

	ld	bc,(playerPosition)
	ld	(playerLastPos),bc

	; Check each key and move the player accordingly:	
	
	ld	a,(lastKeys)		; Load last keys pressed

	bit	0,a			; Check for I key and move up if pressed
	jr	z,_playerUp
	bit	2,a			; Check for K key and move down if pressed
	jr	z,_playerDown
	bit	3,a			; Check for J key and move left if pressed
	jr	z,_playerLeft
	bit	1,a			; Check for L key and move right if pressed
	jr	z,_playerRight
	jr	_exitMove		; No keys are pressed

_playerUp:

	ld	bc,(playerPosition)
	ld	a,b
	cp	0			; Test if at the top of the board
	jr	z,_exitMove		; If so do nothing

	dec	b			; Move one row up
	ld	(playerPosition),bc	; Save new position
	jr	_exitMove

_playerDown:

	ld	bc,(playerPosition)
	ld	a,b
	cp	21			; Test if at the bottom of the board
	jr	z,_exitMove		; If so do nothing

	inc	b			; Move one row up
	ld	(playerPosition),bc	; Save new position
	jr	_exitMove

_playerLeft:

	ld	bc,(playerPosition)
	ld	a,c
	cp	0			; Test if at the left of the board
	jr	z,_exitMove		; If so do nothing

	dec	c			; Move one row left
	ld	(playerPosition),bc	; Save new position
	jr	_exitMove

_playerRight:

	ld	bc,(playerPosition)
	ld	a,c
	cp	21			; Test if at the right of the board
	jr	z,_exitMove		; If so do nothing

	inc	c			; Move one row right
	ld	(playerPosition),bc	; Save new position
	jr	_exitMove		; Not necessary, but for good code structure

_exitMove:

	pop	bc

	ret

; ------------------------------------------------------------------------------
; Cleanup routine. Removes all beams with position at the edge of the board
; where length is reduced to zero (condition: len=0 & pos=21).
; ------------------------------------------------------------------------------

removeExitingBeams:

	ld	c,44			; Total number of beams counter(22+22)
	ld	hl,vHeadsPos		; Start of beam head position area
	ld	de,vBeamsLen		; Start of beam length area

_removeBeamsLoop:

	ld	a,(de)			; Load beam length
	cp	0			; Test if beam length is zero
	jr	nz,_removeNextBeam	; If not go to the next beam

	ld	a,(hl)			; Load beam head position
	cp	21			; Test if beam has hit the board edge
	jr	nz,_removeNextBeam	; If not go to the next beam

	ld	(hl),$ff		; Mark beam as inactive

_removeNextBeam:

	inc	hl
	inc	de
	dec	c
	jr	nz,_removeBeamsLoop

	ret

; ------------------------------------------------------------------------------
; Inserts new beams at random position. New beams will be inserted only in 
; rows or columns with no currently active beams. Initial values for a new beam
; is position=0 and length=1 (replacing position=$ff and length=0)
; ------------------------------------------------------------------------------

insertNewBeams:

	; Check if inserting is required or still generating a gap:

	ld	hl,beamGapCounter	; beamGapCounter variable address
	dec	(hl)			; Decrease and check if still generating gap
	ret	nz			; Return, still generating a gap

	; If zero, reinitialize gap counter:

	ld	a,(beamGapSize)
	ld	(beamGapCounter),a

_retryInsert:

	; Generate random number from 0..43:
	
	call	generateRandom		; Generate random 0..255
	and	%00111111		; Reduce to 0..63 range
	cp	44			; Compare to 44
	jp	p,_retryInsert		; If A > 43 try again

	; Check if beam at generated random position is active or not: 

	ld	hl,vHeadsPos		; Head positions area start address
	ld	bc,0			; Move random offset from A to BC
	ld	c,a
	add	hl,bc			; Calculate head address
	ld	a,(hl)			; Load head position
	cp	$ff			; Check if beam is active
	jr	nz,_retryInsert		; If active try inserting somewhere else
	
	; If beam is not active it is OK to insert a new one at generated position:

	ld	(hl),0			; Initialize head position
	ld	hl,vBeamsLen		; Initialize beam length
	add	hl,bc
	ld	(hl),1

	ret

; ------------------------------------------------------------------------------
; Moves all active beams. 
; ------------------------------------------------------------------------------

moveBeams:

	ld	c,44			; Total number of beams (22+22) counter
	ld	hl,vHeadsPos		; Start of beam head position area
	ld	de,vBeamsLen		; Start of beam length area

_moveBeamsLoop:

	ld	a,(hl)			; Load beam head position
	cp	$ff			; Test if beam is active
	jr	z,_moveNextBeam		; If not active ($ff) go to the next beam

	; CASE 1: beam is at the end, head position does not change, only 
	; length decreases until it is gone. Note that separate routine does
	; board cleanup after this code reduces length to zero:

_beamEnds:

	ld	a,(hl)			; Load head position
	cp	21			; Check if head has hit the end
	jr	nz,_beamStarts		; If there is still room to move, skip to next case

	ld	a,(de)			; Decrease beam length
	dec	a
	ld	(de),a

	jr	_moveNextBeam

	; CASE 2: beam is at the beginning and still growing, both position and
	; length are increasing. Beam is at the beginning if position is less
	; than final allowed beam length: 

_beamStarts:

	ld	a,(maxBeamLength)	; Load final allowed beam length
	ld	b,a			; Move to B to save A for comparison later
	dec	b			; Decrease length by 1 to indicate incomplete beam
	ld	a,(hl)			; Load beam position
	cp	b			; Compare position and length as pos-(len-1)
	jp	p,_beamTravels		; If zero or positive full beam is visible, skip to next case

	inc	a			; Increase head position
	ld	(hl),a
	ld	a,(de)			; Increase beam length
	inc	a
	ld	(de),a

	jr	_moveNextBeam

	; CASE 3: beam is traveling over the board, not touching any edge.
	; Just head position is increasing, length is already at the maxBeamLength.
	; No tests are necessary since this is the only remaining case:

_beamTravels:
	
	inc	(hl)			; Move head position only, length remains the same

	; Moves to the next beam:

_moveNextBeam:

	inc	hl
	inc	de
	dec	c
	jr	nz,_moveBeamsLoop

	ret

; ------------------------------------------------------------------------------
; Erases tails. 
; ------------------------------------------------------------------------------

eraseTails:

	ld	c,0			; Vertical beams counter
	ld	hl,vHeadsPos		; Start of beam head position area
	ld	de,vBeamsLen		; Start of beam length area

_removeTailsLoop:

	ld	a,(de)			; Load tail length for the current beam
	ld	b,a
	ld	a,(hl)			; Load head position for the current beam
	cp	$ff			; If inactive go to the next beam
	jr	z,_nextTail
	sub	b			; Calculate tail position as Head-Length
	jp	m,_nextTail		; If negative beam is still growing, skip removal

	ld	b,a			; Prepare row in B (column is already in C)
	call	eraseBeamBlock

_nextTail:

	inc	hl
	inc	de
	inc	c
	ld	a,c
	cp	44
	jr	nz,_removeTailsLoop

	ret

; ------------------------------------------------------------------------------
; Draws the entire board with all visual elements. Takes care of the proper
; order redrawing elements and offsets to center the board horizontally.
; ------------------------------------------------------------------------------

drawBoard:

	; Remove target from the last position:

	ld	bc,(targetLastPos)
	call	eraseTargetBlock	

	; Display target on the current position:

	ld	bc,(targetPosition)
	call	displayTargetBlock

	; Remove player from the last position:

	ld	bc,(playerLastPos)
	call	erasePlayerBlock
	
	; Display player on the current position:

	ld	bc,(playerPosition)	
	call	displayPlayerBlock

	; Display beams:

	ld	c,0			; Beams counter
	ld	hl,vHeadsPos		; Start of beam head position area
	ld	de,vBeamsLen		; Start of beam length area

_displayBeamsLoop:

	ld	b,(hl)			; Load head position for the current beam
	ld	a,b			; Check if beam is active
	cp	$ff
	jr	z,_nextBeam		; If inactive, skip

	ld	a,(de)			; Prepare length for the current beam

_singleBeamLoop:

	push	af			; Display single beam element
	call	displayBeamBlock
	pop	af
	
	dec	b			; Move beam position up 
	dec	a			; Decrease beam lenght
	jr	nz,_singleBeamLoop

_nextBeam:
	
	inc	hl
	inc	de
	inc	c
	ld	a,c
	cp	44
	jr	nz,_displayBeamsLoop
	
	ret

; ------------------------------------------------------------------------------
; Displays or erases beam block on a 22x22 board. It adjusts coordinates to
; center the board and transposes coordinates for columns 22..43. Takes care
; of odd/even rows and cross-drawing beams every other row.
; ------------------------------------------------------------------------------

displayBeamBlock:

	push	de			; Save DE (beam length, not used here)
	ld	de,vBeamChar		; Assume vertical beam will be displayed
	ld	a,c			; Check if C>21 for horizontal beam
	cp	22
	jp	m,_setBeamColor		; C in 0..21 stays vertical
	ld	de,hBeamChar		; If not, load horizontal beam character

_setBeamColor:

	ld	a,(beamColor)		; Set beam color
	jr	_reverse

eraseBeamBlock:

	push	de			; Save DE (beam length, not used here)
	ld	de,boardChar
	ld	a,(boardColor)		; Set board color
	jr	_reverse

_reverse:

	push	bc			; Save original coordinates
	push	af			; Save color for later use

	; Reverse column position for odd rows. Stays the same for even rows:
	
	bit	0,c			; Check if even/odd column for position reversal
	jr	z,_transpose		; If even skip to transpose
	
	ld	a,21			; Reverse position
	sub	b
	ld	b,a

_transpose:

	; Transpose for upper half:

	ld	a,c			; Load column to A
	cp	22			; Check if C points to columns 22..43
	jp	m,_center		; If in 0..21 range do not transpose

	; For columns in the 22..43 range normalize to 0..21 and transpose:

	sub	22			; Normalize column 22..43 to 0..21 (value from C)
	ld	c,b			; B -> C
	ld	b,a			; A -> B (normalized C already in A)

_center:

	; Move column to the center of the screen by moving it 5 characters to the right:

	ld	a,c
	add	a,5
	ld	c,a

	; Coordinates are ready, display or erase beam:

	call	displayChar		; Display or erase beam block, address already in DE
	pop	af			; Restore selected color
	call	setAttribute		; Display block at normalized, transposed,
					; and adjusted coordinates

	pop	bc			; Restore original coordinates
	pop	de			; Restore DE (beam length)
	ret

; ------------------------------------------------------------------------------
; Displays or erases player and target on a 22x22 board. It adjusts coordinates
; to center the boar.
; ------------------------------------------------------------------------------

displayPlayerBlock:

	push	bc
	ld	a,c
	add	a,5
	ld	c,a
	ld	de,playerChar
	call	displayChar
	ld	a,(playerColor)
	call	setAttribute
	pop	bc
	ret

erasePlayerBlock:

	push	bc
	ld	a,c
	add	a,5
	ld	c,a
	ld	de,boardChar
	call	displayChar
	ld	a,(boardColor)
	call	setAttribute
	pop	bc
	ret

displayTargetBlock:

	push	bc
	ld	a,c
	add	a,5
	ld	c,a
	ld	de,targetChar
	call	displayChar
	ld	a,(targetColor)
	call	setAttribute
	pop	bc
	ret

eraseTargetBlock:

	push	bc
	ld	a,c
	add	a,5
	ld	c,a
	ld	de,boardChar
	call	displayChar
	ld	a,(boardColor)
	call	setAttribute
	pop	bc
	ret

; ------------------------------------------------------------------------------
; Sets attribute from register A to coordinates in BC.
;
; No registers are affected.
; ------------------------------------------------------------------------------

setAttribute:

	push	hl
	call	calcAttrAddress
	ld	(hl),a		; Save to calculated video attributes memory
	pop	hl
	
	ret

; ------------------------------------------------------------------------------
; Calculates attribute address based on coordinates in BC, where B = row (0-23) 
; and C = column (0-31). Upper left corner is 0,0 while lower right is 23,31.
; Routine takes into account coordinates rollover if values are out of range.
;
; The address is returned in HL. No other registers are affected.
; ------------------------------------------------------------------------------

calcAttrAddress:

	push	bc
	push	af

	ld	hl,22528	; First byte in attributes memory,
				; binary value is HL = 01011000 00000000

	; Clean up registers B and C for easier calculation and to prevent
	; memory leak into the printer buffer right after attributes area.	
	; For register B additionally calculate rollover back to 0-23 range
	; if B > 23 (can not be over 31 because of already masked lower 5 bits).
	; No need for this for register C since range is already 0-31:
	
	ld	a,c
	and	%00011111
	ld	c,a

	ld	a,b
	and	%00011111
	ld	b,a

	cp	24		; Check limit of 24 for B
	jp	m,_skipRollover	; If under 24 skip B rollover
	sub	24		; If over 23 then rollover B
	ld	b,a		; Refresh B with the rollover value

_skipRollover:

	; Set part of row offset (lower 3 bits in B) and
	; full column offset (lower 5 bits in C) to L:

	ld	a,l		; Prepare accumulator
	or	b		; Load full row offset in B
	sla	a		; Adjust and make space for column offset in C
	sla	a
	sla	a
	sla	a
	sla	a
	or	c		; Load full column offset to L
	ld	l,a		; Store back to L

	; Set the remaining part of row offset (bits 3 and 4 in B)
	; to H register (at this point H = 01011000, replacing lower 2 bits).
	; Final result in HL is formed as 010110BB BBBCCCCC:

	ld	a,b		; Load full row offset in B
	and	%00011000	; Preserve only bits 3 and 4
	srl	a		; Adjust and make space for address base in H
	srl	a
	srl	a
	or	h		; Load H to prepared accumulator
	ld	h,a		; Store back to H

	pop	af
	pop	bc
	
	ret

; ------------------------------------------------------------------------------
; Fills video memory with zero pixels and resets all colors to black.
; ------------------------------------------------------------------------------

clearScreen:

	ld	hl,16384	; Video RAM start
	ld	bc,6912		; Size of pixel+attributes area (6144+768)

_clearScreenLoop:

	ld	(hl),0		; Erase video RAM location
	inc	hl
	dec	bc
	ld	a,b
	or	c
	jr	nz,_clearScreenLoop

	ret

; ------------------------------------------------------------------------------
; Draws game board.
; ------------------------------------------------------------------------------

initializeScreen:

	; Set border color:

	ld	a,(borderColor)	; Game board frame color
	call	8859		; BORDER

	; Fill screen with frame characters:

	ld	b,23

_frameRowsLoop:

	ld	c,31

_frameColsLoop:

	ld	de,frameChar
	call	displayChar
	ld	a,(frameColor)
	call	setAttribute

	dec	c
	jp	p,_frameColsLoop

	dec	b
	jp	p,_frameRowsLoop

	; Display board area:

	ld	b,0

_boardRowsLoop:

	ld	c,5

_boardColsLoop:

	ld	de,boardChar
	call	displayChar
	ld	a,(boardColor)
	call	setAttribute

	inc	c
	ld	a,c
	cp	27
	jr	nz,_boardColsLoop

	inc	b
	ld	a,b
	cp	22
	jr	nz,_boardRowsLoop

	ret

; ------------------------------------------------------------------------------
; Displays character at the BC position (B = row 0..23, C = column 0..31) as
; defined as 8 bytes (8x8 matrix) at the address passed in DE.
; Registers are preserved.
; ------------------------------------------------------------------------------

displayChar:

	push	bc
	push	de
	push	hl

	; Calculate position of the first byte in video memory
	; corresponding to the position in BC. This part determines
	; position in the first 8 rows only:

	ld	hl,16384	; Video RAM start

	; Calculate row increment as B*256: 

	push	bc
	ld	c,b
	sla	c
	sla	c
	sla	c
	sla	c
	sla	c
	ld	b,0
	add	hl,bc
	pop	bc

	; Calculate column increment as C*1:

	push	bc
	ld	b,0
	ld	a,c
	and	%00011111	; Mask to limit values to 0..31
	ld	c,a
	add	hl,bc
	pop	bc

	; Offsets address in HL by 0, 2048 or 4096 based on B 
	; to position character in the right 1/3 of the screen:

	bit	4,b		; Test for the third section (10xxx)
	jr	z,_testSecond	; If not there, check for the second third
	ld	bc,4096		; If in the third section calculate offset
	add	hl,bc
	jr	_generateChar

_testSecond:

	bit	3,b		; Test for the second section (01xxx) 
	jr	z,_generateChar	; if not there, then no offset is required
	ld	bc,2048		; If in the second section calculate offset
	add	hl,bc

	; Display character:

_generateChar:

	ld	b,8

_charLoop:

	ld	a,(de)
	ld	(hl),a
	inc	de
	push	de
	ld	de,256
	add	hl,de
	pop	de
	dec	b
	jp	nz,_charLoop

	; Exit:

	pop	hl
	pop	de
	pop	bc

	ret

; ------------------------------------------------------------------------------
; Generates random position in BC register in 1..20 interval in both B and C.
; Use for generating player and target random coordinates. Since the board is
; 22x22 (0..21) generating 1..20 range assures no plaxyer or target will be
; placed next to the board edge to avoid surprise beams hitting the player 
; at the moment of catching the target.  
; ------------------------------------------------------------------------------

generateRandomPosition:

_randRow:

	call	generateRandom
	and	%00011111		; Limit to 0..31
	cp	20			; Check if it is over 19
	jp	p,_randRow		; If => 20 try again
	ld	b,a			; If OK store to B (row)
	inc	b			; Increment B to offset 0..19 to 1..20

_randCol:

	call	generateRandom
	and	%00011111		; Limit to 0..31
	cp	20			; Check if it is over 19
	jp	p,_randCol		; If => 20 try again
	ld	c,a			; If OK store to C (column)
	inc	c			; Increment C to offset 0..19 to 1..20

	ret

; ------------------------------------------------------------------------------
; The random number generator given here originally
; appeared as a post by Patrik Rak on WoSF.
; Random number is returned in A. No other registers are affected.
; ------------------------------------------------------------------------------

generateRandom:

	push	de
	push	hl

_rnd:   ld	hl,0xA280   ; xz -> yw
	ld	de,0xC0DE   ; yw -> zt

        ld	(_rnd+1),de ; x = y, z = w
        ld 	a,e         ; w = w ^ ( w << 3 )
        add	a,a
        add	a,a
        add	a,a
        xor	e
        ld	e,a
        ld	a,h         ; t = x ^ (x << 1)
        add	a,a
        xor	h
        ld	d,a
        rra                 ; t = t ^ (t >> 1) ^ w
        xor	d
        xor	e
        ld	h,l         ; y = z
        ld	l,a         ; w = t
        ld	(_rnd+4),hl

	pop	hl
	pop	de

        ret

; ------------------------------------------------------------------------------
; Game parameters and data area:
; ------------------------------------------------------------------------------

; Constants:

playerColor:	db	%01000010
targetColor:	db	%11000111
beamColor:	db	%01000110
boardColor:	db	%00000000
frameColor:	db	%00001001
borderColor:	db	%00000001

playerChar:	db	%00011000
		db	%00111100
		db	%01111110
		db	%11111111
		db	%11111111
		db	%01111110
		db	%00111100
		db	%00011000

targetChar:	db	%11111111
		db	%10000001
		db	%10111101
		db	%10100101
		db	%10100101
		db	%10111101
		db	%10000001
		db	%11111111

vBeamChar:	db	%00111100
		db	%00000000
		db	%00111100
		db	%00000000
		db	%00111100
		db	%00000000
		db	%00111100
		db	%00000000

hBeamChar:	db	%00000000
		db	%00000000
		db	%10101010 
		db	%10101010
		db	%10101010
		db	%10101010
		db	%00000000
		db	%00000000

boardChar:	db	%00000000
		db	%00000000
		db	%00000000
		db	%00011000
		db	%00011000
		db	%00000000
		db	%00000000
		db	%00000000

frameChar:	db	%10001000
		db	%00000000
		db	%00100010
		db	%00000000
		db	%10001000
		db	%00000000
		db	%00100010
		db	%00000000


; Preset read-only parameters:

levelDuration:	db	10	; Number of catches before moving to the next level
gameSpeed:	db	6	; GAME SPEED: Delay in 1/50s (interrupt time)
beamGapSize: 	db	4	; GAME DIFFICULTY: successive rows for generating beams 
				; (value 1 generates new beam one after another)
; Game variables, read-write:

beamGapCounter:	db	0	; Current counter for beam spacing, initialized as beamGapSize  
maxBeamLength:	db	0	; Beam length, initialized as 2 (game level is maxBeamLength-1)
levelRemaining:	db	0	; Remaining catches for the current level, initialized as levelDuration 
playerPosition:	dw	0
playerLastPos:	dw	0
targetPosition:	dw	0
targetLastPos:	dw	0
lastKeys:	db	$0f	; No keys pressed condition

; Space for storing beam head positions and lengths on a 22x22 board. $ff indicates no active
; beam on this row. Directions order in vertical beams array: down, up, down, up...
; Order in horizontal beams array: left, right, left, right... 

vHeadsPos:		db	$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
hHeadsPos:		db	$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
vBeamsLen:		db	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
hBeamsLen:		db	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
