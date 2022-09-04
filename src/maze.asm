SECTION "Maze", ROM0

GenerateMaze::
	; Clear the playfield first
	ld hl, Playfield
:	xor a
	ld [hl+], a
	ld a, h
	cp HIGH(PlayfieldEnd)
	jr nz, :-

	; Create a big space to put walls on
	ld a, 1
	ld b, 48
	ld c, 48
	ld d, 8
	ld e, 8
	call RectFill

	ld a, 1
	ld b, 10
	ld c, 10
	ld d, 2
	ld e, 2
	call RectFill

	; -----------------------------------------------------
	; Add walls to the ground tiles that were placed
	; -----------------------------------------------------

	ld hl, Playfield+64+1
AddWalls:
    ; Don't try to place walls in the void
	ld a, [hl]
	or a
	jr z, .skip

	; Skip this wall?
	call RandomByte
	and 3
	cp 0
	call nz, AddWallHere
.skip:
    ; Move down two rows
	ld a, l
	add %10000000
	ld l, a
	jr nc, :+
		inc h
		; Did it move past the end?
		ld a, h
		cp HIGH(PlayfieldEnd)
		jr nc, NextColumn
	:
    ; Keep going on the same column
	jr AddWalls
NextColumn:
    ; Go back 64 rows and move over 2 to the right
	ld de, -(64*64)+2
	add hl, de
	ld a, l
    ; Stop at 61 tiles to the right
	cp 61+64
	jr nz, AddWalls

	; -----------------------------------------------------
	; Count the floors
	; -----------------------------------------------------

	ld hl, Playfield
	ld bc, 0 ; Floor counter
:	ld a, [hl+]
	dec a ; Test for 1
	jr nz, :+
		inc bc ; Add a floor
	:
	ld a, h
	cp HIGH(PlayfieldEnd)
	jr nz, :--

	ld b,b

	ret


AddWallHere:
	; Place a wall there
	ld [hl], 2

	; Choose a direction to go from here
	call RandomByte
	and 3
	jr nz, .notLeft
	; A = 0 : Left
	dec l
	ld [hl], 2
	inc l
	ret
.notLeft:
	dec a
	jr nz, .notUp
	; A = 1 : Up
	push hl
	ld de, -64
	add hl, de
	ld [hl], 2
	pop hl
	ret
.notUp:
	dec a
	jr nz, .notDown
	; A = 2 : Down
	push hl
	ld de, 64
	add hl, de
	ld [hl], 2
	pop hl
	ret
.notDown:
	; A = 3 : Right
	inc l
	ld [hl], 2
	dec l
	ret


; Sets HL to a pointer at a point on the map where D=X and E=Y
MapPointerDE_XY:
           ;     E           A
	xor a  ; ..yy yyyy | .... ....
	srl e
	rra    ; ...y yyyy | y... ....
	srl e
	rra    ; .... yyyy | yy.. ....
	or d
	ld l, a

	ld a, e
	or high(Playfield)
	ld h, a
	ret


; Inputs: A(Type), B(Width), C(Height), D(X), E(Y)
RectFill:
	ldh [temp1], a
	call MapPointerDE_XY
.another_row:
	push hl
	push bc
	ldh a, [temp1]
.row:
	ld [hl+], a
	dec b
	jr nz, .row
	pop bc
	pop hl

	; Move to the next row
	ld a, l
	add %01000000
	ld l, a
	jr nc, :+
		inc h
	:
	dec c
	jr nz, .another_row
	ret


; Adapted from http://wiki.nesdev.com/w/index.php/Random_number_generator/Linear_feedback_shift_register_(advanced)
RandomByte:
	push bc
	; rotate the middle bytes left
	ldh a, [seed+0]
	ld c, a

	ldh a, [seed+1]
	ldh [seed+2], a
	; compute seed+1 ($C5>>1 = %1100010)
	ldh a, [seed+3] ; original high byte
	srl a
	ld b, a ; reverse: 100011
	srl a
	srl a
	srl a
	srl a
	xor b
	srl a
	xor b
	xor c ; combine with original low byte
	ldh [seed+1], a
	; compute seed+0 ($C5 = %11000101)

	ldh a, [seed+2] ; will move to seed+3 at the end
	ld c, a         ; save it for then

	ldh a, [seed+3] ; original high byte
	ld b, a
	add a
	xor b
	add a
	add a
	add a
	add a
	xor b
	add a
	add a
	xor b
	ldh [seed+0], a

	; finish rotating byte 2 into 3
	ld a, c
	ldh [seed+3], a
	pop bc

	ldh a, [seed+0]
	ret
