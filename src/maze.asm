; Maffi cyberspace game
; Copyright (C) 2022 NovaSquirrel
;
; This program is free software: you can redistribute it and/or
; modify it under the terms of the GNU General Public License as
; published by the Free Software Foundation; either version 3 of the
; License, or (at your option) any later version.
;
; This program is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;

include "macros.inc"

SECTION "Maze", ROM0

	enum_start
	enum_elem T_EMPTY
	enum_elem T_FLOOR
	enum_elem T_WALL
	enum_elem T_WALL____U
	enum_elem T_WALL___D_
	enum_elem T_WALL___DU
	enum_elem T_WALL__R__
	enum_elem T_WALL__R_U
	enum_elem T_WALL__RD_
	enum_elem T_WALL__RDU
	enum_elem T_WALL_L___
	enum_elem T_WALL_L__U
	enum_elem T_WALL_l_D_
	enum_elem T_WALL_L_DU
	enum_elem T_WALL_LR__
	enum_elem T_WALL_LR_U
	enum_elem T_WALL_LRD_
	enum_elem T_WALL_LRDU

FLOOD_VISITED = 128

def FloodFillReadIndex  equs "temp1"
def FloodFillWriteIndex equs "temp2"
def RectFillValue       equs "temp3"

GenerateMaze::
	; Clear the playfield first
	ld hl, Playfield
:	xor a
	rept 8
	ld [hl+], a
	endr
	ld a, h
	cp HIGH(PlayfieldEnd)
	jr nz, :-

	; Create a big space to put walls on
	ld a, T_FLOOR
	ld b, 56
	ld c, 56
	ld d, 4
	ld e, 4
	call RectFill

	; -----------------------------------------------------
	; Add walls to the ground tiles that were placed
	; -----------------------------------------------------

	ld hl, Playfield+64+1
    ; B is free for another parameter
	ld c, 64 ; Wall chance
AddWalls:
    ; Don't try to place walls in the void
	ld a, [hl]
	or a
	jr z, .skip

	; Skip this wall?
	call RandomByte
	cp c
	call nc, AddWallHere
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
	; Flood fill time
	; -----------------------------------------------------

	ld d, 10
	ld e, 10
	call MapPointerDE_XY
	ld [hl], T_FLOOR
	call FloodFillPlayfield

	; -----------------------------------------------------
	; Fix unsolveable mazes
	; -----------------------------------------------------

	; Try it forward

	ld hl, Playfield
FixMazeForward:
	rept 4
	ld a, [hl+]
	dec a ; Test for 1 (which is T_FLOOR)
	jr nz, :+
		push hl
		call FixUnvisitedFloor
		pop hl
	:
	endr
	ld a, h
	cp HIGH(PlayfieldEnd)
	jr nz, FixMazeForward

	; Try it backward too

	ld hl, PlayfieldEnd-1
FixMazeBackward:
	rept 4
	ld a, [hl-]
	dec a ; Test for 1 (which is T_FLOOR)
	jr nz, :+
		push hl
		call FixUnvisitedFloor
		pop hl
	:
	endr
	ld a, h
	cp HIGH(Playfield-1)
	jr nz, FixMazeBackward

	; -----------------------------------------------------
	; Has the maze been fixed well enough?
	; -----------------------------------------------------

	ld hl, Playfield
	ld bc, 0 ; Floor counter
	;ld de, 0 ; Visited counter
CountVisitedUnvisited:
	ld a, [hl+]
	cp T_WALL

	jr nz, :+
		push bc
		push hl
		dec l ; Undo the hl+

		ld c, 0 ; Walls
		dec l
		call IsWallAutotile ; Left
		inc l
		inc l
		call IsWallAutotile ; Right
		ld de, -1+64
		add hl, de
		call IsWallAutotile ; Down
		ld de, -64-64
		add hl, de
		call IsWallAutotile ; Up

		pop hl
		; Store the modified wall
		ld a, T_WALL
		add c
		dec l
		ld [hl+], a
		pop bc
		jr .skip
	:
	dec a ; Test for 1
	jr nz, :+
		inc bc ; Add a non-visited floor
	:
	;add a,a
	;jr nc, :+
	;	inc de ; Add a visited floor
	;:
.skip:
	ld a, h
	cp HIGH(PlayfieldEnd)
	jr nz, CountVisitedUnvisited

	ret

; -----------------------------------------------------------------------------

; Helper for the wall autotiling
IsWallAutotile:
	ld a, [hl]
	cp T_WALL
	jr c, :+
	cp T_WALL_LRDU+1
	rl c
	ret ; NC if >=
        ; C if  <
:	or a ; Clear carry
	rl c
	ret

; Try to fix a floor tile that wasn't reached by connecting it to a visited tile 2 tiles away
FixUnvisitedFloor:
	; Use a different order sometimes to mix things up
	bit 1, l
	jr z, .alternate_order

	; Left
	ld a, l
	dec l
	dec l
	bit 7, [hl]
	jr nz, .fix_left

	; Right
	ld l, a
	inc l
	inc l
	bit 7, [hl]
	jr nz, .fix_right

	; Down
	ld de, -2+64+64
	add hl, de
	bit 7, [hl]
	jr nz, .fix_down

	; Up
	ld de, -64-64-64-64
	add hl, de
	bit 7, [hl]
	jr nz, .fix_up
	ret

.alternate_order:
	; Up
	ld de, -64-64
	add hl, de
	bit 7, [hl]
	jr nz, .fix_up

	; Down
	ld de, 64+64+64+64
	add hl, de
	bit 7, [hl]
	jr nz, .fix_down

	; Right
	ld de, -64-64+2
	add hl, de
	bit 7, [hl]
	jr nz, .fix_right

	; Left
	dec l
	dec l
	dec l
	dec l
	bit 7, [hl]
	jr nz, .fix_left
	ret

.fix_left:
	inc l
	ld [hl], T_FLOOR
	jp FloodFillPlayfield
.fix_right:
	dec l
	ld [hl], T_FLOOR
	jp FloodFillPlayfield
.fix_down:
	ld de, -64
	add hl, de
	ld [hl], T_FLOOR
	jp FloodFillPlayfield
.fix_up:
	ld de, 64
	add hl, de
	ld [hl], T_FLOOR
	jp FloodFillPlayfield

; Add a wall at [HL] and put a block in a random direction
AddWallHere:
	; Place a wall at [HL]
	ld [hl], T_WALL

	; Choose a direction to go from here
	call RandomByte
	and 3
	jr nz, .notLeft
	; A = 0 : Left
	dec l
	ld [hl], T_WALL
	inc l
	ret
.notLeft:
	dec a
	jr nz, .notUp
	; A = 1 : Up
	push hl
	ld de, -64
	add hl, de
	ld [hl], T_WALL
	pop hl
	ret
.notUp:
	dec a
	jr nz, .notDown
	; A = 2 : Down
	push hl
	ld de, 64
	add hl, de
	ld [hl], T_WALL
	pop hl
	ret
.notDown:
	; A = 3 : Right
	inc l
	ld [hl], T_WALL
	dec l
	ret


; HL = Playfield position
; DE = Queue pointer
FloodFillAddToQueue:
	set 7, [hl]

	; Write to the queue
	ld a, h
	ld [de], a ; FloodQueueHi
	inc d
	ld a, l
	ld [de], a ; FloodQueueLo
	dec d

	; Next entry
	inc e

	; Are we out of space?
	ldh a, [FloodFillReadIndex]
	cp e
	ret nz
	ld b,b
	ret


; HL = Playfield position
FloodFillPlayfield:
	xor a
	ldh [FloodFillReadIndex], a  ; Starts at zero - read the first byte
	inc a
	ldh [FloodFillWriteIndex], a ; Will be 1 after the first byte is written

	ld de, FloodQueueHi          ; D will point at FloodQueueHi and FloodQueueLo
	call FloodFillAddToQueue     ; Start off the buffer with the initial position
	ld e, 0                      ; Go into the loop reading the first byte

.loop:
	; Read pointer from the queue
	ld a, [de]
	ld h, a
	inc d
	ld a, [de]
	ld l, a
	dec d

	; Switch to the write index
	push de
	ldh a, [FloodFillWriteIndex]
	ld e, a

	; Left
	dec l
	ld a, [hl]
	cp T_FLOOR
	call z, FloodFillAddToQueue
	inc l

	; Right
	inc l
	ld a, [hl]
	cp T_FLOOR
	call z, FloodFillAddToQueue
	dec l

	; Up
	push hl
	push hl
	ld bc, -64
	add hl, bc
	ld a, [hl]
	cp T_FLOOR
	call z, FloodFillAddToQueue
	pop hl

	; Down
	ld bc, 64
	add hl, bc
	ld a, [hl]
	cp T_FLOOR
	call z, FloodFillAddToQueue
	pop hl

	; Write the new write index
	ld a, e
	ldh [FloodFillWriteIndex],a
	ld b, a ; Hold onto the write index
	pop de

	; Read the next index - is it the same as the write one?
	inc e
	ld a, e
	ldh [FloodFillReadIndex], a ; So that running out of space can be detected
	cp b
	jr nz, .loop

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
	ldh [RectFillValue], a
	call MapPointerDE_XY
.another_row:
	push hl
	push bc
	ldh a, [RectFillValue]
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
