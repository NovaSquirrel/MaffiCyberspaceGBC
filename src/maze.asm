; Maffi cyberspace game
; Copyright (C) 2022-2024 NovaSquirrel
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

include "include/macros.inc"
include "include/leveldata.inc"
include "res/block_enum.inc"

SECTION "Maze Generation", ROM0

def FLOOD_VISITED = 128

def FloodFillReadIndex  equs "temp1"
def FloodFillWriteIndex equs "temp2"
def RectFillValue       equs "temp3"

def Arg1                equs "temp1"
def Arg2                equs "temp2"
def Arg3                equs "temp4"

def CurrentTileValue    equs "temp8"

; .--------------------------------------------------------
; | Runs through all of the commands in a level
; | HL = Level pointer
; '--------------------------------------------------------
LoadLevel::
	; Default settings
	ld a, LEVEL_AREA_1
	ldh [CurrentTileValue], a

	push hl

	ld a, $C3 ; JP opcode
	ld c, PlaceholderPointers_End-PlaceholderPointers_AddFloors
	ld hl, PlaceholderPointers_AddFloors
	rst MemsetSmall

	ld a, LOW(FloorSomeStars)
	ld [PlaceholderPointers_AddFloors+4*0+1], a
	ld [PlaceholderPointers_AddFloors+4*1+1], a
	ld a, HIGH(FloorSomeStars)
	ld [PlaceholderPointers_AddFloors+4*0+2], a
	ld [PlaceholderPointers_AddFloors+4*1+2], a

	ld a, LOW(WallChance75Percent)
	ld [PlaceholderPointers_AddWalls+4*0+1], a
	ld a, HIGH(WallChance75Percent)
	ld [PlaceholderPointers_AddWalls+4*0+2], a
	ld a, LOW(WallChance25Percent)
	ld [PlaceholderPointers_AddWalls+4*1+1], a
	ld a, HIGH(WallChance25Percent)
	ld [PlaceholderPointers_AddWalls+4*1+2], a

	pop hl

; Process each level command
LoadLevelLoop:
	ld a, [hl+]
	ld d, h ; Save HL temporarily
	ld e, l

	ld hl, LoadLevelCommands
	add_hl_a
	ld a, [hl+]
	ld h, [hl]
	ld l, a
	push hl

	ld h, d ; Restore HL
	ld l, e
	ret     ; Jump to the handler

; .--------------------------------------------------------
; | Level load command handlers
; '--------------------------------------------------------
LoadLevelCommands:
	dw LevelCommand_End
	dw LevelCommand_Type
	dw LevelCommand_Rect
	dw LevelCommand_RectCompact
	dw LevelCommand_Single
	dw LevelCommand_AddWalls
	dw LevelCommand_AddFloors
	dw LevelCommand_PutAnywhere
	dw LevelCommand_PutWithinRect

; ---------------------------------------------------------

LevelCommand_End:
	ret ; All done!

; ---------------------------------------------------------

LevelCommand_Type:
	ld a, [hl+]
	ldh [CurrentTileValue], a
	jp LoadLevelLoop

; ---------------------------------------------------------

LevelCommand_Rect:
	ld a, [hl+] ; x
	ld d, a
	ld a, [hl+] ; y
	ld e, a
	ld a, [hl+] ; w
	ld b, a
	ld a, [hl+] ; h
	ld c, a
	push hl
	ldh a, [CurrentTileValue]
	call RectFill
	pop hl
	jp LoadLevelLoop

; ---------------------------------------------------------

LevelCommand_RectCompact:
	ld a, [hl] ; XY
	and $f0    ; xxxx0000
	rrca       ; 0xxxx000
	rrca       ; 00xxxx00
	ld d, a ; x

	ld a, [hl+] ; XY
	and $0f     ; 0000yyyy
	add a       ; 000yyyy0
	add a       ; 00yyyy00
	ld e, a ; y

	ld a, [hl]  ; WH
	and $f0     ; xxxx0000
	rrca        ; 0xxxx000
	rrca        ; 00xxxx00
	ld b, a ; w

	ld a, [hl+] ; WH
	and $0f     ; 0000yyyy
	add a       ; 000yyyy0
	add a       ; 00yyyy00
	ld c, a ; h

	push hl
	ldh a, [CurrentTileValue]
	call RectFill
	pop hl
	jp LoadLevelLoop

; ---------------------------------------------------------

LevelCommand_Single:
	ld a, [hl+] ; x
	ld d, a
	ld a, [hl+] ; x
	ld e, a
	push hl
	call MapPointerDE_XY
	ldh a, [CurrentTileValue]
	ld [hl], a 
	pop hl
	jp LoadLevelLoop

; ---------------------------------------------------------

LevelCommand_AddWalls:
	push hl
	; -----------------------------------------------------
	; Add walls to the ground tiles that were placed
	; -----------------------------------------------------
	ld de, Playfield+64+1
AddWalls:
    ; Don't try to place walls in the void, or anything else that's not a placeholder area
	ld a, [de]
	sub LEVEL_AREA_1
	jr c, ReturnFromAddWalls
		; Jump to the wall placement function for that specific level area
		add a ; Multiply by 4 because it's slightly faster than multiplying by 3
		add a
		add PlaceholderPointers_AddWalls-PlaceholderPointers_AddFloors
		assert LOW(PlaceholderPointers_AddFloors) == 0 
		ld h, HIGH(PlaceholderPointers_AddWalls)
		ld l, a
		jp hl
	ReturnFromAddWalls:
    ; Move down two rows
	ld a, e
	add %10000000
	ld e, a
	jr nc, :+
		inc d
		bit 4, d ; Will be 0 at PlayfieldEnd
		jr z, NextColumn
	:
    ; Keep going on the same column
	jr AddWalls
NextColumn:
    ; Go back 64 rows and move over 2 to the right
	ld hl, -(64*64)+2
	add hl, de
	ld d, h
	ld e, l
	ld a, l
    ; Stop at 61 tiles to the right
	cp 61+64
	jr nz, AddWalls

	; -----------------------------------------------------
	; Flood fill time
	; -----------------------------------------------------

	; TODO: use the actual starting location!!
	ld d, 10
	ld e, 10
	call MapPointerDE_XY
	ld [hl], BlockType_Floor
	call FloodFillPlayfield

	; -----------------------------------------------------
	; Fix unsolveable mazes
	; -----------------------------------------------------

	; ---------------------------------
	; Try it forward

	ld hl, Playfield
FixMazeForward:
	rept 4
	ld a, [hl+]
	add a ; Check visited bit, and check for void
	jr z, .skip
	jr c, .skip

	rrca ; Undo shifting left
	ld d, HIGH(BlockFlags)
	ld e, a
	ld a, [de]
	bit 6, a ; Is this block visitable, but it wasn't visited?
	jr z, :+
		push hl
		dec l
		call FixUnvisitedFloor
		pop hl
	:
	endr
.skip
	bit 4, h ; Will be 0 at PlayfieldEnd
	jr nz, FixMazeForward

	; ---------------------------------
	; Try it backward too

	ld hl, PlayfieldEnd-1
FixMazeBackward:
	rept 4
	ld a, [hl-]
	add a ; Check visited bit, and check for void
	jr z, .skip
	jr c, .skip

	rrca ; Undo shifting left
	ld d, HIGH(BlockFlags)
	ld e, a
	ld a, [de]
	bit 6, a ; Is this block visitable, but it wasn't visited?
	jr z, :+
		push hl
		dec l
		call FixUnvisitedFloor
		pop hl
	:
	endr
.skip:
	bit 4, h ; Will be 0 at Playfield-1
	jr nz, FixMazeBackward
	pop hl
	jp LoadLevelLoop

; ---------------------------------------------------------

LevelCommand_AddFloors:
	; -----------------------------------------------------
	; Has the maze been fixed well enough?
	; Also apply autotiling, and add items to the floor
	; -----------------------------------------------------
	push hl

	ld hl, Playfield
	ld bc, 0 ; Floor counter
	;ld de, 0 ; Visited counter
CountVisitedUnvisited:
	ld a, [hl+]
	or a
	jr nz, :+ ; If it's void, just skip it
		; Check if we're past the end
		bit 4, h ; Will be 1 at PlayfieldEnd
		jr nz, CountVisitedUnvisited
		jr ReturnFromAddFloors.done
	:
	dec hl ; undo the hl+

	; Count tiles that haven't been visited
	cp BlockType_Floor
	jr z, .NotVisited
	sub LEVEL_AREA_1
	cp 8
	jr nc, :+
	.NotVisited:
		inc bc
	:

	res 7, [hl] ; Clear "visited" flag

	; Autotile the walls
	ld a, [hl]
	sub LEVEL_AREA_1 ; Fill in the placeholders
	jr c, .NotPlaceholder
		add a ; Multiply by 4 because it's slightly faster than multiplying by 3
		add a
		assert LOW(PlaceholderPointers_AddFloors) == 0 
		ld d, HIGH(PlaceholderPointers_AddFloors)
		ld e, a
		push de
		ret
	.NotPlaceholder:
	cp BlockType_Wall-LEVEL_AREA_1
	jr nz, .NotWall
		push bc
		push hl
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
		ld a, BlockType_Wall
		add c
		ld [hl], a
		pop bc
		jr .next
	.NotWall:
	;add a,a
	;jr nc, :+
	;	inc de ; Add a visited floor
	;:
.next:
ReturnFromAddFloors:
	inc hl
	ld a, h
	bit 5, h ; Will be 1 at PlayfieldEnd
	jr z, CountVisitedUnvisited
.done:

	pop hl
	jp LoadLevelLoop

; -----------------------------------------------------------------------------

LevelCommand_PutAnywhere::
	ld a, [hl+]
	ldh [Arg1], a ; Count
	ld a, [hl+]
	ldh [CurrentTileValue], a ; Type
	push hl

	ld d, 255     ; Eventually fail
.Try:
	call RandomByteLCG

	and $0f
	or HIGH(Playfield)
	ld h, a
	call RandomByte
	and %00111100
	ld l, a

	ld a, [hl]
	cp LEVEL_AREA_1|FLOOD_VISITED
	jr nc, .Good

	; See if toggling some of the bits can help!
	ld a, h
	xor %00001000
	ld h, a
	ld a, [hl]
	cp LEVEL_AREA_1|FLOOD_VISITED
	jr nc, .Good

	ld a, l
	xor %00100000
	ld l, a
	ld a, [hl]
	cp LEVEL_AREA_1|FLOOD_VISITED
	jr nc, .Good

	ld a, h
	xor %00000100
	ld h, a
	ld a, [hl]
	cp LEVEL_AREA_1|FLOOD_VISITED
	jr nc, .Good

	ld a, l
	xor %00010000
	ld l, a
	ld a, [hl]
	cp LEVEL_AREA_1|FLOOD_VISITED
	jr nc, .Good

	ld a, h
	xor %00000010
	ld h, a
	ld a, [hl]
	cp LEVEL_AREA_1|FLOOD_VISITED
	jr nc, .Good

	ld a, l
	xor %00001000
	ld l, a
	ld a, [hl]
	cp LEVEL_AREA_1|FLOOD_VISITED
	jr nc, .Good

	dec d ; Use up one try
	jr nz, .Try
	jr .Fail
.Good:
	ldh a, [CurrentTileValue]
	ld [hl], a

	ldh a, [Arg1] ; Remove 1 from the count
	dec a
	ldh [Arg1], a
	jr nz, .Try   ; More left to make?

.Fail:
	pop hl
	jp LoadLevelLoop	

; -----------------------------------------------------------------------------

LevelCommand_PutWithinRect::
	jp LoadLevelLoop

; -----------------------------------------------------------------------------

; Helper for the wall autotiling
IsWallAutotile:
	ld a, [hl]
	cp BlockType_Wall
	jr c, :+
	cp BlockType_Wall_LRDU+1
	rl c
	ret ; NC if >=
        ; C if  <
:	or a ; Clear carry
	rl c
	ret

; -----------------------------------------------------------------------------
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
	ld [hl], BlockType_Floor | 128
	jp FloodFillPlayfield
.fix_right:
	dec l
	ld [hl], BlockType_Floor | 128
	jp FloodFillPlayfield
.fix_down:
	ld de, -64
	add hl, de
	ld [hl], BlockType_Floor | 128
	jp FloodFillPlayfield
.fix_up:
	ld de, 64
	add hl, de
	ld [hl], BlockType_Floor | 128
	jp FloodFillPlayfield


; ---------------------------------------------------------
FloorAllNormal:
	ld [hl], BlockType_Floor
	jp ReturnFromAddFloors
FloorSomeStars:
	ld [hl], BlockType_Floor
	push hl
	call RandomByteLCG
	pop hl
	cp 15
	jp nc, ReturnFromAddFloors
	ld [hl], BlockType_Star
	jp ReturnFromAddFloors

; ---------------------------------------------------------
WallChance25Percent:
	call RandomByte
	cp 192
	call nc, AddWallHere
	jp ReturnFromAddWalls
WallChance50Percent:
	call RandomByte
	cp 128
	call nc, AddWallHere
	jp ReturnFromAddWalls
WallChance75Percent:
	call RandomByte
	cp 64
	call nc, AddWallHere
	jp ReturnFromAddWalls
WallChance93Percent:
	call RandomByte
	cp 16
	call nc, AddWallHere
	jp ReturnFromAddWalls

; Add a wall at [HL] and put a block in a random direction
AddWallHere:
	; Place a wall at [HL]
	ld h, d
	ld l, e
	ld [hl], BlockType_Wall

	; Choose a direction to go from here
	call RandomByte
	and 3
	jr nz, .notLeft
	; A = 0 : Left
	dec l
	ld [hl], BlockType_Wall
	inc l
	ret
.notLeft:
	dec a
	jr nz, .notUp
	; A = 1 : Up
	ld bc, -64
	add hl, bc
	ld [hl], BlockType_Wall
	ret
.notUp:
	dec a
	jr nz, .notDown
	; A = 2 : Down
	ld bc, 64
	add hl, bc
	ld [hl], BlockType_Wall
	ret
.notDown:
	; A = 3 : Right
	inc l
	ld [hl], BlockType_Wall
	dec l
	ret


; ---------------------------------------------------------
; HL = Playfield position
; DE = Queue pointer
FloodFillAddToQueue:
	set 7, [hl] ; Mark as visited

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
	ld b,b ; Breakpoint, because this is a problem!!
	ret


; ---------------------------------------------------------
; HL = Playfield position
FloodFillPlayfield:
	xor a
	ldh [FloodFillReadIndex], a  ; Starts at zero - read the first byte
	inc a
	ldh [FloodFillWriteIndex], a ; Will be 1 after the first byte is written

	ld de, FloodQueueHi          ; D will point at FloodQueueHi and FloodQueueLo
	call FloodFillAddToQueue     ; Start off the buffer with the initial position (passed in with HL)
	ld e, 0                      ; Go into the loop reading the first byte

.loop:
	; Read pointer from the queue
	ld a, [de] ; High half
	ld h, a
	inc d
	ld a, [de] ; Low half
	ld l, a
	dec d

	; Switch to the write index
	push de
	ldh a, [FloodFillWriteIndex]
	ld e, a

	; Left
	dec l
	ld a, [hl]
	bit 7, a     ; Already visited?
	jr nz, :+
		ld b, HIGH(BlockFlags)
		ld c, a
		ld a, [bc]
		bit 6, a ; Is this block visitable?
		call nz, FloodFillAddToQueue
	:
	inc l

	; Right
	inc l
	ld a, [hl]
	bit 7, a     ; Already visited?
	jr nz, :+
		ld b, HIGH(BlockFlags)
		ld c, a
		ld a, [bc]
		bit 6, a ; Is this block visitable?
		call nz, FloodFillAddToQueue
	:
	dec l

	; Up
	push hl
	push hl
	ld bc, -64
	add hl, bc
	ld a, [hl]
	bit 7, a     ; Already visited?
	jr nz, :+
		ld b, HIGH(BlockFlags)
		ld c, a
		ld a, [bc]
		bit 6, a ; Is this block visitable?
		call nz, FloodFillAddToQueue
	:
	pop hl

	; Down
	ld bc, 64
	add hl, bc
	ld a, [hl]
	bit 7, a     ; Already visited?
	jr nz, :+
		ld b, HIGH(BlockFlags)
		ld c, a
		ld a, [bc]
		bit 6, a ; Is this block visitable?
		call nz, FloodFillAddToQueue
	:
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


; ---------------------------------------------------------
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
