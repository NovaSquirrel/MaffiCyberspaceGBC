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

; Temporary state
def Arg1                equs "temp1"
def Arg2                equs "temp2"
def Arg3                equs "temp3"

def RectX               equs "temp1"
def RectY               equs "temp2"
def RandomXMask         equs "temp3"
def RandomXLimit        equs "temp4"
def RandomYMask         equs "temp5"
def RandomYLimit        equs "temp6"

def RectFillValue       equs "temp1"

; State held across commands
def CurrentTileValue    equs "temp12"

; .--------------------------------------------------------
; | Runs through all of the commands in a level
; | HL = Level pointer
; '--------------------------------------------------------
LoadLevel::
	; Default settings
	ld a, LEVEL_AREA_1
	ldh [CurrentTileValue], a
	xor a
	ld [RescueCritterCount], a

	push hl

	ld a, $C3 ; JP opcode
	ld c, PlaceholderPointers_End-PlaceholderPointers
	ld hl, PlaceholderPointers
	rst MemsetSmall

	pop hl

; Process each level command
LoadLevelLoop:
	ld a, [hl+]
	ld d, h ; Save HL temporarily
	ld e, l

	ld hl, LoadLevelCommands
	rst AddHL_A
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
	call DE_NybblesTimesFour
	call BC_NybblesTimesFour
	push hl
	ldh a, [CurrentTileValue]
	call RectFill
	pop hl
	jp LoadLevelLoop

DE_NybblesTimesFour:
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
	ret

BC_NybblesTimesFour:
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
	ret

DE_Nybbles:
	ld a, [hl]
	and $f0
	swap a
	ld d, a
	ld a, [hl+]
	and $0f
	ld e, a
	ret

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
	; Fill in the jump table for handling the placeholder tiles
	ld de, PlaceholderPointers+1
:	ld a, [hl+]
	add a
	push af
	ld bc, PlaceholderPointerTable_Walls
	add_bc_a
	ld a, [bc] ; Get low byte
	ld [de], a
	inc bc
	inc e
	ld a, [bc] ; Get high byte
	ld [de], a
	inc e
	inc e
	inc e
	pop af
	jr nc, :-

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
		assert LOW(PlaceholderPointers) == 0 
		ld h, HIGH(PlaceholderPointers)
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
	ld d, 32
	ld e, 32
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
	; Fill in the jump table for handling the placeholder tiles
	ld de, PlaceholderPointers+1
:	ld a, [hl+]
	add a
	push af
	ld bc, PlaceholderPointerTable_Floors
	add_bc_a
	ld a, [bc] ; Get low byte
	ld [de], a
	inc bc
	inc e
	ld a, [bc] ; Get high byte
	ld [de], a
	inc e
	inc e
	inc e
	pop af
	jr nc, :-

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
		assert LOW(PlaceholderPointers) == 0 
		ld d, HIGH(PlaceholderPointers)
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

	ld d, 255     ; Number of tries remaining
.Try:
	call RandomByteLCG ; Trashes HL
	and $0f
	or HIGH(Playfield)
	ld h, a
	call RandomByte
	and %00111100
	ld l, a

	; Is this random position acceptable?
	ld a, [hl]
	cp LEVEL_AREA_1|FLOOD_VISITED
	jr nc, .Good

	; See if toggling some of the bits can help!
	; Unlike LevelCommand_PutWithinRect, I make drastic changes to the position by toggling the high bits, moving down to lower ones
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
	call .PlaceTileAtRandomLocation
	jr nz, .Try   ; More left to make?
	pop hl
	jp LoadLevelLoop

; Common routine to place a tile after having picked a location for it, so that certain types can be treated specially
.PlaceTileAtRandomLocation:
	ldh a, [CurrentTileValue]
	ld [hl], a

	; Exits and critters to be rescued need to have their positions recorded
	cp BlockType_Exit|128
	jr nz, :+
		call MapPointerHL_To_XY_DE
		ld a, d
		ld [MazeExitX], a
		ld a, e
		ld [MazeExitY], a
		jr .ExitPlaceTileAtRandomLocation
:	cp BlockType_RescueCritter|128
	jr nz, :+
		call MapPointerHL_To_XY_DE
		ld a, [RescueCritterCount]
		inc a
		ld [RescueCritterCount], a
		dec a
		add a
		ld hl, CritterXYList
		rst AddHL_A
		ld [hl], d
		inc hl
		ld [hl], e
	:

	.ExitPlaceTileAtRandomLocation:
	ldh a, [Arg1] ; Remove 1 from the count for how many to make
	dec a
	ldh [Arg1], a
	ret

; -----------------------------------------------
.Fail:
	; Panic and just scan the level for a place to put things, I guess!
	; It's better than an unsolvable level
	ld b,b ; Breakpoint because this is not good

	ldh a, [Arg1]
	ld b, a ; Count
	ld hl, Playfield
.FailLoop:
	ld a, [hl+]
	cp LEVEL_AREA_1|FLOOD_VISITED
	jr c, :+
		dec hl
		ldh a, [CurrentTileValue]
		ld [hl+], a
		dec b
		jr z, .FailDone
	:
	bit 4, h ; Will be 0 at PlayfieldEnd
	jr nz, .FailLoop
.FailDone:
	pop hl
	jp LoadLevelLoop	

; -----------------------------------------------------------------------------

LevelCommand_PutWithinRect::
	call DE_NybblesTimesFour
	ld a, d
	ldh [RectX], a ; X
	ld a, e
	ldh [RectY], a ; Y

	call DE_Nybbles

	; Get X mask and limit
	ld bc, .RandomMask
	call .BCPlusD
	ldh [RandomXMask], a
	ld bc, .RandomLimit
	call .BCPlusD
	ldh [RandomXLimit], a

	; Get Y mask and limit
	ld bc, .RandomMask
	call .BCPlusE
	ldh [RandomYMask], a
	ld bc, .RandomLimit
	call .BCPlusE
	ldh [RandomYLimit], a

	ld a, [hl+]
	ldh [Arg1], a ; Count
	ld a, [hl+]
	ldh [CurrentTileValue], a ; Type
	xor a
	ldh [Arg3], a ; Try count (counts upwards)

	; ---------------------------------
	push hl
.Try:
	; Get an X coordinate
	ldh a, [RandomXMask]
	ld d, a
	ldh a, [RandomXLimit]
	ld e, a
:	call RandomByteLCG
	and d
	cp e
	jr nc, :-
	ldh [Arg2], a ; Save for a bit

	; Get a Y coordinate
	ldh a, [RandomYMask]
	ld d, a
	ldh a, [RandomYLimit]
	ld e, a
:	call RandomByteLCG
	and d
	cp e
	jr nc, :-

	; Add the random offsets to the base
	ld e, a
	ldh a, [RectX]
	add e
	ld e, a

	ldh a, [Arg2]
	ld d, a
	ldh a, [RectY]
	add d
	ld d, a

	call MapPointerDE_XY

	; Is this random position acceptable?
	ld a, [hl]
	cp LEVEL_AREA_1|FLOOD_VISITED
	jr nc, .Good

	; Check if toggling some of the bits in the pointer nudges it over to a valid tile
	ld a, l
	xor %00000001
	ld l, a
	ld a, [hl]
	cp LEVEL_AREA_1|FLOOD_VISITED
	jr nc, .Good

	ld a, l
	xor %01000000
	ld l, a
	ld a, [hl]
	cp LEVEL_AREA_1|FLOOD_VISITED
	jr nc, .Good

	ld a, l
	xor %00000010
	ld l, a
	ld a, [hl]
	cp LEVEL_AREA_1|FLOOD_VISITED
	jr nc, .Good

	ld a, l
	xor %10000000
	ld l, a
	ld a, [hl]
	cp LEVEL_AREA_1|FLOOD_VISITED
	jr nc, .Good

	; Add one try, fail if we wrap around to zero
	ldh a, [Arg3]
	inc a
	ldh [Arg3], a
	jr nz, .Try
	jp LevelCommand_PutAnywhere.Fail
.Good:
	; Place a tile in the chosen spot!!
	call LevelCommand_PutAnywhere.PlaceTileAtRandomLocation
	jr nz, .Try   ; More left to make?

	pop hl
	jp LoadLevelLoop

.BCPlusD:
	ld a, d
	jr .BCPlusA
.BCPlusE:
	ld a, e
.BCPlusA:
	add_bc_a
	ld a, [bc]
	ret

.RandomMask:  ; Number to AND the random byte with
	db  3, 7, 15, 15, 31, 31, 31, 31, 63, 63, 63, 63, 63, 63, 63, 63
.RandomLimit: ; Number to reject a random number for being greater or equal to
	db  4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60, 64

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
	; Randomly choose between four direction priorities
	call RandomByte
	and 3
	jr z, .alternate_order
	dec a
	jr z, .alternate_order2
	dec a
	jr z, .alternate_order3

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

.alternate_order2:
	; Right
	ld a, l
	inc l
	inc l
	bit 7, [hl]
	jr nz, .fix_right

	; Left
	ld l, a
	dec l
	dec l
	bit 7, [hl]
	jr nz, .fix_left

	; Up
	ld de, 2-64-64
	add hl, de
	bit 7, [hl]
	jr nz, .fix_down

	; Down
	ld de, 64+64+64+64
	add hl, de
	bit 7, [hl]
	jr nz, .fix_up
	ret

.alternate_order3:
	; Down
	ld de, 64+64
	add hl, de
	bit 7, [hl]
	jr nz, .fix_down

	; Up
	ld de, -64-64-64-64
	add hl, de
	bit 7, [hl]
	jr nz, .fix_up

	; Left
	ld de, 64+64-2
	add hl, de
	bit 7, [hl]
	jr nz, .fix_right

	; Right
	inc l
	inc l
	inc l
	inc l
	bit 7, [hl]
	jr nz, .fix_left
	ret

; -------------------------------------

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
PlaceholderPointerTable_Floors:
	dw FloorAllNormal
	dw FloorRareStars
	dw FloorSomeStars

FloorAllNormal:
	ld [hl], BlockType_Floor
	jp ReturnFromAddFloors
FloorRareStars:
	ld [hl], BlockType_Floor
	push hl
	call RandomByteLCG
	pop hl
	cp 5
	jp nc, ReturnFromAddFloors
	ld [hl], BlockType_Star
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
PlaceholderPointerTable_Walls:
	dw WallChance0Pecent
	dw WallChance25Percent
	dw WallChance50Percent
	dw WallChance75Percent
	dw WallChance87Percent
	dw WallChance93Percent
	dw WallGridPattern

WallChance0Pecent:
	jp ReturnFromAddWalls
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
WallChance87Percent:
	call RandomByte
	cp 32
	call nc, AddWallHere
	jp ReturnFromAddWalls
WallChance93Percent:
	call RandomByte
	cp 16
	call nc, AddWallHere
	jp ReturnFromAddWalls
WallGridPattern:
	ld a, BlockType_Wall
	ld [de], a
	jp ReturnFromAddWalls

; Add a wall at [DE] and put a block in a random direction
AddWallHere:
	; Place a wall at [DE]
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
;FloodFillAddToQueue:
;	set 7, [hl] ; Mark as visited
;
;	; Write to the queue
;	ld a, h
;	ld [de], a ; FloodQueueHi
;	inc d
;	ld a, l
;	ld [de], a ; FloodQueueLo
;	dec d
;
;	; Next entry
;	inc e
;
;	; Are we out of space?
;	ldh a, [FloodFillReadIndex]
;	cp e
;	ret nz
;	ld b,b ; Breakpoint, because this is a problem!!
;	ret

; Macro version, because this is a very frequently called routine!
MACRO FloodFillAddToQueueMacro
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
	; No error checking here; hopefully it isn't needed!
ENDM

; ---------------------------------------------------------
; HL = Playfield position
FloodFillPlayfield:
	xor a
	ldh [FloodFillReadIndex], a  ; Starts at zero - read the first byte
	inc a
	ldh [FloodFillWriteIndex], a ; Will be 1 after the first byte is written

	ld de, FloodQueueHi          ; D will point at FloodQueueHi and FloodQueueLo
	FloodFillAddToQueueMacro     ; Start off the buffer with the initial position (passed in with HL)
	ld e, 0                      ; Go into the loop reading the first byte

.loop:
	; Read maze pointer from the queue
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
		jr z, :+
		FloodFillAddToQueueMacro
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
		jr z, :+
		FloodFillAddToQueueMacro
	:
	dec l

	; Up
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
		jr z, :+
		FloodFillAddToQueueMacro
	:

	; Down
	ld bc, 64+64
	add hl, bc
	ld a, [hl]
	bit 7, a     ; Already visited?
	jr nz, :+
		ld b, HIGH(BlockFlags)
		ld c, a
		ld a, [bc]
		bit 6, a ; Is this block visitable?
		jr z, :+
		FloodFillAddToQueueMacro
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

	bit 0, b
	jr z, RectFillDivisibleBy2 ; If width is even, do a little bit of loop unrolling
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

RectFillDivisibleBy2:
	srl b
	bit 0, b
	jr z, RectFillDivisibleBy4 ; If width is a multiple of 4, do a little bit more loop unrolling
.another_row:
	push hl
	push bc
	ldh a, [RectFillValue]
.row:
	ld [hl+], a
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

RectFillDivisibleBy4:
	srl b
.another_row:
	push hl
	push bc
	ldh a, [RectFillValue]
.row:
	ld [hl+], a
	ld [hl+], a
	ld [hl+], a
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
