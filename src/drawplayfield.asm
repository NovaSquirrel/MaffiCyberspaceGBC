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

; This file has routines for displaying a level and redrawing the map as you walk around it.
; It can handle a maximum of 64 different block types, where each block uses a single attribute byte.

include "include/macros.inc"
include "include/hardware.inc/hardware.inc"

SECTION "DrawPlayfield", ROM0

; .-----------------------------------------------------------------------------
; | UpdateRow and UpdateColumn
; '-----------------------------------------------------------------------------

; Update a row of tiles (for scrolling)
UpdateRow::
	ld b, a ; Which row of tiles to update within the level

	; Calculate the VRAM address of the first tile to update

	call GetTilemapRowAddress ; HL = VRAM address

	; Start slightly to the left of the camera, unless doing so would go outside the level
	ldh a, [CameraX+1]
	add a
	cp 2
	jr c, :+
		sub 2 ; Go left two blocks
	:
	ld c, a   ; Will be used as a level data X coordinate
	and 31
	add l
	ld l, a

	; -------------------------------------------
	; Calculate map data pointer
	push hl
	ld d, c
	srl d
	ld e, b
	srl e
	call MapPointerDE_XY
	ld d, h
	ld e, l
	pop hl

	; Pick which tiles to draw based on whether the row is even or odd
	srl b
	jp c, ScrollUpdateBottom
	jp ScrollUpdateTop

; Update a column of tiles (for scrolling)
UpdateColumn::
	ld b, a ; Which column of tiles to update within the level

	; Calculate the VRAM address of the first tile to update

	; First, get the row address, going slightly above the camera, unless doing so would go outside the level
	ldh a, [CameraY+1]
	add a
	cp 2
	jr c, :+
		sub 2 ; Go up two blocks
	:
	ld c, a   ; Will be used as a level data Y coordinate
	call GetTilemapRowAddress ; HL = VRAM address

	; Next, offset to the correct column
	ld a, b
	and 31
	add l
	ld l, a

	; -------------------------------------------
	; Calculate map data pointer

	push hl
	ld d, b
	srl d
	ld e, c
	srl e
	call MapPointerDE_XY
	ld d, h
	ld e, l
	pop hl

	; Pick which tiles to draw based on whether the column is even or odd
	srl b
	jp c, ScrollUpdateRight
	jp ScrollUpdateLeft

; Get address of a row of tiles on the tilemap, starting from a tile row number across the whole level
GetTilemapRowAddress:
	and 31
	ld l, a
	ld h, 0
	; Multiply by 5
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl

	ld de, _SCRN0
	add hl, de
	ret

; Render a whole screen worth of level tiles
RenderLevelScreen::
	ldh a, [CameraY+0]
	add a
	ldh a, [CameraY+1]
	rla
	ld c, a  ; Starting row number
	ld b, 19 ; Rows to display
.loop:
	push bc
	ld a, c
	call UpdateRow
	pop bc
	inc c
	dec b
	jr nz, .loop
	ret

; .----------------------------------------------------------------------------
; | Update tile numbers for columns
; '----------------------------------------------------------------------------

; Input: DE (Level pointer)
; Output: B,C (Two block IDs), DE (Level pointer, + 2 rows)
ScrollUpdateLeftRightRead:
	; Get first block
	ld a, [de]
	add a ; *4, four bytes per block
	add a
	ld b, a

	call GoDownOneLevelRow
	
	; Get second block
	ld a, [de]
	add a ; *4, four bytes per block
	add a
	ld c, a
	fallthrough GoDownOneLevelRow

; Input: DE (Level pointer)
; Output: DE (Level pointer + 1 row)
GoDownOneLevelRow:
	; Move down one row in level data
	ld a, 64
	add a,e
	ld e,a
	ret nc
	inc d
	ret

; Write the top left and bottom left, or top right and bottom right
ScrollUpdateLeftRightWriteOne:
	wait_vram
	ld a, [de]   ; 2
	ld [hl], a   ; 2
	set 5, l     ; 2 Move down a row
	set 1, e     ; 2
	ld a, [de]   ; 2
	ld [hl], a   ; 2
	ret

ScrollUpdateLeftRightWrite:
	push de
	ld d, HIGH(BlockAppearance)

	ld e, b
	call ScrollUpdateLeftRightWriteOne

	ld e, c

	ld bc, 32    ; Now BC is free so it can be used for adding to HL
	add hl, bc
	res 2, h     ; Wrap vertically

	call ScrollUpdateLeftRightWriteOne

	add hl, bc
	res 2, h     ; Wrap vertically

	pop de
	ret

ScrollUpdateLeft::
	ld b, 6 ; 6 units of 2 blocks each
	push hl
	push de
.loop:
	push bc
	call ScrollUpdateLeftRightRead
	call ScrollUpdateLeftRightWrite
	pop bc
	dec b
	jr nz, .loop
	jr ScrollUpdateRight.ColorUpdateIfColor

ScrollUpdateRight::
	ld b, 6 ; 6 units of 2 blocks each
	push hl
	push de
.loop:
	push bc
	call ScrollUpdateLeftRightRead
	inc b ; Use the top right and bottom right
	inc c ; Use the top right and bottom right
	call ScrollUpdateLeftRightWrite
	pop bc
	dec b
	jr nz, .loop
.ColorUpdateIfColor:
	pop de
	pop hl
	ldh a, [IsNotGameBoyColor]
	or a
	jp z, ColorUpdateColumn
	ret

; .----------------------------------------------------------------------------
; | Update tile numbers for columns
; '----------------------------------------------------------------------------

; Input: DE (Level pointer)
; Output: B,C (Two block IDs), DE (Level pointer, + 2 columns)
ScrollUpdateTopBottomRead:
	; Get first block
	ld a, [de]
	inc e
	add a ; *4, four bytes per block
	add a
	ld b, a

	; Get second block
	ld a, [de]
	inc e
	add a ; *4, four bytes per block
	add a
	ld c, a
	ret

; Write the top two tile numbers of a block to VRAM
ScrollUpdateTopWrite:
	wait_vram
	ld a, [de]   ; 2
	ld [hl+], a  ; 2
	inc e        ; 1
	ld a, [de]   ; 2
	ld [hl+], a  ; 2
	res 5, l     ; 2 Wrap horizontally
	ret

ScrollUpdateTop::
	ld b, 6 ; 6 units of 2 blocks each
	push hl
	push de
.loop:
	push bc

	; Preload block IDs into B and C, multiplied by 4 to index into the block data table
	call ScrollUpdateTopBottomRead

	; Write ---------------------------
	push de
	ld d, HIGH(BlockAppearance)
	ld e, b
	call ScrollUpdateTopWrite
	ld e, c
	call ScrollUpdateTopWrite

	pop de
	pop bc

	dec b
	jr nz, .loop
	pop de
	pop hl
	ldh a, [IsNotGameBoyColor]
	or a
	jp z, ColorUpdateTop
	ret

; Write the bottom two tile numbers of a block to VRAM
ScrollUpdateBottomWrite:
	wait_vram
	ld a, [de]   ; 2
	ld [hl+], a  ; 2
	inc e        ; 1
	ld a, [de]   ; 2
	ld [hl], a   ; 2
	res 5, l     ; 2 Wrap horizontally
	inc l        ; 1
	set 5, l     ; 2
	ret

ScrollUpdateBottom::
	ld b, 6 ; 6 units of 2 blocks each
	push hl
	push de
.loop:
	push bc

	; Preload block IDs into B and C, multiplied by 4 to index into the block data table
	call ScrollUpdateTopBottomRead
	set 1, b ; Start at the bottom row
	set 1, c ; Start at the bottom row

	; Write ---------------------------
	push de
	ld d, HIGH(BlockAppearance)
	ld e, b
	call ScrollUpdateBottomWrite
	ld e, c
	call ScrollUpdateBottomWrite

	pop de
	pop bc

	dec b
	jr nz, .loop
	pop de
	pop hl
	ldh a, [IsNotGameBoyColor]
	or a
	jp z, ColorUpdateBottom
	ret

SECTION "DrawPlayfieldColor", ROM0

; .----------------------------------------------------------------------------
; | Set colors on columns
; '----------------------------------------------------------------------------

; Update the colors on a column of 12 blocks
; Input: HL (VRAM pointer), DE (level pointer)
ColorUpdateColumn:
	ld a, 1
	ldh [rVBK], a

	ld b, 6 ; 6 units of 2 blocks each
.loop:
	; --- Read ---
	push bc
	push hl
	; Get first block's color
	ld a, [de]
	ld hl, BlockAppearanceColor
	add_hl_a
	ld b, [hl] ; B = block color 1

	call GoDownOneLevelRow
	
	; Get second block's color
	ld a, [de]
	ld hl, BlockAppearanceColor
	add_hl_a
	ld c, [hl] ; C = block color 2

	call GoDownOneLevelRow
	pop hl

	; --- Write ---
	push de
	ld de, 32    ; Now DE is free so it can be used for adding to HL

	wait_vram    ; Should take exactly 16 cycles - which is the amount I should get?
	ld [hl], b   ; 2
	set 5, l     ; 2 Move down a row
	ld [hl], b   ; 2

	add hl, de   ; 2
	res 2, h     ; 2 Wrap vertically

	ld [hl], c   ; 2
	set 5, l     ; 2 Move down a row
	ld [hl], c   ; 2

	add hl, de
	res 2, h     ; Wrap vertically

	pop de
	pop bc
	dec b
	jr nz, .loop

	xor a
	ldh [rVBK], a
	ret

; .----------------------------------------------------------------------------
; | Set colors on rows
; '----------------------------------------------------------------------------

MACRO ColorUpdateRowReadOne
	ld a, [de]
	inc e
	ld hl, BlockAppearanceColor
	add_hl_a
ENDM

MACRO ColorUpdateRowRead
	push hl
	; Get first block
	ColorUpdateRowReadOne
	ld b, [hl]

	; Get second block
	ColorUpdateRowReadOne
	ld c, [hl]

	; Get third block
	ColorUpdateRowReadOne
	ld a, [hl]
	ldh [temp1], a

	; Get fourth block
	ColorUpdateRowReadOne
	ld a, [hl]
	ldh [temp2], a
	pop hl
ENDM

; Update the colors on the top row of 12 blocks
; Input: HL (VRAM pointer), DE (level pointer)
ColorUpdateTop::
	ld a, 1
	ldh [rVBK], a

	ld b, 3 ; 3 units of 4 blocks each
.loop:
	push bc
	ColorUpdateRowRead
	; Write ---------------------------
	wait_vram
	ld a, b      ; 1
	ld [hl+], a  ; 2
	ld [hl+], a  ; 2
	res 5, l     ; 2 Wrap horizontally
	ld a, c      ; 1
	ld [hl+], a  ; 2
	ld [hl+], a  ; 2
	res 5, l     ; 2 Wrap horizontally

	; This code will only be run on GBC, and therefore in double speed mode, so I have extra safe time
	ldh a, [temp1]
	ld [hl+], a  ; 2
	ld [hl+], a  ; 2
	res 5, l     ; 2 Wrap horizontally
	ldh a, [temp2]
	ld [hl+], a  ; 2
	ld [hl+], a  ; 2
	res 5, l     ; 2 Wrap horizontally
	; ---------------------------------
	pop bc
	dec b
	jr nz, .loop

	xor a
	ldh [rVBK], a
	ret

; Update the colors on the bottom row of 12 blocks
; Input: HL (VRAM pointer), DE (level pointer)
ColorUpdateBottom::
	ld a, 1
	ldh [rVBK], a

	ld b, 3 ; 3 units of 4 blocks each
.loop:
	push bc
	ColorUpdateRowRead
	; Write ---------------------------
	wait_vram
	ld a, b      ; 1
	ld [hl+], a  ; 2
	ld [hl], a   ; 2

	res 5, l     ; 2 Wrap horizontally
	inc l        ; 1
	set 5, l     ; 2

	ld a, c      ; 1
	ld [hl+], a  ; 2
	ld [hl], a   ; 2

	res 5, l     ; 2 Wrap horizontally
	inc l        ; 1
	set 5, l     ; 2

	wait_vram
	ldh a, [temp1]
	ld [hl+], a  ; 2
	ld [hl], a   ; 2

	res 5, l     ; 2 Wrap horizontally
	inc l        ; 1
	set 5, l     ; 2

	ldh a, [temp2]
	ld [hl+], a  ; 2
	ld [hl], a   ; 2

	res 5, l     ; 2 Wrap horizontally
	inc l        ; 1
	set 5, l     ; 2
	; ---------------------------------
	pop bc
	dec b
	jr nz, .loop

	xor a
	ldh [rVBK], a
	ret
