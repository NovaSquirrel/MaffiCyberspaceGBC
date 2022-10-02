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

include "include/macros.inc"
include "include/hardware.inc/hardware.inc"

SECTION "DrawPlayfieldColor", ROM0

ColorUpdateLeftRight:
	ld b, 6
	ld a, 1
	ldh [rVBK], a
.loop:
	; --- Read ---
	push bc
	push hl
	; Get first block
	ld a, [de]
	ld hl, BlockAppearanceColor
	rst_add_hl_a
	ld b, [hl]

	call GoDownOneLevelRow
	
	; Get first block
	ld a, [de]
	ld hl, BlockAppearanceColor
	rst_add_hl_a
	ld c, [hl]

	call GoDownOneLevelRow
	pop hl

	; --- Write ---
	push de
	ld de, 32    ; Now BC is free so it can be used for adding to HL

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

; -----------------------------------------------------------------------------
ColorUpdateTopBottomReadOne:
	ld a, [de]
	inc e
	ld hl, BlockAppearanceColor
	jp $0008

ColorUpdateTopBottomRead:
	push hl
	; Get first block
	call ColorUpdateTopBottomReadOne
	ld b, [hl]

	; Get second block
	call ColorUpdateTopBottomReadOne
	ld c, [hl]

	; Get third block
	call ColorUpdateTopBottomReadOne
	ld a, [hl]
	ldh [temp1], a

	; Get fourth block
	call ColorUpdateTopBottomReadOne
	ld a, [hl]
	ldh [temp2], a
	pop hl
	ret

ColorUpdateTop::
	ld b, 3
	ld a, 1
	ldh [rVBK], a
.loop:
	push bc
	call ColorUpdateTopBottomRead
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

ColorUpdateBottom::
	ld b, 3
	ld a, 1
	ldh [rVBK], a
.loop:
	push bc
	call ColorUpdateTopBottomRead
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
	ld [hl], a  ; 2

	res 5, l     ; 2 Wrap horizontally
	inc l        ; 1
	set 5, l     ; 2

	ldh a, [temp2]
	ld [hl+], a  ; 2
	ld [hl], a  ; 2

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

SECTION "DrawPlayfield", ROM0

; -----------------------------------------------------------------------------

ScrollUpdateLeftRightRead:
	; Get first block
	ld a, [de]
	add a
	add a
	ld b, a

	call GoDownOneLevelRow
	
	; Get second block
	ld a, [de]
	add a
	add a
	ld c, a

	; Fall through
GoDownOneLevelRow:
	; Move down one row in level data
	ld a, 64
	add a,e
	ld e,a
	jr nc, :+
		inc d
	:
	ret

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
	ld b, 6
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
	ld b, 6
	push hl
	push de
.loop:
	push bc
	call ScrollUpdateLeftRightRead
	inc b
	inc c
	call ScrollUpdateLeftRightWrite
	pop bc
	dec b
	jr nz, .loop
.ColorUpdateIfColor:
	pop de
	pop hl
	ldh a, [IsGameBoyColor]
	cp $11
	jp z, ColorUpdateLeftRight
	ret

; -----------------------------------------------------------------------------

ScrollUpdateTopBottomRead:
	; Get first block
	ld a, [de]
	inc e
	add a
	add a
	ld b, a

	; Get second block
	ld a, [de]
	inc e
	add a
	add a
	ld c, a
	ret

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
	ld b, 6
	push hl
	push de
.loop:
	push bc

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
	ldh a, [IsGameBoyColor]
	cp $11
	jp z, ColorUpdateTop
	ret

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
	ld b, 6
	push hl
	push de
.loop:
	push bc

	call ScrollUpdateTopBottomRead
	set 1, b
	set 1, c

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
	ldh a, [IsGameBoyColor]
	cp $11
	jp z, ColorUpdateBottom
	ret

SECTION "Camera", ROM0

; -----------------------------------------------------------------------------

; For comparison against the new values
def OldCameraX equs "temp1"
def OldCameraY equs "temp2"

; For telling the direction - it's the target-initial high byte
def CameraDXH equs "temp5"
def CameraDYH equs "temp6"

GetCameraTargetX:
	ldh a, [PlayerPXL]
	ld e, a
	ldh a, [PlayerPXH]
	sub 10/2
	jr nc, :+
		xor a
		ld e, a
	:
	cp 64-10
	jr c, :+
		xor a
		ld e, a
		ld a, 64-10
	:
	ld d, a
	ret

GetCameraTargetY:
	ldh a, [PlayerPYL]
	ld e, a
	ldh a, [PlayerPYH]
	sub 9/2
	jr nc, :+
		xor a
		ld e, a
	:
	cp 64-9
	jr c, :+
		xor a
		ld e, a
		ld a, 64-9
	:
	ld d, a
	ret

InitCamera::
	call GetCameraTargetX
	ld a, e
	ldh [CameraX+0], a
	ld a, d
	ldh [CameraX+1], a

	call GetCameraTargetY
	ld a, e
	ldh [CameraY+0], a
	ld a, d
	ldh [CameraY+1], a

	jp CameraConvertXY

AdjustCamera::
	; Get scroll target
	call GetCameraTargetX

	; Find difference
	ldh a, [CameraX+0]
	ldh [OldCameraX], a
	ld l, a
	ldh a, [CameraX+1]
	ld h, a
	; ---
	ld a, e
	sub l
	ld l, a
	ld a, d
	sbc h
	ld h, a
	ldh [CameraDXH], a

	; Divide by 4
	call DivideDifferenceForLerp

	; Move the camera toward target
	ldh a, [CameraX+0]
	add l
	ldh [CameraX+0], a
	ldh a, [CameraX+1]
	adc h
	ldh [CameraX+1], a

; ---------------------------------------

	; Get scroll target
	call GetCameraTargetY

	; Find difference
	ldh a, [CameraY+0]
	ldh [OldCameraY], a
	ld l, a
	ldh a, [CameraY+1]
	ld h, a
	; ---
	ld a, e
	sub l
	ld l, a
	ld a, d
	sbc h
	ld h, a
	ldh [CameraDYH], a

	call DivideDifferenceForLerp

	; Move the camera toward target
	ldh a, [CameraY+0]
	add l
	ldh [CameraY+0], a
	ldh a, [CameraY+1]
	adc h
	ldh [CameraY+1], a

; ---------------------------------------
; Do scroll updates on the side as necessary

	; Is a column update required?
	ld hl, CameraX
	ldh a, [OldCameraX]
	xor [hl]
	and $80
	jr z, .NoUpdateColumn
		ldh a, [CameraDXH]
		add a ; Check the sign

		; Get the number of tiles for the current camera position
		push af
		ld a, [hl] ; CameraX
		add a      ; Get top bit
		ldh a, [CameraX+1]
		rla        ; Add in the top bit from CameraX+0 to get the tile count
		ld h, a
		pop af
		ld a, h

		jr c, .UpdateLeft
	.UpdateRight:
		add 20
		;call UpdateColumn
		ld [DoUpdateColumn], a
		jr .NoUpdateColumn
	.UpdateLeft:
		dec a
		;call UpdateColumn
		ld [DoUpdateColumn], a
	.NoUpdateColumn:

	ld hl, CameraY
	ldh a, [OldCameraY]
	xor [hl]
	and $80
	jr z, .NoUpdateRow
		ldh a, [CameraDYH]
		add a ; Check the sign

		; Get the number of tiles for the current camera position
		push af
		ld a, [hl] ; CameraY
		add a      ; Get top bit
		ldh a, [CameraY+1]
		rla        ; Add in the top bit from CameraY+0 to get the tile count
		ld h, a
		pop af
		ld a, h

		jr c, .UpdateUp
	.UpdateDown:
		add 18
		;call UpdateRow
		ld [DoUpdateRow], a
		jr .NoUpdateRow
	.UpdateUp:
		;call UpdateRow
		ld [DoUpdateRow], a
	.NoUpdateRow:

; ---------------------------------------

CameraConvertXY:
	call CameraConvertY
	; Fall through
CameraConvertX:
	; Convert camera to pixel coordinates
	ldh a, [CameraX+1]
	ld b, a
	ldh a, [CameraX+0]
	rept 4
		srl b
		rra
	endr
	adc 0
	ld [CameraXPixel+0], a
	ld a, b
	ld [CameraXPixel+1], a
	ret

CameraConvertY:
	; Convert camera to pixel coordinates
	ldh a, [CameraY+1]
	ld b, a
	ldh a, [CameraY+0]
	rept 4
		srl b
		rra
	endr
	adc 0
	ld [CameraYPixel+0], a
	ld a, b
	ld [CameraYPixel+1], a
	ret

; Update a row of tiles (for scrolling)
UpdateRow::
	ld b, a ; Tile to update, vertically

	; Get a VRAM address to update first

	call GetRowAddress

	ldh a, [CameraX+1]
	add a
	cp 2
	jr c, :+
		sub 2      ; Go left one metatile
	:
	ld c, a
	and 31
	add_hl_a

	; -------------------------------------------
	; Calculate map data pointer
	push hl
	ld a, c
	and %1111110
	rra     ; Carry guaranteed clear
	ld d, a
	ld e, b
	srl e
	call MapPointerDE_XY
	ld d, h
	ld e, l
	pop hl

	; Pick which tiles to draw
	srl b
	jp c, ScrollUpdateBottom
	jp ScrollUpdateTop

; Update a column of tiles (for scrolling)
UpdateColumn::
	ld b, a ; Tile to update, horizontally

	; Get a VRAM address to update first

	ldh a, [CameraY+1]
	add a
	cp 2
	jr c, :+
		sub 2      ; Go up one metatile
	:
	ld c, a
	call GetRowAddress

	ld a, b
	and 31
	add_hl_a

	; -------------------------------------------
	; Calculate map data pointer

	push hl
	ld d, b
	srl d
	ld a, c
	and %1111110
	rra     ; Carry guaranteed clear
	ld e, a
	call MapPointerDE_XY
	ld d, h
	ld e, l
	pop hl

	; Pick which tiles to draw
	srl b
	jp c, ScrollUpdateRight
	jp ScrollUpdateLeft

GetRowAddress:
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

DivideDifferenceForLerp:
	ld a, l
	rept 3
	sra h
	rra
	endr
	ld l, a
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
