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
