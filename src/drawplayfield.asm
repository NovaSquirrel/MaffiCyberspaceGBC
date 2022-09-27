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
include "include/hardware.inc/hardware.inc"

SECTION "DrawPlayfield", ROM0

; -----------------------------------------------------------------------------

ScrollUpdateLeft::
	ld b, 5
	ld b,b
.loop:
	push bc

	; Get first block
	ld a, [de]
	add a
	add a
	ld b, a

	; Move down one row in level data
	ld a, 64
	add a,e
	ld e,a
	jr nc, :+
		inc d
	:

	; Get second block
	ld a, [de]
	add a
	add a
	ld c, a

	; Move down one row in level data
	ld a, 64
	add a,e
	ld e,a
	jr nc, :+
		inc d
	:

	; Write ---------------------------
	push de
	ld d, HIGH(BlockAppearance)

	ld e, b
	wait_vram
	ld a, [de]   ; 2
	ld [hl], a   ; 2
    set 5, l     ; 2 Move down a row
	set 1, e     ; 2
	ld a, [de]   ; 2
	ld [hl], a   ; 2
	res 2, h     ; 2 Wrap vertically

	ld e, c

    ld bc, 32    ; Now BC is free
    add hl, bc

	wait_vram
	ld a, [de]   ; 2
	ld [hl], a   ; 2
    set 5, l     ; 2 Move down a row
	set 1, e     ; 2
	ld a, [de]   ; 2
	ld [hl], a   ; 2
	res 2, h     ; 2 wrap vertically

    add hl, bc

	pop de
	pop bc

	dec b
	jr nz, .loop
	ret

ScrollUpdateRight::
	ld b, 5
.loop:
	push bc

	; Get first block
	ld a, [de]
	add a
	add a
	inc a ; Right side
	ld b, a

	; Move down one row in level data
	ld a, 64
	add a,e
	ld e,a
	jr nc, :+
		inc d
	:

	; Get second block
	ld a, [de]
	add a
	add a
	inc a ; Right side
	ld c, a

	; Move down one row in level data
	ld a, 64
	add a,e
	ld e,a
	jr nc, :+
		inc d
	:

	; Write ---------------------------
	push de
	ld d, HIGH(BlockAppearance)

	ld e, b
	wait_vram
	ld a, [de]   ; 2
	ld [hl], a   ; 2
    set 5, l     ; 2 Move down a row
	set 1, e     ; 2
	ld a, [de]   ; 2
	ld [hl], a   ; 2
	res 2, h     ; 2 Wrap vertically

	ld e, c

    ld bc, 32    ; Now BC is free
    add hl, bc

	wait_vram
	ld a, [de]   ; 2
	ld [hl], a   ; 2
    set 5, l     ; 2 Move down a row
	set 1, e     ; 2
	ld a, [de]   ; 2
	ld [hl], a   ; 2
	res 2, h     ; 2 wrap vertically

    add hl, bc

	pop de
	pop bc

	dec b
	jr nz, .loop
	ret

; -----------------------------------------------------------------------------

ScrollUpdateTop::
	ld b, 6
.loop:
	push bc

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

	; Write ---------------------------
	push de
	ld d, HIGH(BlockAppearance)

	ld e, b
	wait_vram
	ld a, [de]   ; 2
	ld [hl+], a  ; 2
	inc e        ; 1
	ld a, [de]   ; 2
	ld [hl+], a  ; 2
	res 5, l     ; 2 Wrap horizontally

	ld e, c
	wait_vram
	ld a, [de]   ; 2
	ld [hl+], a  ; 2
	inc e        ; 1
	ld a, [de]   ; 2
	ld [hl+], a  ; 2
	res 5, l     ; 2 Wrap horizontally

	pop de
	pop bc

	dec b
	jr nz, .loop
	ret

ScrollUpdateBottom::
	ld b, 6
.loop:
	push bc

	; Get first block
	ld a, [de]
	inc e
	add a
	add a
	add 2 ; Bottom row
	ld b, a

	; Get second block
	ld a, [de]
	inc e
	add a
	add a
	add 2 ; Bottom row
	ld c, a

	; Write ---------------------------
	push de
	ld d, HIGH(BlockAppearance)

	ld e, b
	wait_vram
	ld a, [de]   ; 2
	ld [hl+], a  ; 2
	inc e        ; 1
	ld a, [de]   ; 2
	ld [hl], a   ; 2
	res 5, l     ; 2 Wrap horizontally
	inc l        ; 1
	set 5, l     ; 2

	ld e, c
	wait_vram
	ld a, [de]   ; 2
	ld [hl+], a  ; 2
	inc e        ; 1
	ld a, [de]   ; 2
	ld [hl], a   ; 2
	res 5, l     ; 2 Wrap horizontally
	inc l        ; 1
	set 5, l     ; 2

	pop de
	pop bc

	dec b
	jr nz, .loop
	ret



