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
include "include/hardware.inc/hardware.inc"
include "include/macros.inc"

SECTION "Parallax", ROM0

InitParallax::
	ld hl, ParallaxTemplate
	ld de, ParallaxShifts
	ld b, 16
	call memcpy8
	ld hl, ParallaxTemplate
	ld de, ParallaxShifts+16
	ld b, 16
	call memcpy8

	; Do the shifts
	ld hl, ParallaxShifts
	ld de, ParallaxShifts+32
	ld b, 256-32
.loop:
	ld a, [hl+]
	rrca
	ld [de], a
	inc de
	dec b
	jr nz, .loop
	ret

ParallaxTemplate:
;	db %10101110, 0
;	db %01001101, 0
;	db %11101111, 0
;	db %11111111, 0
;	db %00001000, 0
;	db %01001101, 0
;	db %10101110, 0
;	db %01001101, 0

; Stone, flipped
	db %10001010, 0
	db %01001101, 0
	db %00001000, 0
	db %00000000, 0
	db %11101111, 0
	db %01001101, 0
	db %10001010, 0
	db %01001101, 0

; Stone, but not flipped
;	db %01010001, 0
;	db %10110010, 0
;	db %00010000, 0
;	db %00000000, 0
;	db %11110111, 0
;	db %10110010, 0
;	db %01010001, 0
;	db %10110010, 0

;	db %00000110, 0
;	db %00001001, 0
;	db %00001001, 0
;	db %00000110, 0
;	db %01100000, 0
;	db %10010000, 0
;	db %10010000, 0
;	db %01100000, 0

;	db %00111100, 0
;	db %01000010, 0
;	db %10100101, 0
;	db %10000001, 0
;	db %10100101, 0
;	db %10011001, 0
;	db %01000010, 0
;	db %00111100, 0
