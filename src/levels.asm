; Maffi cyberspace game
; Copyright (C) 2024 NovaSquirrel
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
include "include/leveldata.inc"
include "res/block_enum.inc"

SECTION "Level Load", ROM0

; Takes a level number in register A, and loads/generates the map, and does other setup
StartLevel::
	ld [LevelID], a

	; Clear the playfield
	ld hl, Playfield
	xor a
:
	rept 8
	ld [hl+], a
	endr
	bit 4, h
	jr nz, :- ; When Dx goes to Ex it's done

	call InitParallax

	ld hl, TestLevel
	call LoadLevel

	;---------------------------------------
	; Set the palettes and attribute screen
	ld a, [IsSuperGameBoy]
	or a
	jr z, :+
		ld a, BANK(sgb_set_palettes_bcde_attr_a)
		ld [rROMB0], a
		ld b, 13
		ld c, 1
		ld d, 2
		ld e, 3
		ld a, %11000001
		call sgb_set_palettes_bcde_attr_a
	:

	jp StartMainLoop

SECTION "Level Data", ROMX

TestLevel:
;	db LC_RECT, 20,20, 15, 15
;	db LC_TYPE, LEVEL_AREA_2
;	db LC_RECT, 35,20, 5, 15

	db LC_RECT, 4, 4, 56, 56

	db LC_ADD_WALLS
	db LC_PUT_ANYWHERE_VISITED, 20, BlockType_RescueCritter
	db LC_ADD_FLOORS
	db LC_END
