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
	ld sp, StackEnd ; Reset the stack
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

	ld a, BANK(TestLevel)
	ld [rROMB0], a
	ld a, [LevelID]
	add a
	ld hl, LevelPointers
	rst AddHL_A
	ld a, [hl+]
	ld h, [hl]
	ld l, a
	call LoadLevel

	jp StartMainLoop

SECTION "Level Data", ROMX

LevelPointers:
	dw SimpleLevel
	dw SmallTightMazeLevel
	dw WideOpenLevel
	dw BigMazeLevel

SimpleLevel:
	LevelRect 30, 30, 8, 8
	LevelAddWalls WALL_75P
	LevelPutAnywhere 2, BlockType_RescueCritter
	LevelPutAnywhere 1, BlockType_Exit
	LevelAddFloors FLOOR_RARE_STARS
	db LC_END

SmallTightMazeLevel:
	LevelRect 20, 20, 56-40, 56-40
	LevelAddWalls WALL_93P
	LevelPutAnywhere 3, BlockType_RescueCritter
	LevelPutAnywhere 1, BlockType_Exit
	LevelAddFloors FLOOR_RARE_STARS
	db LC_END

WideOpenLevel:
	LevelRect 20, 20, 56-40, 56-40
	LevelAddWalls WALL_25P
	LevelPutAnywhere 5, BlockType_RescueCritter
	LevelPutAnywhere 1, BlockType_Exit
	LevelAddFloors FLOOR_RARE_STARS
	db LC_END

BigMazeLevel:
	LevelRect 12, 12, 56-24, 56-24
	LevelAddWalls WALL_75P
	LevelPutAnywhere 3, BlockType_RescueCritter
	LevelPutAnywhere 1, BlockType_Exit
	LevelAddFloors FLOOR_RARE_STARS
	db LC_END

TestLevel:
;	db LC_RECT, 20,20, 15, 15
;	db LC_TYPE, LEVEL_AREA_2
;	db LC_RECT, 35,20, 5, 15
;	LevelRect 12, 12, 56-24, 56-24
	LevelRect 20, 20, 56-40, 56-40
;	LevelRect 4, 4, 56, 56
	LevelAddWalls WALL_25P ;93P
	LevelPutAnywhere 3, BlockType_RescueCritter
;	LevelPutWithinRect 8, 8, 16, 16, 5, BlockType_EnemyPaint
	LevelPutAnywhere 1, BlockType_Exit
	LevelAddFloors FLOOR_RARE_STARS
	db LC_END
