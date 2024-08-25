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

include "include/macros.inc"
include "include/defines.inc"
include "include/hardware.inc/hardware.inc"
include "res/actor_enum.inc"
include "res/block_enum.inc"

;
;def generate(PlayerShootX, PlayerShootY):
;	print("; %d, %d ----------------------------------" % (PlayerShootX, PlayerShootY))
;
;	for i in (2, 1, 3, 0, 4):
;		x = 0
;		y = 0
;
;		is_diagonal = PlayerShootX != 0 and PlayerShootY != 0
;		scale = 6 if is_diagonal else 8
;		
;		x += PlayerShootY * (i-2) * scale;
;		y += -PlayerShootX * (i-2) * scale;
;
;		print("ld a, e")
;		print("add %d" % y)
;		print("ld [hl+], a ; Y position")
;		print("ld a, d")
;		print("add %d" % x)
;		print("ld [hl+], a ; X position")
;		print("ld a, b")
;		print("ld [hl+],a ; set tile number")
;		print("ld a, c")
;		print("ld [hl+],a ; set attribute\n")
;
;generate(1,   0)
;generate(1,   1)
;generate(0,   1)
;generate(-1,  1)
;generate(-1,  0)
;generate(-1, -1)
;generate(0,  -1)
;generate(1,  -1)

SECTION FRAGMENT "ActorCode", ROMX

DrawPaintLine0:
	; 1, 0 ----------------------------------
	ld a, e
	ld [hl+], a ; Y position
	ld a, d
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	add 8
	ld [hl+], a ; Y position
	ld a, d
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	add -8
	ld [hl+], a ; Y position
	ld a, d
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	add 16
	ld [hl+], a ; Y position
	ld a, d
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	add -16
	ld [hl+], a ; Y position
	ld a, d
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	pop de ; restore "this"
	ld a, l
	ldh [OAMWrite], a
	ret

DrawPaintLine1:
	; 1, 1 ----------------------------------
	ld a, e
	ld [hl+], a ; Y position
	ld a, d
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	add 6
	ld [hl+], a ; Y position
	ld a, d
	add -6
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	add -6
	ld [hl+], a ; Y position
	ld a, d
	add 6
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	add 12
	ld [hl+], a ; Y position
	ld a, d
	add -12
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	add -12
	ld [hl+], a ; Y position
	ld a, d
	add 12
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	pop de ; restore "this"
	ld a, l
	ldh [OAMWrite], a
	ret

DrawPaintLine2:
	; 0, 1 ----------------------------------
	ld a, e
	ld [hl+], a ; Y position
	ld a, d
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	ld [hl+], a ; Y position
	ld a, d
	add -8
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	ld [hl+], a ; Y position
	ld a, d
	add 8
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	ld [hl+], a ; Y position
	ld a, d
	add -16
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	ld [hl+], a ; Y position
	ld a, d
	add 16
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	pop de ; restore "this"
	ld a, l
	ldh [OAMWrite], a
	ret

DrawPaintLine3:
	; -1, 1 ----------------------------------
	ld a, e
	ld [hl+], a ; Y position
	ld a, d
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	add -6
	ld [hl+], a ; Y position
	ld a, d
	add -6
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	add 6
	ld [hl+], a ; Y position
	ld a, d
	add 6
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	add -12
	ld [hl+], a ; Y position
	ld a, d
	add -12
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	add 12
	ld [hl+], a ; Y position
	ld a, d
	add 12
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	pop de ; restore "this"
	ld a, l
	ldh [OAMWrite], a
	ret

DrawPaintLine4:
	; -1, 0 ----------------------------------
	ld a, e
	ld [hl+], a ; Y position
	ld a, d
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	add -8
	ld [hl+], a ; Y position
	ld a, d
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	add 8
	ld [hl+], a ; Y position
	ld a, d
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	add -16
	ld [hl+], a ; Y position
	ld a, d
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	add 16
	ld [hl+], a ; Y position
	ld a, d
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	pop de ; restore "this"
	ld a, l
	ldh [OAMWrite], a
	ret

DrawPaintLine5:
	; -1, -1 ----------------------------------
	ld a, e
	ld [hl+], a ; Y position
	ld a, d
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	add -6
	ld [hl+], a ; Y position
	ld a, d
	add 6
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	add 6
	ld [hl+], a ; Y position
	ld a, d
	add -6
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	add -12
	ld [hl+], a ; Y position
	ld a, d
	add 12
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	add 12
	ld [hl+], a ; Y position
	ld a, d
	add -12
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	pop de ; restore "this"
	ld a, l
	ldh [OAMWrite], a
	ret

DrawPaintLine6:
	; 0, -1 ----------------------------------
	ld a, e
	ld [hl+], a ; Y position
	ld a, d
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	ld [hl+], a ; Y position
	ld a, d
	add 8
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	ld [hl+], a ; Y position
	ld a, d
	add -8
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	ld [hl+], a ; Y position
	ld a, d
	add 16
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	ld [hl+], a ; Y position
	ld a, d
	add -16
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	pop de ; restore "this"
	ld a, l
	ldh [OAMWrite], a
	ret

DrawPaintLine7:
	; 1, -1 ----------------------------------
	ld a, e
	ld [hl+], a ; Y position
	ld a, d
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	add 6
	ld [hl+], a ; Y position
	ld a, d
	add 6
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	add -6
	ld [hl+], a ; Y position
	ld a, d
	add -6
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	add 12
	ld [hl+], a ; Y position
	ld a, d
	add 12
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	ld a, e
	add -12
	ld [hl+], a ; Y position
	ld a, d
	add -12
	ld [hl+], a ; X position
	ld a, b
	ld [hl+],a ; set tile number
	ld a, c
	ld [hl+],a ; set attribute

	pop de ; restore "this"
	ld a, l
	ldh [OAMWrite], a
	ret

DrawPaintLineTable::
	dw DrawPaintLine0
	dw DrawPaintLine1
	dw DrawPaintLine2
	dw DrawPaintLine3
	dw DrawPaintLine4
	dw DrawPaintLine5
	dw DrawPaintLine6
	dw DrawPaintLine7
