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
include "include/defines.inc"
include "include/hardware.inc/hardware.inc"
include "res/blockenum.inc"

SECTION "Player", ROM0

RunPlayer::
	; .----------------------------------------------------
	; | Move the player
	; '----------------------------------------------------

	ld hl, PlayerPYH
	ldh a, [KeyDown]
	and PADF_UP
	jr z, :+
		ld a, DIRECTION_UP
		ld [PlayerDrawDirection], a
		ldh a, [PlayerPYL]
		sub 16
		ldh [PlayerPYL], a
		jr nc, :+
			dec [hl]
	:

	ldh a, [KeyDown]
	and PADF_DOWN
	jr z, :+
		ld a, DIRECTION_DOWN
		ld [PlayerDrawDirection], a
		ldh a, [PlayerPYL]
		add 16
		ldh [PlayerPYL], a
		jr nc, :+
			inc [hl]
	:

	ld hl, PlayerPXH
	ldh a, [KeyDown]
	and PADF_LEFT
	jr z, :+
		ld a, DIRECTION_LEFT
		ld [PlayerDrawDirection], a
		ldh a, [PlayerPXL]
		sub 16
		ldh [PlayerPXL], a
		jr nc, :+
			dec [hl]
	:
	ldh a, [KeyDown]
	and PADF_RIGHT
	jr z, :+
		ld a, DIRECTION_RIGHT
		ld [PlayerDrawDirection], a
		ldh a, [PlayerPXL]
		add 16
		ldh [PlayerPXL], a
		jr nc, :+
			inc [hl]
	:

	; .----------------------------------------------------
	; | Draw the player
	; '----------------------------------------------------
	def PlayerTile1 equs "temp1"
	def PlayerTile2 equs "temp2"
	def PlayerTile3 equs "temp3"
	def PlayerTile4 equs "temp4"
	def PlayerTile5 equs "temp5"
	def PlayerTile6 equs "temp6"

	; Get X position first
	ldh a, [CameraX+0]
	ld c, a
	ldh a, [CameraX+1]
	ld b, a
	
	ldh a, [PlayerPXL]
	sub c
	ld c, a
	ldh a, [PlayerPXH]
	call SharedCameraSubtractCode
	add 8-8
	ld d, a

	; Adjust for the pose
	ld hl, HorizontalOffsetForPose
	ld a, [PlayerDrawDirection]
	rst_add_hl_a
	ld a, d
	add [hl]
	ld d, a

	; Get Y position next
	ldh a, [CameraY+0]
	ld c, a
	ldh a, [CameraY+1]
	ld b, a
	
	ldh a, [PlayerPYL]
	sub c
	ld c, a
	ldh a, [PlayerPYH]
	call SharedCameraSubtractCode
	add 16-24
	ld e, a

	; Get the six tiles for the direction
	ld a, [PlayerDrawDirection]
	ld hl, FirstTileForPose
	rst_add_hl_a
	ld a, [hl]
	ld hl, PlayerTile1
	ld b, 6
:	ld [hl+], a
	inc a
	dec b
	jr nz, :-

	ldh a, [KeyDown]
	and PADF_LEFT | PADF_DOWN | PADF_UP | PADF_RIGHT
	jr z, .NotPressingDirection
		ld hl, WalkCycle
		ldh a, [framecount]
		rrca
		rrca
		and 3
		rst_add_hl_a
		ld c, [hl]

		; Add offset to bottom two tiles
		ld hl, PlayerTile5
		ld a, [hl]
		add c
		ld [hl+], a
		ld a, [hl]
		add c
		ld [hl+], a
	.NotPressingDirection:

	; Flip if left
	ld b, PALETTE_PLAYER
	ld a, [PlayerDrawDirection]
	cp DIRECTION_LEFT
	jr nz, :+
		hswap [PlayerTile1], [PlayerTile2]
		hswap [PlayerTile3], [PlayerTile4]
		hswap [PlayerTile5], [PlayerTile6]
		ld b, OAMF_XFLIP|PALETTE_PLAYER
	:

	; B  = Attribute
	; C  = Number of rows
	; D  = X position
	; E  = Y position
	; HL = OAM write pointer

	; Push all of the tiles first
	ld hl, PlayerTile6
	ld c, 6
:  	ld a, [hl-]
	push af
	dec c
	jr nz, :-

	ld h, HIGH(OamBuffer)
	ldh a, [OamWrite]
	ld l, a

; --------------------------------
	; Loop to create the OAM entries
	ld c, 3
:	ld a, e
	ld [hl+], a ; Y position
	ld a, d
	ld [hl+], a ; X position
	pop af
	ld [hl+],a ; set tile number
	ld a, b
	ld [hl+],a ; set attribute

	ld a, e
	ld [hl+], a ; Y position
	ld a, d
	add a, 8
	ld [hl+], a ; X position
	pop af
	ld [hl+],a ; set tile number
	ld a, b
	ld [hl+],a ; set attribute

	ld a, e
	add 8
	ld e, a

	dec c
	jr nz, :-

; --------------------------------

	ld a, l
	ldh [OamWrite], a
	ret

FirstTileForPose:
	db 0, 16, 0, 32
WalkCycle:
	db 0, 2, 0, 4
HorizontalOffsetForPose:
	db -2, 2, 2, -2

SharedCameraSubtractCode:
	sbc b
	ld b, a

	ld a, c
	rept 4
		sra b
		rra
	endr
	adc 0
	; A = low half of BC>>4
	ret
