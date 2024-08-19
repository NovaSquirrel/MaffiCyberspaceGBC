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

SECTION "Player", ROMX

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
	ret

; ---------------------------------------------------------
DrawPlayer::
	; .----------------------------------------------------
	; | Draw the player
	; '----------------------------------------------------
	def PlayerTile1 equs "temp1"
	def PlayerTile2 equs "temp2"
	def PlayerTile3 equs "temp3"
	def PlayerTile4 equs "temp4"

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
	add_hl_a
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
	ld hl, PlayerFrameForPose
	rst_add_hl_a
	ld a, [hl]
	ld [PlayerAnimationFrame], a

	; Set up PlayerTile1 through PlayerTile6 with 0-5
	xor a
	ld hl, PlayerTile1
	ld b, 4
:	ld [hl+], a
	inc a
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

		; Offset the animation frame number
		ld a, [PlayerAnimationFrame]
		add c
		ld [PlayerAnimationFrame], a
	.NotPressingDirection:

	; Flip if left
	ld b, PALETTE_PLAYER
	ld a, [PlayerDrawDirection]
	cp DIRECTION_LEFT
	jr nz, :+
		hswap [PlayerTile1], [PlayerTile2]
		hswap [PlayerTile3], [PlayerTile4]
		ld b, OAMF_XFLIP|PALETTE_PLAYER
	:

	; B  = Attribute
	; C  = Number of rows
	; D  = X position
	; E  = Y position
	; HL = OAM write pointer

	; Push all of the tiles first
	ld hl, PlayerTile4
	ld c, 4
:  	ld a, [hl-]
	push af
	dec c
	jr nz, :-

	ld h, HIGH(OamBuffer)
	ldh a, [OamWrite]
	ld l, a

; --------------------------------
	; Add extra sprite first

	ldh a, [IsNotGameBoyColor]
	or a
	jp nz, NoFaceSprite
	ld a, [PlayerDrawDirection]
	cp DIRECTION_UP
	jr z, NoFaceSprite
	cp DIRECTION_DOWN
	jr z, FaceSpriteDown
	FaceSpriteLeftRight:
		ld a, e
		add 4
		ld [hl+], a ; Y position

		ld a, d
		add 2
		bit 5, b    ; Check X flip bit in attribute
		jr nz, :+
			add 4
		:
		ld [hl+], a ; X position

		ld a, 8 ; Tile
		jr FaceSpriteFinish
	FaceSpriteDown:
		ld a, e
		add 5
		ld [hl+], a ; Y position
		ld a, d
		add 2
		ld [hl+], a ; X position

		ld a, 10 ; Tile
	FaceSpriteFinish:
		ld [hl+], a ; tile number
		ld a, b
		dec a
		ld [hl+], a ; attribute
	NoFaceSprite:

	; Now add the rest of the player

	; Loop to create the OAM entries
	ld c, 2     ; 3 rows
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

	ld a, e    ; go down 16 pixels
	add 16
	ld e, a

	dec c
	jr nz, :-

; --------------------------------

	ld a, l
	ldh [OamWrite], a
	ret

PlayerFrameForPose:
	db PLAYER_FRAME_R, PLAYER_FRAME_D, PLAYER_FRAME_R, PLAYER_FRAME_U
WalkCycle:
	db 0, 1, 0, 2
HorizontalOffsetForPose:
	db -2, 2, 2, -2

; Input: B (camera position high byte), A (entity position high byte),
;        C (entity position low byte; should already have subtracted camera position low byte)
; Output: A (pixel position)
SharedCameraSubtractCode:
	sbc b
	ld b, a

	; BC = entity position - camera
	ld a, c
	rept 4
		sra b
		rra
	endr
	adc 0
	; A = low half of BC>>4
	ret

; Player frames
	enum_start
	enum_elem PLAYER_FRAME_R
	enum_elem PLAYER_FRAME_R2
	enum_elem PLAYER_FRAME_R3
	enum_elem PLAYER_FRAME_D
	enum_elem PLAYER_FRAME_D2
	enum_elem PLAYER_FRAME_D3
	enum_elem PLAYER_FRAME_U
	enum_elem PLAYER_FRAME_U2
	enum_elem PLAYER_FRAME_U3

SECTION "PlayerGraphics", ROMX, ALIGN[4]
PlayerAnimationFrameGraphics::
	incbin "res/tilesets_8x16/MaffiWalkR.2bpp"
	incbin "res/tilesets_8x16/MaffiWalkD.2bpp"
	incbin "res/tilesets_8x16/MaffiWalkU.2bpp"
