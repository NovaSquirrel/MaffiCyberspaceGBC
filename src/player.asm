; Maffi cyberspace game
; Copyright (C) 2022-2024 NovaSquirrel
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
include "res/block_enum.inc"
include "res/actor_enum.inc"

SECTION FRAGMENT "Player", ROMX

RunPlayer::
	; .----------------------------------------------------
	; | Move the player
	; '----------------------------------------------------

	; Count down the timers
	ld hl, PaintShootDiagonalTimer
	ld a, [hl] ; HL: PaintShootDiagonalTimer
	or a
	jr z, :+
		dec [hl]
	:
	inc hl
	ld a, [hl] ; HL: PaintShootingTimer
	or a
	jr z, :+
		dec [hl]
	:
	inc hl
	ld a, [hl] ; HL: PlayerInvincibleTimer
	or a
	jr z, :+
		dec [hl]
	:
	inc hl
	ld a, [hl] ; HL: PaintRefillCooldown
	or a
	jr z, :+
		dec [hl]
		jr .NoRefill
	:
	; PaintRefillCooldown is zero, so refill some paint
	ldh a, [HoldingPaintButton] ; Don't refill if you're rolling
	cp TIME_NEEDED_TO_ROLL
	jr nc, .NoRefill
	ld a, [PaintAmount]
	inc a
	jr z, :+
		ld [PaintAmount], a
	:
	.NoRefill:
	inc hl
	ld a, [hl] ; HL: PlayerNotMovingTimer
	inc a
	jr z, :+
		inc [hl]
	:
	ldh a, [KeyDown]
	and PADF_LEFT | PADF_DOWN | PADF_UP | PADF_RIGHT
	jr z, :+
		ld [hl], 0
	:

	; -------------------------------------------

	ld a, [DieAnimationActive]
	or a
	ret nz

	ldh a, [KeyDown]
	and PADF_UP
	jr z, .NoUp
		ld a, DIRECTION_UP
		ld [PlayerDrawDirection], a
		ld a, DIRECTION8_NORTH
		ld [PaintShootDirection], a

		ldh a, [PlayerPYL]
		sub 16
		ldh a, [PlayerPYH]
		sbc 0
		ld h, a
		ldh a, [PlayerPXH]
		ld l, a
		call MapFlagsLH_XY
		rla
		jr c, .NoUp

		ldh a, [PlayerPYL]
		sub 16
		ldh [PlayerPYL], a
		jr nc, :+
			ld hl, PlayerPYH
			dec [hl]
		:
	.NoUp:

	ldh a, [KeyDown]
	and PADF_DOWN
	jr z, .NoDown
		ld a, DIRECTION_DOWN
		ld [PlayerDrawDirection], a
		ld a, DIRECTION8_SOUTH
		ld [PaintShootDirection], a

		ldh a, [PlayerPYL]
		add 16
		ldh a, [PlayerPYH]
		adc 0
		ld h, a
		ldh a, [PlayerPXH]
		ld l, a
		call MapFlagsLH_XY
		rla
		jr c, .NoDown

		ldh a, [PlayerPYL]
		add 16
		ldh [PlayerPYL], a
		jr nc, :+
			ld hl, PlayerPYH
			inc [hl]
		:
	.NoDown:

	ldh a, [KeyDown]
	and PADF_LEFT
	jr z, .NoLeft
		ld a, DIRECTION_LEFT
		ld [PlayerDrawDirection], a
		ld a, DIRECTION8_WEST
		ld [PaintShootDirection], a

		ldh a, [PlayerPXL]
		sub 16
		ldh a, [PlayerPXH]
		sbc 0
		ld l, a
		ldh a, [PlayerPYH]
		ld h, a
		call MapFlagsLH_XY
		rla
		jr c, .NoLeft

		ldh a, [PlayerPXL]
		sub 16
		ldh [PlayerPXL], a
		jr nc, :+
			ld hl, PlayerPXH
			dec [hl]
		:
	.NoLeft:

	ldh a, [KeyDown]
	and PADF_RIGHT
	jr z, .NoRight
		ld a, DIRECTION_RIGHT
		ld [PlayerDrawDirection], a
		ld a, DIRECTION8_EAST
		ld [PaintShootDirection], a

		ldh a, [PlayerPXL]
		add 16
		ldh a, [PlayerPXH]
		adc 0
		ld l, a
		ldh a, [PlayerPYH]
		ld h, a
		call MapFlagsLH_XY
		rla
		jr c, .NoRight

		ldh a, [PlayerPXL]
		add 16
		ldh [PlayerPXL], a
		jr nc, :+
			ld hl, PlayerPXH
			inc [hl]
		:
	.NoRight:

	; Do diagonals too
	ldh a, [KeyDown]
	ld b, a
	and PADF_RIGHT|PADF_DOWN
	cp  PADF_RIGHT|PADF_DOWN
	jr nz, :+
		ld a, DIRECTION8_SOUTHEAST
		call DoDiagonalLock
	:
	ld a, b
	and PADF_LEFT|PADF_DOWN
	cp  PADF_LEFT|PADF_DOWN
	jr nz, :+
		ld a, DIRECTION8_SOUTHWEST
		call DoDiagonalLock
	:
	ld a, b
	and PADF_RIGHT|PADF_UP
	cp  PADF_RIGHT|PADF_UP
	jr nz, :+
		ld a, DIRECTION8_NORTHEAST
		call DoDiagonalLock
	:
	ld a, b
	and PADF_LEFT|PADF_UP
	cp  PADF_LEFT|PADF_UP
	jr nz, :+
		ld a, DIRECTION8_NORTHWEST
		call DoDiagonalLock
	:

	ld a, [PaintShootDiagonalTimer]
	or a
	jr z, :+
		ld a, [PaintShootDiagonalDirection]
		ld [PaintShootDirection], a
	:

	; Check for special floors
	ldh a, [PlayerPXH]
	ld l, a
	ldh a, [PlayerPYH]
	ld h, a
	call MapPointerLH_XY
	ld a, [hl]
	sub BlockMarker_SpecialFloor
	jr c, .NoSpecialFloor
	cp BlockMarker_EndSpecialFloor - BlockMarker_SpecialFloor
	jr nc, .NoSpecialFloor
		push hl
		add a
		ld hl, BlockRoutinesAbove
		add_hl_a
		ld a, [hl+]
		ld d, [hl]
		ld e, a
		pop hl
		rst CallDE
	.NoSpecialFloor:

	; -------------------------------------------
	ld hl, HoldingPaintButton
	ldh a, [KeyDown]
	and PADF_A
	jr nz, .YesHolding
		ld [hl], 0
		jr .NotHolding
	.YesHolding:
		bit 7, [hl] ; Max out at 128
		jr nz, .NotHolding
		inc [hl]
	.NotHolding:

	ldh a, [KeyNew]
	and PADF_A
	jr z, .NotShootStart
	ld a, [PaintShootingTimer]
	or a
	jr nz, .NotShootStart
		ld a, [PaintShootDirection]
		ld [PaintShootDirectionLock], a

		ld a, 25
		ld [PaintShootingTimer], a
	.NotShootStart:

	; -------------------------------------------
	ld a, [PaintShootingTimer]
	cp 24
	jr nz, .NoFlick
	ld hl, PaintAmount
	ld a, [hl]
	sub 64
	jr c, .NoFlick
		ld [hl], a
		ld a, 10
		ld [PaintRefillCooldown], a

		ld hl, PaintShotID
		inc [hl]
		set 7, [hl] ; Make sure zero never matches any IDs actually used

		; Get X and Y speeds
		ld a, [PaintShootDirectionLock]
		add a, a
		ld hl, PaintVelocities
		add_hl_a
		ld a, [hl+]
		ldh [temp1], a
		ld a, [hl]
		ldh [temp2], a

		ld a, [PaintShootDirectionLock]
		ld b, a
		add a ; * 2
		add a ; * 4
		add b ; * 5
		add a ; * 10
		add a ; * 20
		ld de, PaintLineCoordinates
		add_de_a
		ld b, 5
		ld hl, PlayerProjectiles ; Start making projectiles from the start
	:	push bc
		; Make flick particle
		ld [hl], ActorType_PaintProjectile
		inc l
		xor a
		ld [hl+], a ; actor_state
		ld [hl+], a ; actor_timer

		ldh a, [temp1]
		ld [hl+], a ; actor_vxl
		ldh a, [temp2]
		ld [hl+], a ; actor_vyl

		ldh a, [PlayerPYL]
		ld c, a
		ldh a, [PlayerPYH]
		ld b, a
		ld a, [de]
		inc de
		add c
		ld [hl+], a ; actor_pyl
		ld a, [de]
		inc de
		adc b
		ld [hl+], a ; actor_pyh

		ldh a, [PlayerPXL]
		ld c, a
		ldh a, [PlayerPXH]
		ld b, a
		ld a, [de]
		inc de
		add c
		ld [hl+], a ; actor_pxl
		ld a, [de]
		inc de
		adc b
		ld [hl+], a ; actor_pxh

		ld a, [PaintShotID]
		ld [hl+], a ; actor_var1
		pop bc
		ld [hl], b ; HL points to actor_var2, so the first paint projectile will have a 5 here

		ld a, l    ; Move to the next actor
		add (actor_type+ACTOR_SIZE)-actor_var2
		ld l, a
	
		dec b
		jr nz, :-
	:
	.NoFlick:

	ldh a, [HoldingPaintButton]
	cp TIME_NEEDED_TO_ROLL
	jr c, .NoPaintMap
		; Paint the map
		ldh a, [KeyDown]
		and PADF_LEFT | PADF_DOWN | PADF_UP | PADF_RIGHT
		jr z, .NoPaintMap
			ldh a, [framecount]
			and 3
			jr nz, .NoPaintMap

			ld hl, PaintAmount
			ld a, [hl]
			or a
			jr z, .NoPaintMap
			sub 2
			jr c, .NoPaintMap
			ld [hl], a
			ld a, 10
			ld [PaintRefillCooldown], a

			; Paint the ground
			ldh a, [PlayerPXH]
			ld l, a
			ldh a, [PlayerPYH]
			ld h, a
			call MapPointerLH_XY
			ld a, [hl]
			push hl
			get_block_flags
			pop hl
			and BLOCK_CLASS_MASK
			cp BlockClass_Paintable
			ld a, BlockType_Paint
			call z, BlockChangeForPlayer
	.NoPaintMap:
	ret

DoDiagonalLock:
	ld [PaintShootDiagonalDirection], a
	ld a, 4
	ld [PaintShootDiagonalTimer], a
	ret

PaintVelocities:
	db 16,   0
	db 16,   16
	db 0,    16
	db -16,  16
	db -16,  0
	db -16, -16
	db 0,   -16
	db 16,  -16
PaintLineCoordinates:
; 1, 0
	dw 0, 128
	dw 128, 128
	dw -128, 128
	dw 256, 128
	dw -256, 128
; 1, 1
	dw 128, 128
	dw 224, 32
	dw 32, 224
	dw 320, -64
	dw -64, 320
; 0, 1
	dw 128, 0
	dw 128, -128
	dw 128, 128
	dw 128, -256
	dw 128, 256
; -1, 1
	dw 128, -128
	dw 32, -224
	dw 224, -32
	dw -64, -320
	dw 320, 64
; -1, 0
	dw 0, -128
	dw -128, -128
	dw 128, -128
	dw -256, -128
	dw 256, -128
; -1, -1
	dw -128, -128
	dw -224, -32
	dw -32, -224
	dw -320, 64
	dw 64, -320
; 0, -1
	dw -128, 0
	dw -128, 128
	dw -128, -128
	dw -128, 256
	dw -128, -256
; 1, -1
	dw -128, 128
	dw -32, 224
	dw -224, 32
	dw 64, 320
	dw -320, -64

; ---------------------------------------------------------
DrawPlayer::
	ldh a, [IsNotGameBoyColor] ; On GBC, draw direction takes effect immediately
	or a
	jr nz, :+
		ld a, [PlayerDrawDirection]
		ld [DMG_PlayerDrawDirection], a
	:

	; .----------------------------------------------------
	; | Draw the player
	; '----------------------------------------------------
	def PlayerTile1 equs "temp1"
	def PlayerTile2 equs "temp2"
	def PlayerTile3 equs "temp3"
	def PlayerTile4 equs "temp4"
	def PlayerXWithoutOffset equs "temp5"

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
	ldh [PlayerXWithoutOffset], a
	ldh [RollingCollisionX], a
	add 8-(PLAYER_COLLISION_WIDTH/2)
	ldh [PlayerCollisionX], a

	; Adjust the horizontal offset for the pose, to keep the player centered and allow the tail to extend to the side differently
	ld hl, HorizontalOffsetForPose
	ld a, [DMG_PlayerDrawDirection]
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
	add 16-24-7
	ld e, a
	ldh [RollingCollisionY], a
	add 31-(PLAYER_COLLISION_HEIGHT/2)
	ldh [PlayerCollisionY], a

	; Adjust RollingCollisionX and RollingCollisionY if they're needed
	ldh a, [HoldingPaintButton]
	cp TIME_NEEDED_TO_ROLL
	jr c, :+
		ld hl, RollingCollisionX
		ld bc, RollingCollisionXYAdjustForDirection
		ld a, [PlayerDrawDirection]
		add a
		add_bc_a
		ld a, [bc]
		add [hl]
		ld [hl+], a ; X
		inc bc
		ld a, [bc]
		add [hl]
		ld [hl], a ; Y
	:

	; Die animation
	ld hl, DieAnimationActive
	ld a, [hl+]
	or a
	jr z, .NoDieAnimation
		; Increase gravity
		ld a, [hl]  ; VYL
		cp $78
		jr z, :+
			add 4
		:
		ld [hl], a  ; VYL
		call DieAnimationShared
		add e
		ld e, a

		cp 144+16
		jr c, :+
			ld a, [LevelID]
			jp StartLevel
		:

		ld a, [hl]
		call DieAnimationShared
		add d
		ld d, a
	.NoDieAnimation:

	; Get the animation frame for the direction, which will become a source pointer for the DMA or the slower copy
	ld a, [PlayerDrawDirection]
	ld hl, PlayerFrameForPose
	add_hl_a
	ld a, [hl]
	ld [PlayerAnimationFrame], a

	; Set up PlayerTile1 through PlayerTile4 with 0-3
	ld a, [DMG_PlayerAnimationFrame_Page] ; <-- while keeping in mind the double buffering on DMG
	add a
	add a
	add a
	ld hl, PlayerTile1
	ld b, 4
:	ld [hl+], a
	inc a
	inc a
	dec b
	jr nz, :-


	; Hurt animation
	ld a, [DieAnimationActive]
	or a
	jr nz, .DoDieAnimation
	ld a, [PlayerInvincibleTimer]
	cp PLAYER_HURT_INVINCIBILITY - 30
	jr c, :+
	.DoDieAnimation:
		ld a, [PlayerAnimationFrame]
		add PLAYER_FRAME_R_HURT - PLAYER_FRAME_R
		ld [PlayerAnimationFrame], a
		jr .NotPressingDirection
	:
	or a
	jr z, :+
		and 3
		ret z ; Just don't draw the player at all
	:
	ldh a, [HoldingPaintButton]
	cp TIME_NEEDED_TO_ROLL
	jr c, .NotRolling
		ld a, [PlayerAnimationFrame]
		add PLAYER_FRAME_R_SHOOT2 - PLAYER_FRAME_R
		ld [PlayerAnimationFrame], a
		ldh a, [IsNotGameBoyColor]
		or a
		jr z, :+
			ld a, [PlayerAnimationFrame]
			add PLAYER_FRAME_R_ROLLING_DMG - PLAYER_FRAME_R_SHOOT2
			ld [PlayerAnimationFrame], a
		:
		jr .CanDoWalk
	.NotRolling:
	; Shooting animation
	ld a, [PaintShootingTimer]
	cp 25/2
	jr c, :+
		ld a, [PlayerAnimationFrame]
		add PLAYER_FRAME_R_SHOOT2 - PLAYER_FRAME_R
		ld [PlayerAnimationFrame], a
		jr .CanDoWalk
;		jr .NotPressingDirection
	:
	cp 25/2-4
	jr c, :+
		ld a, [PlayerAnimationFrame]
		add PLAYER_FRAME_R_SHOOT - PLAYER_FRAME_R
		ld [PlayerAnimationFrame], a
		jr .NotPressingDirection
	:
	.CanDoWalk:

	; Walking animation
	ldh a, [KeyDown]
	and PADF_LEFT | PADF_DOWN | PADF_UP | PADF_RIGHT
	jr z, .NotPressingDirection
		ld hl, WalkCycle
		ldh a, [framecount]
		rrca
		rrca
		and 3
		add_hl_a
		ld c, [hl]

		; Offset the animation frame number
		ld a, [PlayerAnimationFrame]
		add c
		ld [PlayerAnimationFrame], a
	.NotPressingDirection:

	; Flip if left
	ld b, SP_PALETTE_PLAYER
	ld a, [DMG_PlayerDrawDirection]
	cp DIRECTION_LEFT
	jr nz, :+
		ldh a, [PlayerTile1]
		ldh [PlayerTile2], a
		add 2
		ldh [PlayerTile1], a

		ldh a, [PlayerTile3]
		ldh [PlayerTile4], a
		add 2
		ldh [PlayerTile3], a
		ld b, OAMF_XFLIP|SP_PALETTE_PLAYER
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
	ldh a, [OAMWrite]
	ld l, a

; --------------------------------
; Add rolling brush if needed
DrawRollingBrush:
	ldh a, [IsNotGameBoyColor]
	or a
	jr nz, NoFaceSprite
	ldh a, [HoldingPaintButton]
	cp TIME_NEEDED_TO_ROLL
	jr c, .NoDrawBrush
		ld a, [PlayerDrawDirection]
		cp DIRECTION_UP
		jr z, .NoDrawBrush
		push bc
		push de
		add a
		ld bc, PaintbrushYXOffset
		add_bc_a

		ld a, [bc]
		add e ; Y position
		ld [hl+], a
		inc bc
		ldh a, [PlayerXWithoutOffset]
		ld d, a
		ld a, [bc]
		add d ; X position
		ld [hl+], a
		ld a, TILE_ID_PAINTBRUSH_DOWN
		ld [hl+], a
		ld a, SP_PALETTE_PLAYER
		ld [hl+], a
		pop de
		pop bc
	.NoDrawBrush:

	; --------------------------------
	; Add face sprite above player

	;ldh a, [IsNotGameBoyColor]
	;or a
	;jp nz, NoFaceSprite
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

		ld a, TILE_ID_PLAYER_FACE_RIGHT ; Tile
		jr FaceSpriteFinish
	FaceSpriteDown:
		ld a, e
		add 5
		ld [hl+], a ; Y position
		ld a, d
		add 2
		ld [hl+], a ; X position

		ld a, TILE_ID_PLAYER_FACE_DOWN ; Tile
	FaceSpriteFinish:
		ld [hl+], a ; tile number
		ld a, b
		dec a       ; use the previous palette but keep the flip
		ld [hl+], a ; attribute
	NoFaceSprite:

	; Now add the rest of the player

	; Loop to create the OAM entries
	ld c, 2     ; 2 rows
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

; When facing up, draw this below the player instead of above
; though this should be solvable with just a different graphic for the player, if the brush continues to use the same palette
DrawRollingBrushBelow:
	ldh a, [HoldingPaintButton]
	cp TIME_NEEDED_TO_ROLL
	jr c, .NoDrawBrush
		ld a, [PlayerDrawDirection]
		cp DIRECTION_UP
		jr nz, .NoDrawBrush
		ldh a, [IsNotGameBoyColor]
		or a
		jr nz, .NoDrawBrush

		ld a, e
		sub 6+16+2
		ld [hl+], a
		ldh a, [PlayerXWithoutOffset]
		add 4
		ld [hl+], a
		ld a, TILE_ID_PAINTBRUSH_DOWN
		ld [hl+], a
		ld a, SP_PALETTE_PLAYER
		ld [hl+], a
	.NoDrawBrush:

	ld a, l
	ldh [OAMWrite], a
	ret

DieAnimationShared:
	sex
	ld b, a

	ld a, [hl+] ; VYL
	add [hl]    ; PYL
	ld [hl+], a ; PYL
	ld c, a     ; C = PYL

	ld a, b
	adc [hl]    ; PYH
	ld [hl+], a  ; PYH
	ld b, a     ; B = PYH
	ld a, c
	rept 4
	srl b
	rra
	endr
	ret


PlayerFrameForPose:
	db PLAYER_FRAME_R, PLAYER_FRAME_D, PLAYER_FRAME_R, PLAYER_FRAME_U
WalkCycle:
	db 0, 1, 0, 2
HorizontalOffsetForPose:
	db -2, 2, 2, -2
PaintbrushYXOffset:
	DEF PBDrawOffsetX = 4
	DEF PBDrawOffsetY = 16
	DEF PBDrawDistance = 6
	db PBDrawOffsetY,                PBDrawOffsetX+PBDrawDistance
;	db PBDrawOffsetY+PBDrawDistance, PBDrawOffsetX+PBDrawDistance
	db PBDrawOffsetY+PBDrawDistance-2, PBDrawOffsetX
;	db PBDrawOffsetY+PBDrawDistance, PBDrawOffsetX-PBDrawDistance
	db PBDrawOffsetY,                PBDrawOffsetX-PBDrawDistance
;	db PBDrawOffsetY-PBDrawDistance, PBDrawOffsetX-PBDrawDistance
	db PBDrawOffsetY-PBDrawDistance+2, PBDrawOffsetX
;	db PBDrawOffsetY-PBDrawDistance, PBDrawOffsetX+PBDrawDistance

RollingCollisionXYAdjustForDirection:
; Right
	db 8-(ROLLING_COLLISION_WIDTH/2)+6, 31-(ROLLING_COLLISION_HEIGHT/2)
; Down
	db 8-(ROLLING_COLLISION_WIDTH/2), 31-(ROLLING_COLLISION_HEIGHT/2)+6+2
; Left
	db 8-(ROLLING_COLLISION_WIDTH/2)-6, 31-(ROLLING_COLLISION_HEIGHT/2)
; Up
	db 8-(ROLLING_COLLISION_WIDTH/2), 31-(ROLLING_COLLISION_HEIGHT/2)-6-2

; Player frames
	rsreset
	def PLAYER_FRAME_R rb
	def PLAYER_FRAME_R2 rb
	def PLAYER_FRAME_R3 rb
	def PLAYER_FRAME_R_SHOOT rb
	def PLAYER_FRAME_R_SHOOT2 rb ; And roll
	def PLAYER_FRAME_R_ROLL1 rb
	def PLAYER_FRAME_R_ROLL2 rb
	def PLAYER_FRAME_R_HURT rb
	def PLAYER_FRAME_R_ROLLING_DMG rb
	def PLAYER_FRAME_R_ROLLING_DMG2 rb
	def PLAYER_FRAME_R_ROLLING_DMG3 rb
	def PLAYER_FRAME_D rb
	def PLAYER_FRAME_D2 rb
	def PLAYER_FRAME_D3 rb
	def PLAYER_FRAME_D_SHOOT rb
	def PLAYER_FRAME_D_SHOOT2 rb
	def PLAYER_FRAME_D_ROLL1 rb
	def PLAYER_FRAME_D_ROLL2 rb
	def PLAYER_FRAME_D_HURT rb
	def PLAYER_FRAME_D_ROLLING_DMG rb
	def PLAYER_FRAME_D_ROLLING_DMG2 rb
	def PLAYER_FRAME_D_ROLLING_DMG3 rb
	def PLAYER_FRAME_U rb
	def PLAYER_FRAME_U2 rb
	def PLAYER_FRAME_U3 rb
	def PLAYER_FRAME_U_SHOOT rb
	def PLAYER_FRAME_U_SHOOT2 rb
	def PLAYER_FRAME_U_ROLL1 rb
	def PLAYER_FRAME_U_ROLL2 rb
	def PLAYER_FRAME_U_HURT rb
	def PLAYER_FRAME_U_ROLLING_DMG rb
	def PLAYER_FRAME_U_ROLLING_DMG2 rb
	def PLAYER_FRAME_U_ROLLING_DMG3 rb

; ---------------------------------------------------------

SECTION "PlayerGraphics", ROMX, ALIGN[4]
PlayerAnimationFrameGraphics::
	incbin "res/tilesets_8x16/MaffiWalkR.2bpp"
	incbin "res/tilesets_8x16/MaffiWalkD.2bpp"
	incbin "res/tilesets_8x16/MaffiWalkU.2bpp"

; ---------------------------------------------------------
SECTION FRAGMENT "Player", ROMX

DrawPaintBar::
	; Potentially update the paint bar
	ld hl, PaintAmountShownOnscreen
	ld a, [PaintAmount]
	and %11111000
	cp b
	ret z
	; OK, update it
	ld [hl], a

	ld de, PaintBarLUT
	add_de_a

	ld hl, _SCRN1+6
	ld b, 2
:	wait_vram
	ld a, [de]  ; 2
	ld [hl+], a ; 2
	inc de      ; 2
	ld a, [de]  ; 2
	ld [hl+], a ; 2
	inc de      ; 2
	ld a, [de]  ; 2
	ld [hl+], a ; 2
	inc de      ; =16
	dec b
	jr nz, :-

	; Get the last two tiles
	wait_vram
	ld a, [de]  ; 2
	ld [hl+], a ; 2
	inc de      ; 2
	ld a, [de]  ; 2
	ld [hl+], a ; 2
	ret

PaintBarLUT:
; $f1=empty, $f2=1/4, $f3=1/4, $f4=3/4, $f5, full
def PAINT_BAR_0 EQU $f1
def PAINT_BAR_1 EQU $f2
def PAINT_BAR_2 EQU $f3
def PAINT_BAR_3 EQU $f4
def PAINT_BAR_4 EQU $f5
	db PAINT_BAR_1, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0
	db PAINT_BAR_2, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0
	db PAINT_BAR_3, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0
	db PAINT_BAR_4, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0
	db PAINT_BAR_4, PAINT_BAR_1, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0
	db PAINT_BAR_4, PAINT_BAR_2, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0
	db PAINT_BAR_4, PAINT_BAR_3, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0
	db PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0
	db PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_1, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0
	db PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_2, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0
	db PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_3, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0
	db PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0
	db PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_1, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0
	db PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_2, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0
	db PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_3, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0
	db PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0
	db PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_1, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0
	db PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_2, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0
	db PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_3, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0
	db PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_0, PAINT_BAR_0, PAINT_BAR_0
	db PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_1, PAINT_BAR_0, PAINT_BAR_0
	db PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_2, PAINT_BAR_0, PAINT_BAR_0
	db PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_3, PAINT_BAR_0, PAINT_BAR_0
	db PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_0, PAINT_BAR_0
	db PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_1, PAINT_BAR_0
	db PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_2, PAINT_BAR_0
	db PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_3, PAINT_BAR_0
	db PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_0
	db PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_1
	db PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_2
	db PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_3
	db PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4, PAINT_BAR_4
