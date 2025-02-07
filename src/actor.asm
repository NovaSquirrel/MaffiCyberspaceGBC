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

SECTION FRAGMENT "ActorCode", ROMX

; .----------------------------------------------------------------------------
; | Loop through the actors
; '----------------------------------------------------------------------------

; Run through all of the actors
RunActors::
; Run projectiles first so they display on top of enemies
.RunProjectiles:
	ld d, HIGH(PlayerProjectiles)
	ld e, 0
.ProjectileLoop:
	ld a, [de]
	or a
	call nz, .call

	ld a, e
	add ACTOR_SIZE
	cp ACTOR_SIZE * PLAYER_PROJECTILE_COUNT
	jr z, .RunNormalActors
	ld e, a
	jr .ProjectileLoop

; Other actors, including enemies
.RunNormalActors:
	; Reset the enemy count so we can start counting up from zero
	xor a
	ld [EnemyCount], a

	ld d, HIGH(ActorData)
	ld e, 0
.loop:
	ld a, [de]
	or a
	call nz, .call

	ld a, e
	add ACTOR_SIZE
	ld e, a
	ret z
	jr .loop

.call:
	add a ; multiply by 2, mask off direction bit
	ld h, HIGH(ActorPointers)
	ld l, a

	ld a, [hl+]
	ld h, [hl]
	ld l, a
	jp hl


; .----------------------------------------------------------------------------
; | Per-actor code
; | Called with DE = actor pointer
; '----------------------------------------------------------------------------

ActorNothing::
	ret

ActorSneaker::
	call EnemyCommon
	jr nc, .DrawOnly

	ld b, $08
	call ActorGetTileUnderneath
	cp BlockType_Paint
	jr nz, :+
		ld b, $04
	:
	ld a, b
	call ActorWalkTowardPlayerAndBump

.DrawOnly:
	ldh a, [framecount]
	rrca
	and %100
	ld b, 0
	jp DrawEnemy_16x16_AndCollide


ActorKitty::
	call EnemyCommon
	jp nc, .DrawOnly

	ld hl, actor_timer
	add hl, de
	ld a, [hl]
	inc [hl]
	and 31
	jr nz, .noChangeDirection
		ld hl, actor_pxh
		add hl, de
		ldh a, [PlayerPXH]
		sub [hl]
		jr nc, :+
			cpl
			inc a
		:
		cp 6
		jr nc, .TooFarToChase

		switch_hl_to_field actor_pxh, actor_pyh
		ldh a, [PlayerPYH]
		sub [hl]
		jr nc, :+
			cpl
			inc a
		:
		cp 6
		jr nc, .TooFarToChase
			call RandomByte
			and 7
			cp 2
			jr c, .TooFarToChase ; Do a random direction sometimes anyway

			call ActorGetAngleTowardPlayer
			ld b, a
			call RandomByte
			and 15
			sub 7
			add b
			rrca
			rrca
			and 63

			ld hl, .kittyCosine
			add_hl_a
			ld b, [hl]
			ld a, 64
			add_hl_a
			ld c, [hl]

			ld hl, actor_vxl
			add hl, de
			ld [hl], b
			switch_hl_to_field actor_vxl, actor_vyl
			ld [hl], c
			jr .noChangeDirection
		.TooFarToChase:

		call RandomByte
		and 15
		call MaybeNegative
		ld hl, actor_vxl
		add hl, de
		ld [hl], a

		call RandomByte
		and 15
		call MaybeNegative
		ld hl, actor_vyl
		add hl, de
		ld [hl], a
	.noChangeDirection:

	; Apply VXL and VYL
	ld hl, actor_vxl
	add hl, de

	ld a, [hl]
	and 128   ; Get VXL sign
	ld b, a

	ld a, [de]
	and 127
	or b      ; Copy to direction bit
	ld [de], a

	push hl
	call ActorGetTileUnderneath
	pop hl
	cp BlockType_Paint
	push af ; Save the Z flag
	jr nz, :+
		ld a, [hl]
		sra a
		jr .SlowX
	:

	ld a, [hl] ; Get VXL again
	.SlowX:
	push hl
	call ActorWalkXAndBump
	pop hl
	jr nc, :+
		; Negate VXL
		ld a, [hl]
		cpl
		inc a
		ld [hl], a
	:

	ld hl, actor_vyl
	add hl, de

	pop af ; Z flag from "cp BlockType_Paint"
	jr nz, :+
		ld a, [hl]
		sra a
		jr .SlowY
	:

	ld a, [hl]
	.SlowY:
	push hl
	call ActorWalkYAndBump
	pop hl
	jr nc, :+
		; Negate VYL
		ld a, [hl]
		cpl
		inc a
		ld [hl], a		
	:

.DrawOnly:
	ldh a, [framecount]
	rrca
	rrca
	and %100
	add a, $08
	ld b, 0
	jp DrawEnemy_16x16_AndCollide

;import math
;x = []
;y = []
;for i in range(64):
;	angle = (i / 64) * 2 * math.pi
;	x.append(round(math.cos(angle) * 14))
;	y.append(round(math.sin(angle) * 14))
;print(', '.join([str(x) for x in x]))
;print(', '.join([str(x) for x in y]))

.kittyCosine:
;	db 12, 12, 12, 11, 11, 11, 10, 9, 8, 8, 7, 6, 5, 3, 2, 1, 0, -1, -2, -3, -5, -6, -7, -8, -8, -9, -10, -11, -11, -11, -12, -12, -12, -12, -12, -11, -11, -11, -10, -9, -8, -8, -7, -6, -5, -3, -2, -1, 0, 1, 2, 3, 5, 6, 7, 8, 8, 9, 10, 11, 11, 11, 12, 12
	db 14, 14, 14, 13, 13, 12, 12, 11, 10, 9, 8, 7, 5, 4, 3, 1, 0, -1, -3, -4, -5, -7, -8, -9, -10, -11, -12, -12, -13, -13, -14, -14, -14, -14, -14, -13, -13, -12, -12, -11, -10, -9, -8, -7, -5, -4, -3, -1, 0, 1, 3, 4, 5, 7, 8, 9, 10, 11, 12, 12, 13, 13, 14, 14
.kittySine:
;	db 0, 1, 2, 3, 5, 6, 7, 8, 8, 9, 10, 11, 11, 11, 12, 12, 12, 12, 12, 11, 11, 11, 10, 9, 8, 8, 7, 6, 5, 3, 2, 1, 0, -1, -2, -3, -5, -6, -7, -8, -8, -9, -10, -11, -11, -11, -12, -12, -12, -12, -12, -11, -11, -11, -10, -9, -8, -8, -7, -6, -5, -3, -2, -1
	db 0, 1, 3, 4, 5, 7, 8, 9, 10, 11, 12, 12, 13, 13, 14, 14, 14, 14, 14, 13, 13, 12, 12, 11, 10, 9, 8, 7, 5, 4, 3, 1, 0, -1, -3, -4, -5, -7, -8, -9, -10, -11, -12, -12, -13, -13, -14, -14, -14, -14, -14, -13, -13, -12, -12, -11, -10, -9, -8, -7, -5, -4, -3, -1


ActorPaintProjectile::
	ld hl, actor_vxl
	add hl, de

	; Add X and Y speed to the position
	push de
	ld a, [hl+] ; actor_vxl
	ld e, a
	sex
	ld d, a

	ld a, [hl+] ; actor_vyl
	ld c, a
	sex
	ld b, a

	ld a, [hl] ; actor_pyl
	add c
	ld [hl+], a
	ld a, [hl] ; actor_pyh
	adc b
	ld [hl+], a
	ld a, [hl] ; actor_pxl
	add e
	ld [hl+], a
	ld a, [hl] ; actor_pxh
	adc d
	ld [hl+], a
	pop de

	ld hl, actor_timer
	add hl, de
	ld a, [hl]
	inc [hl]
	cp 24
	jr nz, :+
		; Paint the ground
		ld hl, actor_pxh
		add hl, de
		ld b, [hl]
		switch_hl_to_field actor_pxh, actor_pyl
		ld a, [hl+]
		sub PaintTerrainCollisionYOffset*16
		ld a, [hl]
		sbc 0
		ld h, a
		ld l, b
		call MapPointerLH_XY
		ld a, [hl]
		push hl
		get_block_flags
		pop hl
		and BLOCK_CLASS_MASK
		cp BlockClass_Paintable
		ld a, BlockType_Paint
		call z, BlockChangeForActor

		xor a
		ld [de], a
		ret
	:

	; Only draw the first of the five paint projectiles (the center)
	ld b, a
	ld hl, actor_var2
	add hl, de
	ld a, [hl]
	cp 5
	ret nz
	ld a, b

	ld hl, PaintOffset
	add_hl_a
	ld c, [hl]

	call RandomByte
	and OAMF_YFLIP|OAMF_XFLIP
	or SP_PALETTE_PLAYER
	ld b, a
	ld a, TILE_ID_PAINT_PROJECTILE
	jp DrawPaintProjectile

ActorHurtStarParticle::
	call ActorApplyVelocity

	ld hl, actor_timer
	add hl, de
	ld a, [hl]
	inc [hl]
	cp 24
	jr nz, :+
		xor a
		ld [de], a
		ret
	:
	ld hl, PaintOffset
	rst AddHL_A
	ld c, [hl]

	ld a, TILE_ID_HURT_STAR_PARTICLE
	ld b, 0
	jp DrawActor_8x16_YOffset

ActorPoof::
	ld hl, actor_timer
	add hl, de
	inc [hl]
	ld a, [hl]
	cp 4*3
	jr nz, :+
		xor a
		ld [de], a
		ret
	:

	ld a, [hl]
	and %1100
	add TILE_ID_POOF_ANIMATION
	ld b, 0
	jp DrawActor_16x16

ActorBurger::
	ld a, 0
	ld b, 0
	jp DrawActor_16x16

ActorEnemySpawning::
	ld hl, actor_timer
	add hl, de
	inc [hl]
	ld a, [hl]
	cp 60
	jr nz, .notReady
		; TODO: have an actual lookup here
		ld a, ActorType_Sneaker
		ld [de], a
		call RandomByte
		and 3
		jr nz, .No
		ld a, ActorType_Kitty
		ld [de], a
		.No:

		; Initialize the health
		ld a, [de]
		ld hl, ActorHealth
		rst AddHL_A
		ld a, [hl]
		ld hl, actor_health
		add hl, de
		ld [hl], a
	.notReady:

	ld a, TILE_ID_ENEMY_SPAWNING
	ld b, 0
	jp DrawActor_16x16_Symmetrical

ActorExitReady::
	ld a, TILE_ID_ARROW_DOWN
	ld b, 5 | OAMF_PAL1
	ld c, 4
	jp DrawActor_8x16_YOffset

ActorFollowingCritter::
	assert actor_state == actor_type + 1
	inc de
	ld a, [de]
	dec de
	; A = the state
	or a
	jp nz, .GoToExitState

	; Normal state
	ld hl, actor_var1
	add hl, de
	ldh a, [PlayerPYH]
	add [hl]
	ld b, a
	switch_hl_to_field actor_var1, actor_pyl

	ldh a, [PlayerPYL]
	sub [hl] ; actor_pyl
	ld c, a
	inc l

	ld a, b
	sbc [hl] ; actor_pyh
	dec l
	call .div_32

	add [hl]
	ld [hl+], a ; actor_pyl
	ld a, [hl]
	adc b
	ld [hl], a ; actor_pyh

	; -----

	switch_hl_to_field actor_pyh, actor_var2
	ldh a, [PlayerPXH]
	add [hl]
	ld b, a
	switch_hl_to_field actor_var2, actor_pxl

	ldh a, [PlayerPXL]
	sub [hl] ; actor_pxl
	ld c, a
	inc l

	ld a, b
	sbc [hl] ; actor_pxh
	dec l
	call .div_32

	add [hl]
	ld [hl+], a ; actor_pxl
	ld a, [hl]
	adc b
	ld [hl], a ; actor_pxh

	; -----

	; Move behind player
	switch_hl_to_field actor_pxh, actor_timer
	inc [hl]
	ld a, [hl]
	and 31
	jr nz, .NoReposition
;		call RandomByte
;		and %1110
		ld a, [PaintShootDirection]
		add a
		ld bc, .wander_table
		add_bc_a

		ld hl, actor_var1
		add hl, de
		ld a, [bc]
		ld [hl+], a
		inc bc
		ld a, [bc]
		ld [hl], a
	.NoReposition:

	; Flip to face the player
	ld hl, actor_pxh
	add hl, de
	ldh a, [PlayerPXH]
	cp [hl]
	jr z, .NoFlip
	jr nc, .Left
	.Right:
		ld a, [de]
		or 128
		ld [de], a
		jr .NoFlip
	.Left:
		ld a, [de]
		and 127
		ld [de], a
	.NoFlip:

.Draw:
	; Animate
	ld a, [de]
	and 128
	rrca
	rrca
	or 5 | OAMF_PAL1
	ld b, a

	; Hover back and forth
	ld hl, actor_timer
	add hl, de
	ld a, [hl]
	and 63
	ld hl, .hover_table
	rst AddHL_A
	ld c, [hl]

	ldh a, [framecount]
	rrca
	rrca
	rrca
	rrca
	and 2
	add TILE_ID_CRITTER

	jp DrawActor_8x16_YOffset

;l = []
;import math
;for i in range(64):
;	angle = (i / 64) * 2 * math.pi
;	r = math.sin(angle) * 5;
;	l.append(round(r) + 4)
;print(', '.join([str(x) for x in l]))
.hover_table:
	db 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 9, 8, 8, 8, 8, 7, 7, 6, 6, 5, 5, 4, 4, 4, 3, 3, 2, 2, 1, 1, 0, 0, 0, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4
.div_32:
	ld b, a
	ld a, c
	sra b
	rra
	sra b
	rra
	sra b
	rra
	sra b
	rra
	sra b
	rra
	ret
.wander_table:
	db  0, -1
	db -1, -1
	db -2,  0
	db -1,  1
	db  0,  1
	db  1,  1
	db  2,  0
	db  1, -1
.GoToExitState:
	ld hl, actor_pyl
	add hl, de

	ld a, $80
	sub [hl] ; actor_pyl
	ld c, a
	inc l

	ld a, [MazeExitY]
	sbc [hl] ; actor_pyh
	dec l
	call .div_32

	add [hl]
	ld [hl+], a ; actor_pyl
	ld a, [hl]
	adc b
	ld [hl], a ; actor_pyh

	; -----

	switch_hl_to_field actor_pyh, actor_var2
	ldh a, [PlayerPXH]
	add [hl]
	ld b, a
	switch_hl_to_field actor_var2, actor_pxl

	ld a, $80
	sub [hl] ; actor_pxl
	ld c, a
	inc l

	ld a, [MazeExitX]
	sbc [hl] ; actor_pxh
	dec l
	call .div_32

	add [hl]
	ld [hl+], a
	ld a, [hl] ; actor_pxl
	adc b
	ld [hl], a ; actor_pxh

	switch_hl_to_field actor_pxh, actor_timer
	inc [hl]

	; Increase a timer for a bit and then remove the actor
	switch_hl_to_field actor_timer, actor_var1
	inc [hl]
	ld a, [hl]
	cp 90
	jr c, .DontRemoveYet
		xor a
		ld [de], a

		ld a, [RescueCritterCount]
		or a
		jr nz, .DontRemoveYet
		ld a, ActorType_ExitReady
		ld [de], a

		ld hl, actor_pyl
		add hl, de
		ld a, $80
		put_hl_and_switch_to_field actor_pyl, actor_pyh
		ld a, [MazeExitY]
		put_hl_and_switch_to_field actor_pyh, actor_pxl
		ld a, $80
		put_hl_and_switch_to_field actor_pxl, actor_pxh
		ld a, [MazeExitX]
		ld [hl], a
	.DontRemoveYet:

	jp .Draw

;height = 0
;speed = -10
;while True:
;	print(height)
;	height += speed // 4
;	speed += 1
;	if height > 0:
;		break
PaintOffset:
def PaintTerrainCollisionYOffset equ 6
def PaintOffsetOffset equ 4
	db 0+PaintOffsetOffset, -3+PaintOffsetOffset, -6+PaintOffsetOffset, -8+PaintOffsetOffset, -10+PaintOffsetOffset, -12+PaintOffsetOffset, -14+PaintOffsetOffset, -15+PaintOffsetOffset, -16+PaintOffsetOffset, -17+PaintOffsetOffset, -18+PaintOffsetOffset, -18+PaintOffsetOffset, -18+PaintOffsetOffset, -18+PaintOffsetOffset, -18+PaintOffsetOffset, -17+PaintOffsetOffset, -16+PaintOffsetOffset, -15+PaintOffsetOffset, -14+PaintOffsetOffset, -12+PaintOffsetOffset, -10+PaintOffsetOffset, -8+PaintOffsetOffset, -6+PaintOffsetOffset, -3+PaintOffsetOffset, 0+PaintOffsetOffset

EnemyCommon:
	; Remove the enemy if they're too far off the side of the screen
	; Too far horizontally?
	ld hl, actor_pxh
	add hl, de
	ldh a, [PlayerPXH]
	ld b, a
	ld a, [hl-]
	sub b
	jr nc, :+
		cpl
		inc a
	:
	cp 12
	jr c, :+
	.TooFar:
		xor a
		ld [de], a
		pop hl
		ret
	:
	dec l
	ldh a, [PlayerPYH]
	ld b, a
	ld a, [hl]
	sub b
	jr nc, :+
		cpl
		inc a
	:
	cp 12
	jr nc, .TooFar

	; ----

	ld hl, EnemyCount
	inc [hl]

	; Apply knockback if there is currently knockback
	ld hl, actor_knockback_timer
	add hl, de
	ld a, [hl]
	or a
	jr z, .NoKnockback
		dec [hl]
		ld a, [hl+] ; HL: actor_knockback_timer
		ldh [temp1], a
		ld a, [hl+] ; HL: actor_knockback_sign_x
		ldh [temp2], a
		ld a, [hl]  ; HL: actor_knockback_sign_y

		or a
		jr z, .SkipKnockY
		cp -16
		jr z, .KnockNegativeY
		.KnockPositiveY:
			ldh a, [temp1]
			call ActorWalkYAndBump
			jr .DidKnockY
		.KnockNegativeY:
			ldh a, [temp1]
			cpl
			inc a
			call ActorWalkYAndBump
		.DidKnockY:
			jr nc, .SkipKnockY
				ld hl, actor_knockback_sign_y
				add hl, de
				ld a, [hl]   ; HL: actor_knockback_sign_y
				cpl
				inc a
				ld [hl-], a  ; HL: actor_knockback_sign_y
				dec l

				ld a, [hl]   ; HL: actor_knockback_timer
				cp 15
				jr c, :+
					ld [hl], 15 ; HL: actor_knockback_timer
				:
		.SkipKnockY:

		ldh a, [temp2]
		or a
		jr z, .SkipKnockX
		cp -16
		jr z, .KnockNegativeX
		.KnockPositiveX:
			ldh a, [temp1]
			call ActorWalkXAndBump
			jr .DidKnockX
		.KnockNegativeX:
			ldh a, [temp1]
			cpl
			inc a
			call ActorWalkXAndBump
		.DidKnockX:
			jr nc, .SkipKnockX
				ld hl, actor_knockback_sign_x
				add hl, de
				ld a, [hl]  ; HL: actor_knockback_sign_x
				cpl
				inc a
				ld [hl-], a ; HL: actor_knockback_sign_x

				ld a, [hl]  ; HL: actor_knockback_timer
				cp 15
				jr c, :+
					ld [hl], 15 ; HL: actor_knockback_timer
				:
		.SkipKnockX:

		or a ; Override normal behavior
		ret
	.NoKnockback:

	; Signal that the enemy code should continue as normal
	scf
	ret

;px = 0
;kx = 6
;L = []
;while True:
;	old_px = px
;	px += kx
;	int_old_px = round(old_px*16)
;	int_new_px = round(px*16)
;	L.append(int_new_px - int_old_px) 
;	kx *= 0.90
;	if kx < 0.05:
;		break
;L.reverse()
KnockbackTable:
	db 0, 1, 1, 1, 2, 1, 2, 2, 1, 3, 2, 3, 3, 3, 4, 4, 4, 5, 6, 6, 7, 8, 8, 10, 10, 12, 13, 14, 16, 18, 20, 22, 24, 27, 30, 34, 37, 41, 46, 51, 57, 63, 70, 78, 86, 96

; .----------------------------------------------------------------------------
; | Actor collision
; '----------------------------------------------------------------------------

CollideWithPlayer:
	ldh a, [PlayerCollisionX]
	ld b, a
	ldh a, [EnemyCollisionX]
	scf
	sbc b ; Note will subtract n-1
	sbc PLAYER_COLLISION_WIDTH-1
	ccf
	adc ENEMY_COLLISION_WIDTH+PLAYER_COLLISION_WIDTH-1 ; Carry set if overlap
	ret nc

	ldh a, [PlayerCollisionY]
	ld b, a
	ldh a, [EnemyCollisionY]
	scf
	sbc b ; Note will subtract n-1
	sbc PLAYER_COLLISION_HEIGHT-1
	ccf
	adc ENEMY_COLLISION_HEIGHT+PLAYER_COLLISION_HEIGHT-1 ; Carry set if overlap
	ret

CollideWithRolling:
	ldh a, [HoldingPaintButton]
	cp TIME_NEEDED_TO_ROLL
	ccf
	ret nc
	ld a, [PaintRefillCooldown]
	or a
	ret z ; "or a" will always clear carry

	ldh a, [RollingCollisionX]
	ld b, a
	ldh a, [EnemyCollisionX]
	scf
	sbc b ; Note will subtract n-1
	sbc ROLLING_COLLISION_WIDTH-1
	ccf
	adc ENEMY_COLLISION_WIDTH+10-1 ; Carry set if overlap
	ret nc

	ldh a, [RollingCollisionY]
	ld b, a
	ldh a, [EnemyCollisionY]
	scf
	sbc b ; Note will subtract n-1
	sbc ROLLING_COLLISION_HEIGHT-1
	ccf
	adc ENEMY_COLLISION_HEIGHT+10-1 ; Carry set if overlap
	ret

CollideWithProjectiles:
	; Quickly exit if there are no projectiles
	ld a, [PlayerProjectiles_type]
	or a
	ret z ; "or a" will have cleared carry

	; Quickly exit if the actor has already been attacked by this wave of particles
	ld a, [PaintShotID]
	ld hl, actor_damaged_by_id
	add hl, de
	cp [hl]
	ret z ; If Z is set, carry will not be set

	ld a, [PaintShootDirectionLock]
	and 3
	; 0=vertical, 1=/, 2=horizontal, 3=\
	jp z, CollideWithVerticalWave
	dec a
	jp z, CollideWithDiagonalWave1
	dec a
	jp z, CollideWithHorizontalWave

DEF ProjectileEnemyCollisionYOffset EQU 7

CollideWithDiagonalWave2: ;\
	; Quick rejection
	ld a, [PlayerProjectiles_pxh]
	ld hl, actor_pxh
	add hl, de
	sub [hl]
	jr nc, :+
		cpl
		inc a
	:
	cp 3
	ret nc
	; ---
	switch_hl_to_field actor_pxh, actor_pyh
	ld a, [PlayerProjectiles_pyh]
	sub [hl]
	jr nc, :+
		cpl
		inc a
	:
	cp 3
	ret nc
	; ---------------------------------

	; Collision 1
	ldh a, [ProjectileCollisionX]
	sub 9
	ld b, a
	ldh a, [EnemyCollisionX]
	scf
	sbc b ; Note will subtract n-1
	sbc 10-1
	ccf
	adc ENEMY_COLLISION_WIDTH+10-1 ; Carry set if overlap
	jr nc, .try2

	ldh a, [ProjectileCollisionY]
	sub 9-ProjectileEnemyCollisionYOffset
	ld b, a
	ldh a, [EnemyCollisionY]
	scf
	sbc b ; Note will subtract n-1
	sbc 10-1
	ccf
	adc ENEMY_COLLISION_HEIGHT+10-1 ; Carry set if overlap
	ret c
.try2:
	; Collision 2
	ldh a, [ProjectileCollisionX]
	sub 3
	ld b, a
	ldh a, [EnemyCollisionX]
	scf
	sbc b ; Note will subtract n-1
	sbc 16-1
	ccf
	adc ENEMY_COLLISION_WIDTH+16-1 ; Carry set if overlap
	jr nc, .try3

	ldh a, [ProjectileCollisionY]
	add ProjectileEnemyCollisionYOffset
	ld b, a
	ldh a, [EnemyCollisionY]
	scf
	sbc b ; Note will subtract n-1
	sbc 16-1
	ccf
	adc ENEMY_COLLISION_HEIGHT+16-1 ; Carry set if overlap
	ret c
.try3:
	; Collision 3
	ldh a, [ProjectileCollisionX]
	add 8
	ld b, a
	ldh a, [EnemyCollisionX]
	scf
	sbc b ; Note will subtract n-1
	sbc 10-1
	ccf
	adc ENEMY_COLLISION_WIDTH+10-1 ; Carry set if overlap
	ret nc

	ldh a, [ProjectileCollisionY]
	add 12+ProjectileEnemyCollisionYOffset
	ld b, a
	ldh a, [EnemyCollisionY]
	scf
	sbc b ; Note will subtract n-1
	sbc 10-1
	ccf
	adc ENEMY_COLLISION_HEIGHT+10-1 ; Carry set if overlap
	ret

CollideWithDiagonalWave1: ;/
	; Quick rejection
	ld a, [PlayerProjectiles_pxh]
	ld hl, actor_pxh
	add hl, de
	sub [hl]
	jr nc, :+
		cpl
		inc a
	:
	cp 3
	ret nc
	; ---
	switch_hl_to_field actor_pxh, actor_pyh
	ld a, [PlayerProjectiles_pyh]
	sub [hl]
	jr nc, :+
		cpl
		inc a
	:
	cp 3
	ret nc
	; ---------------------------------

	; Collision 1
	ldh a, [ProjectileCollisionX]
	sub 9
	ld b, a
	ldh a, [EnemyCollisionX]
	scf
	sbc b ; Note will subtract n-1
	sbc 10-1
	ccf
	adc ENEMY_COLLISION_WIDTH+10-1 ; Carry set if overlap
	jr nc, .try2

	ldh a, [ProjectileCollisionY]
	add 12+ProjectileEnemyCollisionYOffset
	ld b, a
	ldh a, [EnemyCollisionY]
	scf
	sbc b ; Note will subtract n-1
	sbc 10-1
	ccf
	adc ENEMY_COLLISION_HEIGHT+10-1 ; Carry set if overlap
	ret c
.try2:
	; Collision 2
	ldh a, [ProjectileCollisionX]
	sub 3
	ld b, a
	ldh a, [EnemyCollisionX]
	scf
	sbc b ; Note will subtract n-1
	sbc 16-1
	ccf
	adc ENEMY_COLLISION_WIDTH+16-1 ; Carry set if overlap
	jr nc, .try3

	ldh a, [ProjectileCollisionY]
	add ProjectileEnemyCollisionYOffset
	ld b, a
	ldh a, [EnemyCollisionY]
	scf
	sbc b ; Note will subtract n-1
	sbc 16-1
	ccf
	adc ENEMY_COLLISION_HEIGHT+16-1 ; Carry set if overlap
	ret c
.try3:
	; Collision 3
	ldh a, [ProjectileCollisionX]
	add 8
	ld b, a
	ldh a, [EnemyCollisionX]
	scf
	sbc b ; Note will subtract n-1
	sbc 10-1
	ccf
	adc ENEMY_COLLISION_WIDTH+10-1 ; Carry set if overlap
	ret nc

	ldh a, [ProjectileCollisionY]
	sub 9-ProjectileEnemyCollisionYOffset
	ld b, a
	ldh a, [EnemyCollisionY]
	scf
	sbc b ; Note will subtract n-1
	sbc 10-1
	ccf
	adc ENEMY_COLLISION_HEIGHT+10-1 ; Carry set if overlap
	ret

CollideWithVerticalWave:
	ldh a, [ProjectileCollisionX]
	sub 4-(PROJECTILE_COLLISION_WIDTH/2)
	ld b, a
	ldh a, [EnemyCollisionX]
	scf
	sbc b ; Note will subtract n-1
	sbc PROJECTILE_COLLISION_WIDTH-1
	ccf
	adc ENEMY_COLLISION_WIDTH+PROJECTILE_COLLISION_WIDTH-1 ; Carry set if overlap
	ret nc

	ldh a, [ProjectileCollisionY]
	sub 12-ProjectileEnemyCollisionYOffset
	ld b, a
	ldh a, [EnemyCollisionY]
	scf
	sbc b ; Note will subtract n-1
	sbc 40-1
	ccf
	adc ENEMY_COLLISION_HEIGHT+40-1 ; Carry set if overlap
	ret

CollideWithHorizontalWave:
	ldh a, [ProjectileCollisionX]
	sub 16
	ld b, a
	ldh a, [EnemyCollisionX]
	scf
	sbc b ; Note will subtract n-1
	sbc 40-1
	ccf
	adc ENEMY_COLLISION_WIDTH+40-1 ; Carry set if overlap
	ret nc

	ldh a, [ProjectileCollisionY]
	add 8-(PROJECTILE_COLLISION_HEIGHT/2)+ProjectileEnemyCollisionYOffset
	ld b, a
	ldh a, [EnemyCollisionY]
	scf
	sbc b ; Note will subtract n-1
	sbc PROJECTILE_COLLISION_HEIGHT-1
	ccf
	adc ENEMY_COLLISION_HEIGHT+PROJECTILE_COLLISION_HEIGHT-1 ; Carry set if overlap
	ret

; .----------------------------------------------------------------------------
; | Actor shared code
; '----------------------------------------------------------------------------

ActorApplyVelocity:
	ld hl, actor_vxl
	add hl, de
	ld a, [hl+] ; HL: actor_vxl
	ld b, a
	sex
	ldh [temp1], a
	ld a, [hl+] ; HL: actor_vyl
	ld c, a
	sex
	ldh [temp2], a

	ld a, [hl]  ; HL: actor_pyl
	add b
	ld [hl+], a
	ldh a, [temp1]
	adc [hl]    ; HL: actor_pyh
	ld [hl+], a

	ld a, [hl]  ; HL: actor_pxl
	add c
	ld [hl+], a
	ldh a, [temp2]
	adc [hl]    ; HL: actor_pxh
	ld [hl], a
	ret

ActorBecomePoof:
	ld a, ActorType_Poof
	ld [de], a ; actor_type
	inc e
	inc e
	xor a
	ld [de], a ; actor_timer
	dec e
	dec e
	ret

ActorWalkY:
	ld c, a
	sex
	ld b, a

	ld hl, actor_pyl
.add:
	add hl, de
	ld a, [hl]
	add c
	ld [hl+], a
	ld a, [hl]
	adc b
	ld [hl], a
	ret

ActorWalkX:
	ld c, a
	sex
	ld b, a

	ld hl, actor_pxl
	jr ActorWalkY.add

ActorWalkYAndBump:
	ld c, a
	sex
	ld b, a

	ld hl, actor_pyl
	add hl, de
	ld a, [hl+] ; HL: PYL --> PYH
	add c
	ld a, [hl+] ; HL: PYH --> PXL
	adc b
	inc l       ; HL: PXL --> PXH
	ld l, [hl]
	ld h, a
	call MapFlagsLH_XY
	rla
	ret c

	ld hl, actor_pyl
	add hl, de
	ld a, [hl]
	add c
	ld [hl+], a
	ld a, [hl]
	adc b
	ld [hl], a
	or a
	ret

ActorWalkXAndBump:
	ld c, a
	sex
	ld b, a

	ld hl, actor_pxl
	add hl, de
	ld a, [hl+] ; HL: PXL --> PXH
	add c
	ld a, [hl-] ; HL: PXH --> PXL
	adc b
	dec l       ; HL: PXL --> PYH
	ld h, [hl]
	ld l, a
	call MapFlagsLH_XY
	rla
	ret c

	ld hl, actor_pxl
	add hl, de
	ld a, [hl]
	add c
	ld [hl+], a
	ld a, [hl]
	adc b
	ld [hl], a
	or a
	ret

; Input: A (Walk speed)
; Output: B (Bit field for wall bump directions; %YX)
ActorWalkTowardPlayerAndBump::
	ldh [temp1], a
	ld b, 0

	ldh a, [PlayerPYH]
	ld hl, actor_pyh
	add hl, de
	cp [hl]
	jr z, .EqualY
	jr nc, .GoDown
	jr .GoUp
	.EqualY:
		ldh a, [PlayerPYL]
		dec l
		cp [hl]
		jr z, .DoneMoveY
		jr nc, .GoDown
	.GoUp:
		ldh a, [temp1]
		cpl
		inc a
		call ActorWalkYAndBump
		jr .DoneMoveY
	.GoDown:
		ldh a, [temp1]
		call ActorWalkYAndBump
	.DoneMoveY:
	rl b

	ldh a, [PlayerPXH]
	ld hl, actor_pxh
	add hl, de
	cp [hl]
	jr z, .EqualX
	jr nc, .GoRight
	jr .GoLeft
	.EqualX:
		ldh a, [PlayerPXL]
		dec l
		cp [hl]
		jr z, .DoneMoveX
		jr nc, .GoRightNoFlip
		jr .GoLeftNoFlip
	.GoLeft:
		ld a, [de]
		or 128
		ld [de], a
	.GoLeftNoFlip:
		ldh a, [temp1]
		cpl
		inc a
		call ActorWalkXAndBump
		jr .DoneMoveX
	.GoRight:
		ld a, [de]
		and 127
		ld [de], a 
	.GoRightNoFlip:
		ldh a, [temp1]
		call ActorWalkXAndBump
	.DoneMoveX:
	rl b
	ret

ActorGetAngleTowardPlayer:
	ld hl, actor_pyl
	add hl, de
	ldh a, [PlayerPYL]
	sub [hl]
	ld c, a
	switch_hl_to_field actor_pyl, actor_pyh
	ldh a, [PlayerPYH]
	sbc [hl]
	ld b, a
	ld a, c
	rept 4
	sra b
	rra
	endr
	; D just holds HIGH(ActorData) so it's ok to overwrite it temporarily
	ld d, a

	switch_hl_to_field actor_pyh, actor_pxl
	ldh a, [PlayerPXL]
	sub [hl]
	ld c, a
	switch_hl_to_field actor_pxl, actor_pxh
	ldh a, [PlayerPXH]
	sbc [hl]
	ld b, a
	ld a, c
	rept 4
	sra b
	rra
	endr
	ld b, a ; X
	ld c, d ; Y
	ld d, HIGH(ActorData)

	jp atan2_bc_xy

ActorGetTileUnderneath:
	ld hl, actor_pyh
	add hl, de
	ld a, [hl+] ; HL = actor_pyh
	push af
	inc l       ; HL = actor_pxl
	ld l, [hl]  ; HL = actor_pxh
	pop af
	ld h, a

	call MapPointerLH_XY
	ld a, [hl]
	ret

; ---------------------------------------------------------

; A = First sprite tile to draw
; B = Attributes
; Draws actor DE
DrawActorWithoutFlip_16x16:
	; Shift B to have zeros for the flip bits
	sla b
DrawActorFlipped:
	ld c, a
	ld a, [de] ; save direction
	push af

	; Patch in direction bit
	add a
	srl b
	rra
	ld [de], a

	ld a, c
	call DrawActor_16x16

	pop af
	ld [de], a ; restore direction
	ret

; ---------------------------------------------------------

MACRO offset_tile_number_by_sprite_tileset_placement
	push af
	ld hl, ActorTileset
	ld a, [de]
	and 127 ; Mask off the direction bit
	add_hl_a
	ld a, [hl]
	ld hl, FirstTileNumberForActorTileset
	add_hl_a
	pop af
	add [hl]
ENDM

; A = First sprite tile to draw
; B = Attributes
; Draws actor DE
DrawActor_16x16:
	; temp1 = Sprite tile (left half)
	; temp2 = Sprite tile (right half)
	; temp3 = Attributes

	offset_tile_number_by_sprite_tileset_placement

	; Store the tile numbers and attributes to use
	ldh [temp1], a
	add 2
	ldh [temp2], a
	ld a, b
	ldh [temp3], a ; attributes

	; Flip if required
	ld a, [de] ; Get direction bit
	rla
	jr nc, .no_horiz_flip
	ldh a, [temp1]
	ldh [temp2], a
	add 2
	ldh [temp1], a
	ld a, b
	xor OAMF_XFLIP
	ldh [temp3], a
.no_horiz_flip:

;	ld hl, actor_state
;	add hl, de
;	ld a, [hl]
;	cp astate_stunned
;	jr nz, .no_vert_flip
;	ldh a, [temp3]
;	or OAM_YFLIP
;	ldh [temp3], a
;.no_vert_flip:

; --------------------------------
; Convert X and Y positions
	ld hl, actor_pyl
	add hl, de
	push de ; push "this" because DE will get used for screen coordinates

	; ---------------------------------
	; Get Y position first
	ldh a, [CameraY+0]
	ld c, a
	ldh a, [CameraY+1]
	ld b, a
	
	ld a, [hl+] ; HL: PYL --> PYH
	sub c
	ld c, a
	ld a, [hl+] ; HL: PYH --> PXL
	call SharedCameraSubtractCode_Bounded
	jr nc, .out_of_bounds
	;add 16-16
	ld e, a ; E = screen Y position
	add 15-(ENEMY_COLLISION_HEIGHT/2)
	ldh [EnemyCollisionY], a

	; Get X position next
	ldh a, [CameraX+0]
	ld c, a
	ldh a, [CameraX+1]
	ld b, a

	ld a, [hl+] ; HL: PXL --> PXH
	sub c
	ld c, a
	ld a, [hl]  ; HL: PXH
	call SharedCameraSubtractCode_Bounded
	jr c, :+
	.out_of_bounds:
		pop de
		or a ; Not drawn
		ret
	:
	;add 8-8
	ld d, a ; D = screen X position
	add 8-(ENEMY_COLLISION_WIDTH/2)
	ldh [EnemyCollisionX], a
	; ---------------------------------

	ldh a, [temp3]
	ld b, a ; B = attribute

	ld h, high(OamBuffer)
	ldh a, [OAMWrite]
	ld l, a
; --------------------------------

	ld a, e
	ld [hl+], a ; Y position
	ld a, d
	ld [hl+], a ; X position
	ldh a, [temp1]
	ld [hl+],a ; set tile number
	ld a, b
	ld [hl+],a ; set attribute

	ld a, e
	ld [hl+], a ; Y position
	ld a, d
	add 8
	ld [hl+], a ; X position
	ldh a, [temp2]
	ld [hl+],a ; set tile number
	ld a, b
	ld [hl+],a ; set attribute

; --------------------------------
	pop de ; restore "this"

	ld a, l
	ldh [OAMWrite], a
	scf
	ret

; ---------------------------------------------------------

DrawEnemy_16x16_AndCollide:
	call DrawActor_16x16
	ret nc

	; -----------------------------------------------------
	call CollideWithProjectiles
	jr nc, .NoCollide
	.DidCollideWithProjectile:
		ld hl, actor_health
		add hl, de
		ld a, [hl] ; HL: actor_health
		sub $08
		ld [hl+], a
		jr z, .OutOfHealth
		jr nc, :+
		.OutOfHealth:
			call ActorBecomePoof

			pop hl
			ret
		:
		ld hl, actor_damaged_by_id
		add hl, de
		ld a, [PaintShotID]
		ld [hl+], a ; HL: actor_damaged_by_id
		ld a, 45 ; Timer amount
		ld [hl+], a ; HL: actor_knockback_timer
		ld a, [PlayerProjectiles_vxl]
		ld [hl+], a ; HL: actor_knockback_sign_x
		ld a, [PlayerProjectiles_vyl]
		ld [hl],  a ; HL: actor_knockback_sign_y

		call CreateDamageParticleForActor
	.NoCollide:

	; -----------------------------------------------------
	ld hl, actor_knockback_timer
	add hl, de
	ld a, [hl]
	or a
	jr nz, .NoCollideWithRoll
	call CollideWithRolling
	jr nc, .NoCollideWithRoll
		ld hl, actor_health
		add hl, de
		ld a, [hl] ; HL: actor_health
		sub $08
		ld [hl+], a
		jr z, .OutOfHealthRoll
		jr nc, :+
		.OutOfHealthRoll:
			call ActorBecomePoof

			pop hl
			ret
		:
		ld hl, actor_knockback_timer
		add hl, de
		ld a, 45 ; Timer amount
		ld [hl+], a ; HL: actor_knockback_timer

		ld a, [PlayerDrawDirection]
		add a
		ld bc, KnockbackXYForRollDirections
		add_bc_a
		ld a, [bc]
		ld [hl+], a ; HL: actor_knockback_sign_x
		inc bc
		ld a, [bc]
		ld [hl],  a ; HL: actor_knockback_sign_y

		call CreateDamageParticleForActor
	.NoCollideWithRoll:

	; -----------------------------------------------------
	; Damage the player
	ld a, [PlayerInvincibleTimer]
	or a
	ret nz
	call CollideWithPlayer
	jp c, HurtPlayer
	ret

KnockbackXYForRollDirections:
	db 16, 0
	db 0, 16
	db -16, 0
	db 0, -16

CreateDamageParticleForActor:
	; Create damage particle
	ld hl, actor_pyl
	add hl, de
	ld a, [hl+] ; HL: actor_pyl
	ldh [temp1], a
	ld a, [hl+] ; HL: actor_pyh
	ldh [temp2], a
	ld a, [hl+] ; HL: actor_pxl
	ldh [temp3], a
	ld a, [hl+] ; HL: actor_pxh
	ldh [temp4], a
	call FindFreeActorSlot
	jr nc, .NoFreeActorSlot
		call ClearActorHL
		ld a, ActorType_HurtStarParticle
		ld [hl], a ; HL: actor_type

		switch_hl_to_field actor_type, actor_vxl
		call RandomByte
		and 31
		sub 16
		ld [hl+], a     ; actor_vxl
		call RandomByte
		and 31
		sub 16
		ld a, 1
		ld [hl+], a    ; actor_vyl
		ldh a, [temp1]
		ld [hl+], a    ; actor_pyl
		ldh a, [temp2]
		ld [hl+], a    ; actor_pyh
		ldh a, [temp3]
		ld [hl+], a    ; actor_pxl
		ldh a, [temp4]
		ld [hl], a     ; actor_pxh
	.NoFreeActorSlot:
	ret

; ---------------------------------------------------------

; A = Sprite tile to draw
; B = Attributes
; Draws actor DE
DrawActor_16x16_Symmetrical:
	; temp1 = Sprite tile (left half)
	; temp2 = Attributes

	; Store the tile numbers and attributes to use
	ldh [temp1], a
	ld a, b
	ldh [temp2], a ; attributes

; --------------------------------
; Convert X and Y positions
	ld hl, actor_pyl
	add hl, de
	push de ; push "this" because DE will get used for screen coordinates

	; ---------------------------------
	; Get Y position first
	ldh a, [CameraY+0]
	ld c, a
	ldh a, [CameraY+1]
	ld b, a
	
	ld a, [hl+] ; HL: PYL --> PYH
	sub c
	ld c, a
	ld a, [hl+] ; HL: PYH --> PXL
	call SharedCameraSubtractCode_Bounded
	jr nc, .out_of_bounds
	;add 16-16
	ld e, a ; E = screen Y position

	; Get X position next
	ldh a, [CameraX+0]
	ld c, a
	ldh a, [CameraX+1]
	ld b, a

	ld a, [hl+] ; HL: PXL --> PXH
	sub c
	ld c, a
	ld a, [hl]  ; HL: PXH
	call SharedCameraSubtractCode_Bounded
	jr c, :+
	.out_of_bounds:
		pop de
		ret
	:
	;add 8-8
	ld d, a ; D = screen X position
	; ---------------------------------

	ld h, high(OamBuffer)
	ldh a, [OAMWrite]
	ld l, a
	; --------------------------------

	ld a, e
	ld [hl+], a ; Y position
	ld a, d
	ld [hl+], a ; X position
	ldh a, [temp1]
	ld [hl+],a ; set tile number
	ldh a, [temp2]
	ld [hl+],a ; set attribute

	ld a, e
	ld [hl+], a ; Y position
	ld a, d
	add 8
	ld [hl+], a ; X position
	ldh a, [temp1]
	ld [hl+],a ; set tile number
	ldh a, [temp2]
	or OAMF_XFLIP
	ld [hl+],a ; set attribute

	; --------------------------------
	pop de ; restore "this"
	ld a, l
	ldh [OAMWrite], a
	ret

; ---------------------------------------------------------

; A = Sprite tile to draw
; B = Attributes
; C = Vertical offset
; Draws actor DE
DrawActor_8x16:
	ld c, 0
DrawActor_8x16_YOffset:
	; temp1 = Sprite tile
	; temp2 = Attributes
	; temp3 = Vertical offset

	; Store the tile numbers and attributes to use
	ldh [temp1], a
	ld a, b
	ldh [temp2], a ; Attributes
	ld a, c
	ldh [temp3], a ; Vertical offset

; --------------------------------
; Convert X and Y positions
	ld hl, actor_pyl
	add hl, de
	push de ; push "this" because DE will get used for screen coordinates

	; ---------------------------------
	; Get Y position first
	ldh a, [CameraY+0]
	ld c, a
	ldh a, [CameraY+1]
	ld b, a
	
	ld a, [hl+] ; HL: PYL --> PYH
	sub c
	ld c, a
	ld a, [hl+] ; HL: PYH --> PXL
	call SharedCameraSubtractCode_Bounded
	jr nc, .out_of_bounds
	;add 16-16
	ld e, a ; E = screen Y position
	ldh a, [temp3]
	add a, e
	ld e, a

	; Get X position next
	ldh a, [CameraX+0]
	ld c, a
	ldh a, [CameraX+1]
	ld b, a

	ld a, [hl+] ; HL: PXL --> PXH
	sub c
	ld c, a
	ld a, [hl]  ; HL: PXH
	call SharedCameraSubtractCode_Bounded
	jr c, :+
	.out_of_bounds:
		pop de
		ret
	:
	add 8-4
	ld d, a ; D = screen X position
	; ---------------------------------

	ld h, high(OamBuffer)
	ldh a, [OAMWrite]
	ld l, a

	ld a, e
	ld [hl+], a ; Y position
	ld a, d
	ld [hl+], a ; X position
	ldh a, [temp1]
	ld [hl+],a ; set tile number
	ldh a, [temp2]
	ld [hl+],a ; set attribute

	pop de ; restore "this"

	ld a, l
	ldh [OAMWrite], a
	ret


; A = Sprite tile to draw
; B = Attributes
; C = Vertical offset
; Draws paint projectile DE
DrawPaintProjectile:
	; temp1 = Sprite tile
	; temp2 = Attributes
	; temp3 = Vertical offset

	; Store the tile numbers and attributes to use
	ldh [temp1], a
	ld a, b
	ldh [temp2], a ; Attributes
	ld a, c
	ldh [temp3], a ; Vertical offset

; --------------------------------
; Convert X and Y positions
	ld hl, actor_pyl
	add hl, de
	push de ; push "this" because DE will get used for screen coordinates

	; ---------------------------------
	; Get Y position first
	ldh a, [CameraY+0]
	ld c, a
	ldh a, [CameraY+1]
	ld b, a
	
	ld a, [hl+] ; HL: PYL --> PYH
	sub c
	ld c, a
	ld a, [hl+] ; HL: PYH --> PXL
	call SharedCameraSubtractCode
	;add 16-16
	ld e, a ; E = screen Y position
	ldh [ProjectileCollisionY], a
	ldh a, [temp3] ; Y offset
	add a, e
	ld e, a

	; Get X position next
	ldh a, [CameraX+0]
	ld c, a
	ldh a, [CameraX+1]
	ld b, a

	ld a, [hl+] ; HL: PXL --> PXH
	sub c
	ld c, a
	ld a, [hl]  ; HL: PXH
	call SharedCameraSubtractCode
	add 8-4
	ld d, a ; D = screen X position
	ldh [ProjectileCollisionX], a

	; ---------------------------------
	; Use the jump table to determine which offsets to use for drawing all 5 projectile sprites

	ld hl, DrawPaintLineTable
	ld a, [PaintShootDirectionLock]
	add a
	add_hl_a
	ld a, [hl+]
	ld h, [hl]
	ld l, a
	push hl

	ld h, high(OamBuffer)
	ldh a, [OAMWrite]
	ld l, a

	ldh a, [temp1]
	ld b, a
	ldh a, [temp2]
	ld c, a
	ret


; Input: B (camera position high byte), A (entity position high byte),
;        C (entity position low byte; should already have subtracted camera position low byte)
; Output: A (pixel position), Carry set if within bounds & clear if outside bounds
SharedCameraSubtractCode_Bounded::
	sbc b
	cp $ff ; -1 is specifically allowed
	jr z, .ok
	cp 11  ; The screen is 160 pixels wide, which is 10 tiles. If it's 11 tiles to the right, it's definitely off-screen.
	       ; This check is also used vertically, and it's ok to allow some extra space on the bottom.
	ret nc ; Returning with carry clear
.ok:
	; Position is within bounds, so divide it by 16
	ld b, a

	; BC = entity position - camera
	ld a, c
	rept 4
		sra b
		rra
	endr
	adc 0 ; Rounding
	; A = low half of BC>>4
	scf ; Set carry
	ret
