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
		ld a, -$08
		call ActorWalkYAndBump
		jr .DoneMoveY
	.GoDown:
		ld a, $08
		call ActorWalkYAndBump
	.DoneMoveY:

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
		ld a, -$08
		call ActorWalkXAndBump
		jr .DoneMoveX
	.GoRight:
		ld a, [de]
		and 127
		ld [de], a 
	.GoRightNoFlip:
		ld a, $08
		call ActorWalkXAndBump
	.DoneMoveX:

.DrawOnly:
	;ld a, $34
	ldh a, [framecount]
	rrca
	and %100
	add a, $50
	ld b, 0
	jp DrawActor_16x16

ActorKitty::
	call EnemyCommon
	ldh a, [framecount]
	rrca
	and %100
	add a, $58
	ld b, 0
	jp DrawActor_16x16

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
		sub PaintCollisionYOffset*16
		ld a, [hl]
		sbc 0
		ld h, a
		ld l, b
		call MapPointerLH_XY
		ld a, [hl]
		cp BlockType_Floor
		ld a, BlockType_Paint
		call z, BlockChangeForActor

		xor a
		ld [de], a
		ret
	:

	ld hl, PaintOffset
	add_hl_a
	ld c, [hl]

	call RandomByte
	and OAMF_YFLIP|OAMF_XFLIP
	or PALETTE_PLAYER
	ld b, a
	ld a, $2C
	jp DrawActor_8x16_YOffset

ActorHurtStarProjectile::
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
	add_hl_a
	ld c, [hl]

	ld a, $22
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
	add $34
	ld b, 0
	jp DrawActor_16x16

ActorEnemySpawning::
	ld hl, actor_timer
	add hl, de
	inc [hl]
	ld a, [hl]
	cp 60
	jr nz, :+
		ld a, ActorType_Sneaker
		ld [de], a
		ret
	:

	ld a, $1E
	ld b, 0
	jp DrawActor_16x16_Symmetrical

;height = 0
;speed = -10
;while True:
;	print(height)
;	height += speed // 4
;	speed += 1
;	if height > 0:
;		break
PaintOffset:
def PaintCollisionYOffset equ 6
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
	add a ; Check the sign bit
	jr nc, :+
		cpl
		inc a
	:
	cp 24 ; 12, but doubled due to the "add a" sign check
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
	add a ; Check the sign bit
	jr nc, :+
		cpl
		inc a
	:
	cp 24 ; 12, but doubled due to the "add a" sign check
	jr nc, .TooFar

	; ----

	ld hl, EnemyCount
	inc [hl]

	call CollideWithProjectiles
	jr nc, .NoCollide
		switch_hl_to_field actor_type, actor_vxl
		ld a, [hl+]
		ld b, a ; B = X Speed
		ld a, [hl]
		ld c, a ; C = Y Speed
		switch_hl_to_field actor_vyl, actor_var1
		ld a, [hl]
		ldh [temp1], a

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
		ldh a, [temp1]
		ld [hl+], a ; HL: actor_damaged_by_id
		ld a, 45 ; Timer amount
		ld [hl+], a ; HL: actor_knockback_timer
		ld a, b
		ld [hl+], a ; HL: actor_knockback_sign_x
		ld a, c
		ld [hl],  a ; HL: actor_knockback_sign_y

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
			ld a, ActorType_HurtStarProjectile
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
	.NoCollide:

	call CollideWithPlayer

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

CollideWithPlayer:
	ld hl, actor_pyh
	add hl, de

	; Attempt to exit early
	ldh a, [PlayerPYH]
	sub [hl] ; actor_pyh
	add a    ; Check to see if it's negative
	jr nc, :+
		cpl
		inc a
	:
	cp 4 ; Actually 2, doubled because of the "add a"
	ret nc
	switch_hl_to_field actor_pyh, actor_pxh
	ldh a, [PlayerPXH]
	sub [hl] ; actor_pxh
	add a    ; Check to see if it's negative
	jr nc, :+
		cpl
		inc a
	:
	cp 4 ; Actually 2, doubled because of the "add a"
	ret nc

	; .------------------------------------------------
	; | Check X coordinates
	; '------------------------------------------------

	; HL = actor_pxh
	switch_hl_to_field actor_pxh, actor_pxl
	ldh a, [PlayerPXL] ; pxl
	ld c, a
	ld a, [hl+] ; HL: actor_pxl
	sub c
	ld c, a
	ldh a, [PlayerPXH] ; pxh
	ld b, a
	ld a, [hl-] ; HL: actor_pxh
	sbc b
	ld b, a

	sla c ; BC *= 2
	rl b
	jr nc, :+ ; Flip the sign if it's negative
		xor a
		sub c
		ld c, a
		ld a, 0
		sbc b
		ld b, a
	:

	; BC should be < (width1+width2)
	; so BC < ((8*16)+(8*16)) or 256
	; so B == 0
	ld a, b
	or a
	jr nz, .NoCollision
	bit 7, c
	jr nz, .NoCollision

	; .------------------------------------------------
	; | Check Y coordinates
	; '------------------------------------------------

	switch_hl_to_field actor_pxl, actor_pyl

	ldh a, [PlayerPYL] ; pyl
	ld c, a
	ld a, [hl+] ; HL: actor_pyl
	sub c
	ld c, a
	ldh a, [PlayerPYH] ; pyh
	ld b, a
	ld a, [hl+] ; HL: actor_pyh
	sbc b
	ld b, a

	sla c ; BC *= 2
	rl b
	jr nc, :+ ; Flip the sign if it's negative
		xor a
		sub c
		ld c, a
		ld a, 0
		sbc b
		ld b, a
	:

	; BC should be < (height1+height2)
	; so BC < ((8*16)+(8*16)) or 256
	; so B == 0
	ld a, b
	or a
	ret nz

	ld b,b
	scf
	ret
.NoCollision:
	or a
	ret

CollideWithProjectiles:
	; Check for collision with player projectiles
	ld hl, actor_pyl
	add hl, de
	ld a, [hl+]
	add PaintCollisionYOffset*16 ; <-- Move the actor's temporary position down to match the way paint projectiles subtract a bit
	ldh [temp1], a ; pyl
	ld a, [hl+]
	adc 0
	ldh [temp2], a ; pyh
	ld a, [hl+]
	ldh [temp3], a ; pxl
	ld a, [hl+]
	ldh [temp4], a ; pxh
	ld hl, actor_damaged_by_id
	add hl, de
	ld a, [hl]
	ldh [temp5], a ; damaged by ID

	push de
	ld hl, PlayerProjectiles + ACTOR_SIZE * (PLAYER_PROJECTILE_COUNT-1)
.CollisionLoop:
	ld a, [hl]
	cp ActorType_PaintProjectile
	jr nz, .Next
		push hl
		; Actor's actor_damaged_by_id can't equal the paint shot's var1 (which is the paint shot ID)
		switch_hl_to_field actor_type, actor_var1
		ldh a, [temp5]
		cp [hl]
		jr z, .PopNext

		; Try to exit early; hopefully this works?
		switch_hl_to_field actor_var1, actor_pyh
		ldh a, [temp2]
		sub [hl] ; actor_pyh
		add a    ; Check to see if it's negative
		jr nc, :+
			cpl
			inc a
		:
		cp 4 ; Actually 2, doubled because of the "add a"
		jr nc, .PopNext
		inc l
		inc l
		ldh a, [temp4]
		sub [hl] ; actor_pxh
		add a    ; Check to see if it's negative
		jr nc, :+
			cpl
			inc a
		:
		cp 4 ; Actually 2, doubled because of the "add a"
		jr nc, .PopNext

		; .------------------------------------------------
		; | Check Y coordinates
		; '------------------------------------------------

		switch_hl_to_field actor_pxh, actor_pyl
	
		ldh a, [temp1] ; pyl
		ld c, a
		ld a, [hl+] ; HL: actor_pyl
		sub c
		ld c, a
		ldh a, [temp2] ; pyh
		ld b, a
		ld a, [hl+] ; HL: actor_pyh
		sbc b
		ld b, a

		sla c ; BC *= 2
		rl b
		jr nc, :+ ; Flip the sign if it's negative
			xor a
			sub c
			ld c, a
			ld a, 0
			sbc b
			ld b, a
		:

		; BC should be < (height1+height2)
		; so BC < ((12*16)+(4*16)) or 256
		; so B == 0 is enough
		ld a, b
		or a
		jr nz, .PopNext

		; .------------------------------------------------
		; | Check X coordinates
		; '------------------------------------------------

		; HL = actor_pyh
		ldh a, [temp3] ; pxl
		ld c, a
		ld a, [hl+] ; HL: actor_pxl
		sub c
		ld c, a
		ldh a, [temp4] ; pxh
		ld b, a
		ld a, [hl+] ; HL: actor_pxh
		sbc b
		ld b, a

		sla c ; BC *= 2
		rl b
		jr nc, :+ ; Flip the sign if it's negative
			xor a
			sub c
			ld c, a
			ld a, 0
			sbc b
			ld b, a
		:

		; BC should be < (width1+width2)
		; so BC < ((16*16)+(8*16)) or 384 or $180
		; so B == 0 and C >= $80 is enough
		ld a, b
		or a
		jr nz, .PopNext
		bit 7, c
		jr nz, .PopNext

		; -------------------------------------------------
		; There is a collision!!
		pop hl ; HL = projectile that was collided with
		pop de ; DE = this, still
		scf    ; True
		ret

		.PopNext:
		pop hl
	.Next:
	ld a, l
	sub ACTOR_SIZE
	ld l, a
	jr nc, .CollisionLoop
.NoCollision:
	pop de
	or a ; Clear carry
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

; A = First sprite tile to draw
; B = Attributes
; Draws actor DE
DrawActor_16x16:
	; temp1 = Sprite tile (left half)
	; temp2 = Sprite tile (right half)
	; temp3 = Attributes

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
	hswap [temp1], [temp2]
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
