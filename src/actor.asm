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
		jr nc, .GoRight
	.GoLeft:
		ld a, [de]
		or 128
		ld [de], a
		ld a, -$08
		call ActorWalkXAndBump
		jr .DoneMoveX
	.GoRight:
		ld a, [de]
		and 127
		ld [de], a 
		ld a, $08
		call ActorWalkXAndBump
	.DoneMoveX:

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
	ld hl, actor_vyl
	add hl, de

	; Add X and Y speed to the position
	push de
	ld a, [hl+] ; Y
	ld c, a
	sex
	ld b, a

	ld a, [hl+] ; X
	ld e, a
	sex
	ld d, a

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
	call DrawActor_8x16_YOffset
	ret


PaintOffset:
def PaintOffsetOffset equ 4

	db 0+PaintOffsetOffset, -3+PaintOffsetOffset, -6+PaintOffsetOffset, -8+PaintOffsetOffset, -10+PaintOffsetOffset, -12+PaintOffsetOffset, -14+PaintOffsetOffset, -15+PaintOffsetOffset, -16+PaintOffsetOffset, -17+PaintOffsetOffset, -18+PaintOffsetOffset, -18+PaintOffsetOffset, -18+PaintOffsetOffset, -18+PaintOffsetOffset, -18+PaintOffsetOffset, -17+PaintOffsetOffset, -16+PaintOffsetOffset, -15+PaintOffsetOffset, -14+PaintOffsetOffset, -12+PaintOffsetOffset, -10+PaintOffsetOffset, -8+PaintOffsetOffset, -6+PaintOffsetOffset, -3+PaintOffsetOffset, 0+PaintOffsetOffset

EnemyCommon:
	ld hl, EnemyCount
	inc [hl]
	ret

; .----------------------------------------------------------------------------
; | Actor shared code
; '----------------------------------------------------------------------------

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
