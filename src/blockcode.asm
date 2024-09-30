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
include "include/macros.inc"
include "include/defines.inc"
include "res/block_enum.inc"
include "res/actor_enum.inc"

SECTION "BlockCode", ROM0

RunBlockPrize::
RunBlockLock::
RunBlockBomb::
RunBlockFire::
RunBlockToggleButton::
RunBlockSpring::
RunBlockTeleporter::
	ret
RunBlockKey::
RunBlockDot::
	ld a, BlockType_Floor
	jp BlockChangeForPlayer

RunBlockHeart::
	ld a, 4
	ld [PlayerHealth], a

	; Refill the hearts in the status bar
	push hl
	ld hl, _SCRN1+14
	wait_vram
	ld a, $F8
	ld [hl+], a
	ld [hl+], a
	ld [hl+], a
	ld [hl+], a
	pop hl

	ld a, BlockType_Floor
	jp BlockChangeForPlayer

RunBlockStar::
	ld a, 255
	ld [PaintAmount], a

	ld a, BlockType_Floor
	jp BlockChangeForPlayer

RunRescueCritter::
	ld a, [HaveCritterActive]
	or a
	ret nz
	inc a
	ld [HaveCritterActive], a

	call MapPointerHL_To_XY_DE
	ld a, d
	ldh [temp1], a
	ld a, e
	ldh [temp2], a

	push hl
	call FindFreeActorSlot
	jr nc, .NoFreeActorSlot ; TODO: make room
		call ClearActorHL
		ld a, ActorType_FollowingCritter
		ld [hl], a ; HL: actor_type

		switch_hl_to_field actor_type, actor_pyl
		ld a, $80
		put_hl_and_switch_to_field actor_pyl, actor_pyh
		ldh a, [temp2]
		put_hl_and_switch_to_field actor_pyh, actor_pxl
		ld a, $80
		put_hl_and_switch_to_field actor_pxl, actor_pxh
		ldh a, [temp1]
		ld [hl], a
	.NoFreeActorSlot:
	pop hl

	ld a, BlockType_Floor
	jp BlockChangeForPlayer

RunBlockExit::
	ld a, [HaveCritterActive]
	or a
	ret z

	ld b, ActorType_FollowingCritter
	call FindFirstActorOfTypeB
	ret c

	switch_hl_to_field actor_type, actor_state
	ld a, [hl]
	or a
	ret nz ; Already 1
	ld [hl], 1 ; Change the state to something nonzero to signal it should go to the exit
	switch_hl_to_field actor_state, actor_var1
	ld [hl], 0 ; Will use this as a timer

	; Update status bar
	wait_vram
	ld a, [RescueCritterCount]
	dec a
	ld [RescueCritterCount], a
	add $f8
	ld [_SCRN1+3], a
	ret
