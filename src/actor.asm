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

SECTION "ActorCode", ROMX

; .----------------------------------------------------------------------------
; | Actor common code
; '----------------------------------------------------------------------------

; Run through all of the actors
RunActors::
	; Reset the enemy count so we can start counting up from zero
	xor a
	ld [EnemyCount], a

	ld d, ActorData>>8
	ld e, 0
.loop:
	ld a, [de]
	or a
	call nz, .call

	ld a, e
	add ACTOR_SIZE
	ret z
	ld e, a
	jr .loop

.call:
	add a ; multiply by 2, mask off direction bit
	ld h, ActorPointers>>8
	ld l, a

	ld a, [hl+]
	ld h, [hl]
	ld l, a
	jp hl

; .----------------------------------------------------------------------------
; | Per-actor code
; | Called with DE = actor pointer
; '----------------------------------------------------------------------------

ActorNone:
	ret

ActorSneaker:
	ld a, $34
	ld b, 0
	call DrawActor_16x16
	ret

; .----------------------------------------------------------------------------
; | Actor shared code
; '----------------------------------------------------------------------------

; A = First sprite tile to draw
; B = Attributes
; Draws actor DE
DrawActorWithoutFlip:
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
	or OAMF_XFLIP
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
	ldh a, [OamWrite]
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
skipdraw:

; --------------------------------
	pop de ; restore "this"

	ld a, l
	ldh [OamWrite], a
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


; .----------------------------------------------------------------------------
; | Actor code pointers
; '----------------------------------------------------------------------------
SECTION "actor_pointers", ROM0,ALIGN[8]
ActorPointers:
  dw ActorNone
  dw ActorSneaker
