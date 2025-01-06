; Maffi cyberspace game
; Copyright (C) 2025 NovaSquirrel
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

SECTION "SpawnActor", ROMX

; B: X1
; C: X2+1
; D: Y1
; E: Y2+1
SpawnActorsWhileScrolling::
	ld hl, ActorsInLevelData_XY
.Loop:
	ld a, [hl+] ; X
	or a        ; A zero X position terminates the list
	ret z
	cp b
	jr c, .SkipY
	cp c
	jr nc, .SkipY
	ld a, [hl+] ; Y
	cp d
	jr c, .Loop
	cp e
	jr c, .Spawn
.SkipY:
	inc hl
	jr .Loop

.Spawn:
	push hl
	push bc
	push de
	dec hl ; HL is one byte past the Y position
	ld a, [hl-]
	ldh [temp2], a ; Y
	ld a, [hl]
	ldh [temp1], a ; X
	ld b, l ; Hold onto what L was at the start of the entry so it can be the ID
	ld de, MAX_ACTORS_IN_LEVEL_DATA*2 ; Get data from ActorsInLevelData_Type
	add hl, de
	ld a, [hl+] ; Type
	ldh [temp3], a
	ld c, [hl]  ; Variable
	; Now HL can be overwritten by other stuff

	; Make sure there is not already an actor with this ID
	ld hl, ActorData
.ScanForExistingAlready:
	ld a, [hl] ; A slot is allowed to have the ID if it's otherwise empty
	or a
	jr z, .Skip
	ASSERT actor_id_from_map == actor_type + 256
	inc h
	ld a, [hl]
	dec h
	cp b
	jr z, .NoSpawn ; Actor already exists
.Skip:
	ld a, l
	add ACTOR_SIZE
	ld l, a
	jr nz, .ScanForExistingAlready

	call FindFreeActorSlotImportant
	jr nc, .NoSpawn
	call ClearActorHL

	ldh a, [temp3] ; Type
	ld [hl], a
	switch_hl_to_field actor_type, actor_pyl
	ld [hl], $80   ; actor_pyl
	inc l
	ldh a, [temp2] ; actor_pyh
	ld [hl+], a
	ld [hl], $80   ; actor_pxl
	inc l
	ldh a, [temp1]
	ld [hl+], a    ; actor_pyh
	ld [hl], c     ; actor_var1

	ld de, actor_id_from_map - actor_var1
	add hl, de
	ld [hl], b     ; actor_id_from_map
.NoSpawn:
	pop de
	pop bc
	pop hl
	jr .Loop
