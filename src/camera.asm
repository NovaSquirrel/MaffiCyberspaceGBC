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
include "include/hardware.inc/hardware.inc"

SECTION "Camera", ROM0

; -----------------------------------------------------------------------------

; For comparison against the new values
def OldCameraX equs "temp1"
def OldCameraY equs "temp2"

; For telling the direction - it's the target-initial high byte
def CameraDXH equs "temp5"
def CameraDYH equs "temp6"

GetCameraTargetX:
	ldh a, [PlayerPXL]
	ld e, a
	ldh a, [PlayerPXH]
	sub 10/2
	ld b, 64-10
	; Fall through
CameraTargetClamp:
	; B = Target maximum
	jr nc, :+
		xor a
		ld e, a
	:
	cp b
	jr c, :+
		xor a
		ld e, a
		ld a, b
	:
	ld d, a
	ret

GetCameraTargetY:
	ldh a, [PlayerPYL]
	ld e, a
	ldh a, [PlayerPYH]
	sub 9/2
	ld b, 64-9
	jr CameraTargetClamp

InitCamera::
	call GetCameraTargetX
	ld a, e
	ldh [CameraX+0], a
	ld a, d
	ldh [CameraX+1], a

	call GetCameraTargetY
	ld a, e
	ldh [CameraY+0], a
	ld a, d
	ldh [CameraY+1], a

	jp CameraConvertXY

AdjustCameraSharedSubtractTarget:
	ld h, a
	; ---
	ld a, e
	sub l
	ld l, a
	ld a, d
	sbc h
	ld h, a
	ret

AdjustCamera::
	; Get scroll target
	call GetCameraTargetX

	; Find difference
	ldh a, [CameraX+0]
	ldh [OldCameraX], a
	ld l, a
	ldh a, [CameraX+1]
	call AdjustCameraSharedSubtractTarget
	ldh [CameraDXH], a

	; Divide by 4
	call DivideDifferenceForLerp

	; Move the camera toward target
	ldh a, [CameraX+0]
	add l
	ldh [CameraX+0], a
	ldh a, [CameraX+1]
	adc h
	ldh [CameraX+1], a

; ---------------------------------------

	; Get scroll target
	call GetCameraTargetY

	; Find difference
	ldh a, [CameraY+0]
	ldh [OldCameraY], a
	ld l, a
	ldh a, [CameraY+1]
	call AdjustCameraSharedSubtractTarget
	ldh [CameraDYH], a

	call DivideDifferenceForLerp

	; Move the camera toward target
	ldh a, [CameraY+0]
	add l
	ldh [CameraY+0], a
	ldh a, [CameraY+1]
	adc h
	ldh [CameraY+1], a

; ---------------------------------------
; Do scroll updates on the side as necessary

	; Is a column update required?
	ld hl, CameraX
	ldh a, [OldCameraX]
	xor [hl]
	and $80
	jr z, .NoUpdateColumn
		ldh a, [CameraDXH]
		add a ; Check the sign

		; Get the number of tiles for the current camera position
		push af
		ld a, [hl] ; CameraX
		add a      ; Get top bit
		ldh a, [CameraX+1]
		rla        ; Add in the top bit from CameraX+0 to get the tile count
		ld h, a
		pop af
		ld a, h

		jr c, .UpdateLeft
	.UpdateRight:
		add 20
		ld [DoUpdateColumn], a
		jr .NoUpdateColumn
	.UpdateLeft:
		dec a
		ld [DoUpdateColumn], a
	.NoUpdateColumn:

	ld hl, CameraY
	ldh a, [OldCameraY]
	xor [hl]
	and $80
	jr z, .NoUpdateRow
		ldh a, [CameraDYH]
		add a ; Check the sign

		; Get the number of tiles for the current camera position
		push af
		ld a, [hl] ; CameraY
		add a      ; Get top bit
		ldh a, [CameraY+1]
		rla        ; Add in the top bit from CameraY+0 to get the tile count
		ld h, a
		pop af
		ld a, h

		jr c, .UpdateUp
	.UpdateDown:
		add 18
		; Fall through
	.UpdateUp:
		ld [DoUpdateRow], a
	.NoUpdateRow:

; ---------------------------------------

	; Fall through
CameraConvertXY:
CameraConvertY:
	; Convert camera to pixel coordinates
	ldh a, [CameraY+1]
	ld b, a
	ldh a, [CameraY+0]
	rept 4
		srl b
		rra
	endr
	adc 0
	ld [CameraYPixel+0], a
	ld a, b
	ld [CameraYPixel+1], a

	; Fall through
CameraConvertX:
	; Convert camera to pixel coordinates
	ldh a, [CameraX+1]
	ld b, a
	ldh a, [CameraX+0]
	rept 4
		srl b
		rra
	endr
	adc 0
	ld [CameraXPixel+0], a
	ld a, b
	ld [CameraXPixel+1], a

	; Calculate pointer for parallax effect
	; X part
	ld a, [CameraXPixel+0]
	rrca
	and 7
	ld h, 0
	ld l, a
	add hl, hl ; * 32
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl

	; Y part
	ld a, [CameraYPixel+0]
	rrca
	cpl
	inc a
	and 7
	add a
	add_hl_a

	; Write the pointer
	ld de, ParallaxShifts
	add hl, de
	ld a, l
	ld [ParallaxSource+0], a
	ld a, h
	ld [ParallaxSource+1], a
	ret

DivideDifferenceForLerp:
	ld a, l
	rept 3
	sra h
	rra
	endr
	ld l, a

	; Is this result too small?
	ld a, h
	or a
	jr z, .High00
    inc a ; Check if FF
    ret nz
.HighFF:
	ld a, l
	cp 256-2 ; Anything except FE or FF is OK
	jr nc, .DontMoveCamera
	ret
.High00:
	ld a, l
    cp 3 ; Anything except 0 1 or 2 is OK
	ret nc
.DontMoveCamera:
	ld h, 0
	ld l, h
	ret
