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

SECTION "MainLoop", ROM0

; -----------------------------------------------------------------------------

;PlayerAnimationFrame

StartLevel::
	call GenerateMaze

	call ClearAndWriteOAM
	call ScreenOn

Gameplay::
; -------------------------------------------------------------------------
; Main loop
	ld a, 32
	ldh [PlayerPXH], a
	ldh [PlayerPYH], a
	ld a, 255
	ld [DoUpdateRow], a
	ld [DoUpdateColumn], a
	xor a
	ld [PlayerAnimationFrame], a

	ld a, LOW(ParallaxShifts)
	ld [ParallaxSource+0], a
	ld a, HIGH(ParallaxShifts)
	ld [ParallaxSource+1], a

	call InitCamera
	call RenderLevelScreen
forever:
	; .----------------------
	; | Pre-vblank tasks
	; '----------------------

	; On DMG, skip over the DMA code
	ldh a, [IsNotGameBoyColor]
	or a
	jp nz, VblankForDMG

	; Calculate a pointer to the frame graphics
	ld a, [PlayerAnimationFrame]
	add a, a ; * 2
	add a, a ; * 4
	add a, a ; * 8
	ld h, 0
	ld l, a
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	ld de, PlayerAnimationFrameGraphics+16
	add hl, de
	push hl

	; .----------------------
	; | Vblank
	; '----------------------
	call WaitVblank

	; Copy in the player frame
	ld a, BANK(PlayerAnimationFrameGraphics)
	ld [rROMB0], a
	pop hl
	ld a, h
	ldh [rHDMA1], a
	ld a, l
	ldh [rHDMA2], a
	ld a, HIGH($8010)
	ldh [rHDMA3], a
	ld a, LOW($8010)
	ldh [rHDMA4], a
	ld a, 7-1
	ldh [rHDMA5], a
	
AfterVblankForDMG: ; The DMG-specific code will jump here once it's done

	; Set scroll
	ld a, [CameraXPixel]
	ldh [rSCX], a
	ld a, [CameraYPixel]
	ldh [rSCY], a

	ld a, OamBuffer>>8
	call RunOamDMA

	; Parallax upload
	ld hl, ParallaxSource
	ld a, [hl+]
	ld h, [hl]
	ld l, a
	ld de, $9000
	ld b, 2
:	; Semi-unrolled loop
	rept 8
	ld a, [hl+]
	ld [de], a
	inc e
	endr
	dec b
	jr nz, :-

	ld a, BANK(UpdateRow)
	ld [rROMB0], a

	; Ideally the row/column updates happen in vblank, but may spill over
	ld a, [DoUpdateRow]
	rlca
	jr c, :+
		rrca
		call UpdateRow
	:

	ld a, [DoUpdateColumn]
	rlca
	jr c, :+
		rrca
		call UpdateColumn
	:

	; .----------------------
	; | Clean up after vblank
	; '----------------------
	ld a, 255
	ld [DoUpdateRow], a
	ld [DoUpdateColumn], a

	; .----------------------
	; | Game logic
	; '----------------------
	call ReadKeys
	call ClearOAM

	xor a
	ldh [OamWrite], a

	call RunPlayer

	call AdjustCamera

	jp forever

; -----------------------------------------------
VblankForDMG:
	; .----------------------
	; | Vblank
	; '----------------------
	call WaitVblank

	jp AfterVblankForDMG
