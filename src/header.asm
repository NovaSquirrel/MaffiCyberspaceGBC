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
include "include/hardware.inc/hardware.inc"
include "include/macros.inc"

SECTION "rst00", ROM0[$0000]
	ret
SECTION "rst08", ROM0[$0008]
	; add hl, a
	add a,l
	ld l,a
	jr nc, @+3
	inc h
	ret
SECTION "rst10", ROM0[$0010]
	ret
SECTION "rst18", ROM0[$0018]
	ret
SECTION "rst20", ROM0[$0020]
	ret
SECTION "rst28", ROM0[$0028]
	ret
SECTION "rst30", ROM0[$0030]
	ret
SECTION "rst38", ROM0[$0038]
	ret

; $0040 - $0067: Interrupt handlers.
SECTION "vblank", ROM0[$0040]
	jp vblank
SECTION "stat", ROM0[$0048]
	reti
SECTION "timer", ROM0[$0050]
	jp timer
SECTION "serial", ROM0[$0058]
	jp serial
SECTION "joypad", ROM0[$0060]
	jp joypad

SECTION "Header", ROM0[$100]

	; This is your ROM's entry point
	; You have 4 bytes of code to do... something
	di
	jp EntryPoint

	; Make sure to allocate some space for the header, so no important
	; code gets put there and later overwritten by RGBFIX.
	; RGBFIX is designed to operate over a zero-filled header, so make
	; sure to put zeros regardless of the padding value. (This feature
	; was introduced in RGBDS 0.4.0, but the -MG etc flags were also
	; introduced in that version.)
	ds $150 - @, 0

SECTION "Entry point", ROM0

EntryPoint:
	ld sp, Playfield

	push af
	; Clear HRAM
	ld hl, $ff80
	ld c, 127
	call memclear8
	pop af

	ldh [IsGameBoyColor], a
	cp $11
	jr nz, .NotGameBoyColor
		xor a
		ldh [rVBK], a  ; VRAM bank
		inc a
		ldh [rSVBK], a ; WRAM bank

		xor a
		ldh [rIE], a
		ld a, $30
		ldh [rP1], a
		ld a, 1
		ldh [rSPD], a
		stop
.NotGameBoyColor:

	; Clear RAM (but not the return address)
	ld hl, _RAM
	ld bc, 4096-2
	call memclear

	; Copy in DMA routine
	ld hl, oam_dma_routine
	ld de, RunOamDMA
	ld c, oam_dma_routine_end - oam_dma_routine
	call memcpy8

	; Copy in the tileset, which the screen should be off for
	call ScreenOff

	ld de, SpriteTileset
	ld hl, _VRAM8000
	ld b, 10*16
	call pb16_unpack_block

	ld de, PlayfieldTileset
	ld hl, _VRAM9000
	ld b, 6*16
	call pb16_unpack_block

	ldh a, [IsGameBoyColor]
	cp $11
	call z, UploadGameplayPalette

	; ---------------------------------------------------------------

	ld a, %00011011 ; black, dark gray, light gray, white
	ldh [rBGP], a
	ld a, %00101111 ; transparent, black, dark gray, white
	ldh [rOBP0], a
	ld a, %00011011 ; transparent, black, light gray, white
	ldh [rOBP1], a

	; Results in a lot of closed-off tiles
	; which is great for testing the maze fixer.
	ld a, 25
	ldh [seed], a
	ld a, 145
	ldh [seed+1], a
	ld a, 161
	ldh [seed+2], a
	ld a, 81
	ldh [seed+3], a

	ld bc, 1234
	call SeedRandomLCG

	call GenerateMaze

	call ClearAndWriteOAM
	call ScreenOn

; -------------------------------------------------------------------------
; Main loop
	ld a, 32
	ldh [PlayerPXH], a
	ldh [PlayerPYH], a
	ld a, 255
	ld [DoUpdateRow], a
	ld [DoUpdateColumn], a
	call InitCamera
	call RenderLevelScreen
forever:
	; .----------------------
	; | Vblank
	; '----------------------
	call WaitVblank

	ld a, [CameraXPixel]
	ldh [rSCX], a
	ld a, [CameraYPixel]
	ldh [rSCY], a

	ld a, OamBuffer>>8
	call RunOamDMA

	ld a, [DoUpdateRow]
	rlca
	jr c, :+
		rrca
		call UpdateRow
		ld a, 255
		ld [DoUpdateRow], a
	:

	ld a, [DoUpdateColumn]
	rlca
	jr c, :+
		rrca
		call UpdateColumn
		ld a, 255
		ld [DoUpdateColumn], a
	:

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

SECTION "OamCode", ROM0

oam_dma_routine:
	ldh [rDMA],a
	ld  a,$28
.wait:
	dec a
	jr  nz,.wait
	ret
oam_dma_routine_end:

SECTION "Tileset", ROM0

PlayfieldTileset:
	incbin "res/tilesets/playfield_tiles.pb16"
SpriteTileset:
	incbin "res/tilesets/sprite_tiles.pb16"
