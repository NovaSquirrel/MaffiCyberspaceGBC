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
AddHL_A::
	; add hl, a
	add a,l
	ld l,a
	ret nc
	inc h
	ret
SECTION "rst10", ROM0[$0010]
MemcpySmall::
	ld a, [de]
	ld [hli], a
	inc de
	dec c
	jr nz, MemcpySmall
	ret
SECTION "rst18", ROM0[$0018]
MemsetSmall::
	ld [hli], a
	dec c
	jr nz, MemsetSmall
	ret
SECTION "rst20", ROM0[$0020]
CallDE::
	push de
	ret
SECTION "rst28", ROM0[$0028]
CallHLIndirect::
	ld a, [hl+]
	ld h, [hl]
	ld l, a
	jp hl
SECTION "rst30", ROM0[$0030]
	ret
SECTION "rst38", ROM0[$0038]
	ret

; $0040 - $0067: Interrupt handlers.
SECTION "vblank", ROM0[$0040]
	jp vblank
SECTION "stat", ROM0[$0048]
	push af
	ldh a, [LYC_Interrupt_LCDC]
	ldh [rLCDC],a
	jp stat_continued
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

	sub $11
	ldh [IsNotGameBoyColor], a
	jr nz, .NotGameBoyColor
		xor a
		ldh [rVBK], a  ; VRAM bank
		inc a
		ldh [rSVBK], a ; WRAM bank
.NotGameBoyColor:

	; It's important to turn audio and rendering off for the speed switch later, to avoid "odd mode"
	; but it also just makes sense to 
	xor a
	ldh [rAUDENA], a

	; Turn the screen off because we're doing setup now
	call ScreenOff

	ldh a, [IsNotGameBoyColor]
	or a
	jr nz, :+
		; Switch to double speed mode; see https://gbdev.io/pandocs/CGB_Registers.html#ff4d--key1-cgb-mode-only-prepare-speed-switch
		xor a
		ldh [rIE], a
		ld a, $30
		ldh [rP1], a
		ld a, 1
		ldh [rSPD], a
		stop
	:

	; Clear half of RAM fast
	ld hl, _RAM
	xor a
:
	rept 16
	ld [hl+], a
	endr
	bit 4, h   ; Detect going from Cx to Dx
	jr z, :-

	; Set up Super Game Boy, if present
	ld a, BANK(detect_sgb)
	ld [rROMB0], a
	ldh a, [IsNotGameBoyColor]
	or a
	call nz, detect_sgb
	ld a, [IsSuperGameBoy]
	or a
	call nz, SetupSGB ; Sets up a library of palettes and attribute screens, so that the game can just use the PAL_SET command later

	; Copy in DMA routine
	ld hl, oam_dma_routine
	ld de, RunOamDMA
	ld c, oam_dma_routine_end - oam_dma_routine
	call memcpy8

	; Load in some graphics
	ld a, BANK(PlayfieldTileset)
	ld [rROMB0], a

	ld de, SpriteTileset
	ld hl, _VRAM8000
	ld b, 6*16
	call pb16_unpack_block

	ld de, PlayfieldTileset
	ld hl, _VRAM9000
	ld b, 8*16
	call pb16_unpack_block
	
	ld de, StatusTileset
	ld a, [IsSuperGameBoy]
	or a
	jr z, :+
		ld de, StatusTilesetSGB
	:
	ld hl, _VRAM8000 + $F00
	ld b, 1*16
	call pb16_unpack_block

	ld de, SpWalkerTileset
	ld hl, _VRAM8000 + $500
	ld b, 16
	call pb16_unpack_block
	ld de, SpBallTileset
	ld hl, _VRAM8000 + $600
	ld b, 16
	call pb16_unpack_block

	ldh a, [IsNotGameBoyColor]
	or a
	call z, UploadGameplayPalette

	; ---------------------------------------------------------------

	ld a, %00011011 ; black, dark gray, light gray, white
	ldh [rBGP], a
	ld a, %00101111 ; transparent, black, dark gray, white
	ldh [rOBP0], a
	ld a, %00011011 ; transparent, dark gray, light gray, white
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

	; Start the first level
	xor a
	jp StartLevel

SECTION "stat2", ROM0
stat_continued:
	; Wait for the start of hblank
	; LYC interrupts happen at the very start of a line, which is mode 2, so there is potential for a race condition where the main code detects the very end of hblank just before it becomes mode 2, and then the interrupt happens, and now it's busy drawing so the writes will fail.
	; See https://gbdev.io/guides/lyc_timing.html#the-vram-access-race-condition
:	ldh a, [rSTAT]
	and STATF_LCD
	jr nz, :-
	pop af
	reti

SECTION "OamCode", ROM0

oam_dma_routine:
	ldh [rDMA],a
	ld  a,$28
.wait:
	dec a
	jr  nz,.wait
	ret
oam_dma_routine_end:

SECTION "Tileset", ROMX

PlayfieldTileset:
	incbin "res/tilesets/playfield_tiles.pb16"
SpriteTileset:
	incbin "res/tilesets_8x16/sprite_tiles.pb16"
StatusTileset:
	incbin "res/tilesets/status_tiles.pb16"
StatusTilesetSGB:
	incbin "res/tilesets/status_tiles_sgb.pb16"
SpWalkerTileset:
	incbin "res/tilesets_8x16/sp_walker.pb16"
SpBallTileset:
	incbin "res/tilesets_8x16/sp_ball.pb16"
SpBonziTileset:
	incbin "res/tilesets_8x16/sp_bonzi.pb16"
SpClippyTileset:
	incbin "res/tilesets_8x16/sp_clippy.pb16"
SpGeorgeTileset:
	incbin "res/tilesets_8x16/sp_george.pb16"
SpRoverTileset:
	incbin "res/tilesets_8x16/sp_rover.pb16"


