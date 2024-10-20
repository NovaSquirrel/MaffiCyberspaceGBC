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
	; 3 bytes free
SECTION "rst10", ROM0[$0010]
MemcpySmall::
	ld a, [hli]
	ld [de], a
	inc de
	dec c
	jr nz, MemcpySmall
	ret
	; 1 byte free
SECTION "rst18", ROM0[$0018]
MemsetSmall::
	ld [hli], a
	dec c
	jr nz, MemsetSmall
	ret
	; 3 bytes free
SECTION "rst20", ROM0[$0020]
CallDE::
	push de
	ret
	; 6 bytes free
SECTION "rst28", ROM0[$0028]
CallHLIndirect::
	ld a, [hl+]
	ld h, [hl]
	ld l, a
	jp hl
	; 4 bytes free
SECTION "rst30", ROM0[$0030]
	ret
SECTION "rst38", ROM0[$0038]
	; Should put some sort of error handler here
	ret

; $0040 - $0067: Interrupt handlers.
SECTION "vblank", ROM0[$0040]
	jp VblankIndirectJump
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
	ld sp, StackEnd

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

	; Turn off the screen but without turning on interrupts
	ld a, BANK(busy_wait_vblank)
	ld [rROMB0], a
	call busy_wait_vblank
	xor a
	ldh [rLCDC], a

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

	call SetDefaultVblankHandler

	; Set up Super Game Boy, if present
	ld a, BANK(detect_sgb)
	ld [rROMB0], a
	ldh a, [IsNotGameBoyColor]
	or a
	call nz, detect_sgb
	ld a, [IsSuperGameBoy]
	or a
	call nz, SetupSGB ; Sets up a library of palettes and attribute screens, so that the game can just use the PAL_SET command later

	call ScreenOff ; SetupSGB will turn the screen off, but make sure it's off on GB and GBC too

	; Copy in OAM DMA routine
	ld hl, oam_dma_routine
	ld de, RunOamDMA
	ld c, oam_dma_routine_end - oam_dma_routine
	call memcpy8

	; Set sprite palettes on GBC only
	ldh a, [IsNotGameBoyColor]
	or a
	jr nz, .DontLoadSpritePalettes
		ld a, BANK(Sprite_Palette)
		ld [rROMB0], a

		ld a, OCPSF_AUTOINC   ; index zero, auto increment
		ldh [rOCPS], a        ; background palette index
		ld hl, Sprite_Palette
		ld a, [UseBrighterPalettes]
		or a
		jr z, :+
			ld hl, Sprite_PaletteBright
		:
		ld b, 2*4*8
	:	ld a, [hl+]
		ldh [rOCPD], a
		dec b
		jr nz, :-

		; Initialize BG_Palette_24bit_Current to white
		ld a, BANK(BG_Palette_24bit_Current)
		ldh [rSMBK], a
		ld hl, BG_Palette_24bit_Current
		ld c, 8*4*3
		ld a, 31*8
		rst MemsetSmall
		ld a, BANK(Playfield)
		ldh [rSMBK], a
	.DontLoadSpritePalettes:

	; ---------------------------------------------------------------

	ld a, %00011011 ; black, dark gray, light gray, white
	ldh [rBGP], a
	ld a, %00101111 ; transparent, black, dark gray, white
	ldh [rOBP0], a
	ld a, %00011111 ; transparent, dark gray, light gray, white
	ldh [rOBP1], a

	jp ShowTitleScreen

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
