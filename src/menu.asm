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

SECTION "MainMenu", ROM0

ShowTitleScreen::
	call FadeToScreenOff

	xor a
	ldh [rVBK], a
	call ClearNametable

	ld a, BANK(MenuFont)
	ld [rROMB0], a
	ld de, MenuFont
	ld hl, _VRAM9000
	ld b, 128
	call pb16_unpack_block

	ld hl, MaffiTitle1
	ld de, _SCRN0 + 32*6 + 3
	ld c, 14
	rst MemcpySmall

	ld hl, MaffiTitle2
	ld de, _SCRN0 + 32*8 + 2
	ld c, 15
	rst MemcpySmall

	ld hl, MaffiTitle3
	ld de, _SCRN0 + 32*10 + 3
	ld c, 13
	rst MemcpySmall

	ld a, LCDCF_ON|LCDCF_OBJ16|LCDCF_OBJON|LCDCF_BGON|LCDCF_BG8800|LCDCF_WIN9C00
	ldh [rLCDC],a
	ld hl, BG_Menu_Palette_24bit
	call FadeFromWhiteToPalette

.loop:
	call WaitVblank
	call ReadKeys

	ldh a, [KeyNew]
	and PADF_A
	jp z, .loop

	ldh a, [rDIV]
	ld b, a
	ldh a, [framecount]
	ldh [seed], a
	inc a
	ldh [seed+1], a
	inc a
	ldh [seed+2], a
	inc a
	xor b
	ldh [seed+3], a

	; Results in a lot of closed-off tiles
	; which is great for testing the maze fixer.
	;ld a, 25
	;ldh [seed], a
	;ld a, 145
	;ldh [seed+1], a
	;ld a, 161
	;ldh [seed+2], a
	;ld a, 81
	;ldh [seed+3], a

	ld bc, 1234
	call SeedRandomLCG

	; -----------------------
	; Start the first level
	xor a
	ld [HaveGameplayGraphicsInVRAM], a
	jp StartLevel

MaffiTitle1:	db "Untitled Maffi"
MaffiTitle2:	db "cyberspace maze"
MaffiTitle3:	db "game! Press A"


ShowMainMenu::
	call ScreenOff
.loop:
	jp .loop


SECTION "MenuTileset", ROMX
MenuFont:
	incbin "res/tilesets/font.pb16"
