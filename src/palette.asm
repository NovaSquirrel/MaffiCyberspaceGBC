; Maffi cyberspace game
; Copyright (C) 2022-2024 NovaSquirrel
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

; .----------------------------------------------
; | Color fade routines
; '----------------------------------------------
SECTION "Palette code", ROM0

FadeToScreenOff::
	ldh a,[rLCDC]
	add a
	ret nc

	call FadeToWhite

	; Be sure the Game Boy is in vblank before actually turning the screen off
:	ldh a, [rLY]
	cp 144
	jr c, :-

	xor a
	ldh [rLCDC], a
	ret

FadeToWhite::
	ldh a, [IsNotGameBoyColor]
	or a
	ret nz

	ld a, BANK(BG_Palette_24bit_Current)
	ldh [rSMBK], a

	; Calculate BG_Palette_RGB888_Delta
	ld hl, BG_Palette_24bit_Current
	ld c, 8*4*3
:	ld a, 31*8
	sub [hl] ; Current
	srl a
	srl a
	srl a
	inc h
	ld [hl+], a ; Delta
	dec h
	dec b
	jr nz, :-

	call Do8StepColorFade

	ld a, BANK(Playfield)
	ldh [rSMBK], a
	ret

; HL = Pointer to a 24-bit color palette
FadeFromWhiteToPalette::
	ldh a, [IsNotGameBoyColor]
	or a
	ret nz

	ld a, BANK(BG_Palette_24bit_Current)
	ldh [rSMBK], a

	; HL = Source, passed into this routine
	ld de, BG_Palette_24bit_Target
	ld c, 8*4*3
	rst MemcpySmall

	; Calculate BG_Palette_RGB888_Delta
	ld hl, BG_Palette_24bit_Target
	ld c, 8*4*3
:	ld a, 31*8
	sub [hl] ; Target
	srl a
	srl a
	srl a
	cpl
	inc a ; Negate
	dec h
	ld [hl+], a ; Delta
	inc h
	dec b
	jr nz, :-

	call Do8StepColorFade

	ld a, BANK(Playfield)
	ldh [rSMBK], a

	ret

; -----------------------------------------------

; Shared routine to gradually fade from the current palette to the destination palette
Do8StepColorFade:
	ld a, 8
	ldh [temp1], a
.FadeLoop:
	ld hl, BG_Palette_24bit_Current
	ld de, BG_Palette_15bit
	ld b, 8*4 + 8*3
.FadeOneColor:
	push bc
	; Add delta
	; Blue
	ld a, [hl]
	inc h
	add [hl]
	dec h
	ld [hl+], a
	and %11111000
	rra
	rra
	rra
	ld b, a ; ...bbbbb ........

	; Green
	ld a, [hl]
	inc h
	add [hl]
	dec h
	ld [hl+], a
	and %11111000
	add a
	rl b
	add a
	rl b
	ld c, a ; .bbbbbgg ggg.....

	; Red
	ld a, [hl]
	inc h
	add [hl]
	dec h
	ld [hl+], a
	and %11111000
	rra
	rra
	rra
	or c ; .bbbbbgg gggrrrrr
	ld [de], a
	inc e
	ld a, b
	ld [de], a
	inc e
	pop bc
	dec b
	jr nz, .FadeOneColor

	call WaitVblank
	call WaitVblank

	ld a, BCPSF_AUTOINC   ; index zero, auto increment
	ldh [rBCPS], a        ; background palette index
	ld hl, BG_Palette_15bit
	ld c, LOW(rBCPD)
	ld b, (8*4*2)/8
:
	rept 8
	ld a, [hl+]
	ld [$ff00+c],a
	endr
	dec b
	jr nz, :-

	; -----------------
	ldh a, [temp1]
	dec a
	ldh [temp1], a
	jr nz, .FadeLoop
	ret

; .----------------------------------------------
; | The actual palettes
; '----------------------------------------------
SECTION "Palette data", ROMX

MACRO rgbf
	db (\3)*8, (\2)*8, (\1)*8
ENDM

BG_Gameplay_Palette_24bit::
; Background palette
; 0 terrain
  rgbf 39/8-3, 65/8-3,  45/8-3
  rgbf 61/8-3, 111/8-3, 67/8-3
  rgbf 66/8-3, 164/8-3, 89/8-3
  rgbf 89/8-3, 207/8-3, 147/8-3
; 1 wall
  rgbf 58/8-3,  81/8-3,  73/8-3
  rgbf 93/8-3,  155/8-3, 121/8-3
  rgbf 134/8-3, 198/8-3, 154/8-3
  rgbf 181/8-3, 231/8-3, 203/8-3
; 2 parallax
  rgbf 27/8, 36/8, 71/8
  rgbf 43/8, 78/8, 149/8
  rgbf 22, 22, 22 ; unused
  rgbf 31, 31, 31 ; unused
; 3 purple
  rgbf $49/8, $41/8, $82/8
  rgbf $78/8, $64/8, $c6/8
  rgbf $9c/8, $8b/8, $db/8
  rgbf $ce/8, $aa/8, $ed/8
; 4 blue
  rgbf $2b/8, $4e/8, $95/8
  rgbf $27/8, $89/8, $cd/8
  rgbf $42/8, $bf/8, $e8/8
  rgbf $73/8, $ef/8, $e8/8
; 5 orange
  rgbf $ac/8, $32/8, $32/8
  rgbf $d9/8, $57/8, $63/8
  rgbf $fc/8, $a5/8, $70/8
  rgbf $ff/8, $e0/8, $b7/8
; 6
  rgbf  0,  0,  0
  rgbf 13, 13, 13
  rgbf 22, 22, 22
  rgbf 31, 31, 31
; 7 status line?
  rgbf 0, 0, 0 ;$49/8,$41/8,$82/8
  rgbf $78/8,$64/8,$c6/8
  rgbf $9c/8,$8b/8,$db/8
  rgbf $ce/8,$aa/8,$ed/8

BG_Gameplay_Palette_24bit_Bright:
; Background palette
; 0 terrain
  rgbf $02, $0d, $05
  rgbf $08, $15, $0A 
  rgbf $0D, $19, $10
  rgbf $12, $1B, $16
; 1 wall
  rgbf $00, $0B, $07
  rgbf $00, $18, $0C
  rgbf $07, $1B, $0F
  rgbf $0D, $1F, $16
; 2 parallax
  rgbf 27/8, 36/8, 71/8
  rgbf 43/8, 78/8, 149/8
  rgbf 22, 22, 22 ; unused
  rgbf 31, 31, 31 ; unused
; 3 purple
  rgbf $49/8, $41/8, $82/8
  rgbf $78/8, $64/8, $c6/8
  rgbf $9c/8, $8b/8, $db/8
  rgbf $ce/8, $aa/8, $ed/8
; 4 blue
  rgbf $2b/8, $4e/8, $95/8
  rgbf $27/8, $89/8, $cd/8
  rgbf $42/8, $bf/8, $e8/8
  rgbf $73/8, $ef/8, $e8/8
; 5 orange
  rgbf $ac/8, $32/8, $32/8
  rgbf $d9/8, $57/8, $63/8
  rgbf $fc/8, $a5/8, $70/8
  rgbf $ff/8, $e0/8, $b7/8
; 6
  rgbf  0,  0,  0
  rgbf 13, 13, 13
  rgbf 22, 22, 22
  rgbf 31, 31, 31
; 7 status line?
  rgbf 0, 0, 0 ;$49/8,$41/8,$82/8
  rgbf $78/8,$64/8,$c6/8
  rgbf $9c/8,$8b/8,$db/8
  rgbf $ce/8,$aa/8,$ed/8

Sprite_Palette::
; Sprite palette
; 0 cyberspace blue
  rgb 0,  0,   0
  rgb $2b/8-2,$4e/8-2, $95/8-2
;  rgb $42/8-3, $bf/8-3, $e8/8-3
;  rgb $00/8, $ae/8, $e8/8
  rgb $42/8+1, $7a/8+1, $e8/8+1
  rgb 31, 31, 31
; 1 green
  rgb  0,  0,  0
  rgb  0,  0,  0
  rgb  0, 31,  0
  rgb 31, 31, 31
; 2 blue
  rgb  0,  0,  0
  rgb  0,  0,  0
  rgb  0,  0, 31
  rgb 31, 31, 31
; 3 yellow
  rgb  0,  0,  0
  rgb  0,  0,  0
  rgb 31, 15,  0
  rgb 31, 31, 31
; 4 
  rgb  0,  0,  0
  rgb  0,  0,  0
  rgb  0,  0,  0
  rgb  0,  0,  0
; 5 pink
  rgb  0,  0,  0
  rgb  0,  0,  0
  rgb 31, 15, 15
  rgb 31, 31, 31
; 6 Maffi's eyes and nose
  rgb  0,  0,  0
  rgb 30, 15, 12 ; Pink
  rgb 17, 18, 31 ; Blue
  rgb 31, 31, 31 ; White
; 7 Maffi
  rgb  0,  0,  0
  rgb  6,  6,  6              ; Black
;  rgb  15-3, 8-3,  31-3      ; Purple
  rgb  15-4, 8-4,  31-4       ; Purple (try to be more purple on GBC?)
  rgb  24-2, 24-2, $14 ;24-2  ; Light gray

Sprite_PaletteBright::
; Sprite palette
; 0 cyberspace blue
  rgb 0,  0,   0
  rgb $04, $09, $17
  rgb $0C, $12, $1F
  rgb 31, 31, 31
; rgb  0,  0,  0
; rgb  0,  0,  0
; rgb 31,  0,  0
; rgb 31, 31, 31
; 1 green
  rgb  0,  0,  0
  rgb  0,  0,  0
  rgb  0, 31,  0
  rgb 31, 31, 31
; 2 blue
  rgb  0,  0,  0
  rgb  0,  0,  0
  rgb  0,  0, 31
  rgb 31, 31, 31
; 3 red
  rgb  0,  0,  0
  rgb  0,  0,  0
  rgb 31, 15,  0
  rgb 31, 31, 31
; 4 
  rgb  0,  0,  0
  rgb  0,  0,  0
  rgb  0,  0,  0
  rgb  0,  0,  0
; 5 pink
  rgb  0,  0,  0
  rgb  0,  0,  0
  rgb 31, 15, 15
  rgb 31, 31, 31
; 6 Maffi's eyes and nose
  rgb  0,  0,  0
  rgb 30, 15, 12 ; Pink
  rgb 17, 18, 31 ; Blue
  rgb 31, 31, 31 ; White
; 7 Maffi
  rgb  0,  0,  0
  rgb  6,  6,  6              ; Black
  rgb  $0F, $09, $1C          ; Purple
  rgb  $16, $16, $15          ; Light gray

BG_Menu_Palette_24bit::
; Background palette
; 0
  rgbf  0,  0,  0
  rgbf 13, 13, 13
  rgbf 22, 22, 22
  rgbf 31, 31, 31
; 1
  rgbf  0,  0,  0
  rgbf 13, 13, 13
  rgbf 22, 22, 22
  rgbf 31, 31, 31
; 2
  rgbf  0,  0,  0
  rgbf 13, 13, 13
  rgbf 22, 22, 22
  rgbf 31, 31, 31
; 3
  rgbf  0,  0,  0
  rgbf 13, 13, 13
  rgbf 22, 22, 22
  rgbf 31, 31, 31
; 4
  rgbf  0,  0,  0
  rgbf 13, 13, 13
  rgbf 22, 22, 22
  rgbf 31, 31, 31
; 5
  rgbf  0,  0,  0
  rgbf 13, 13, 13
  rgbf 22, 22, 22
  rgbf 31, 31, 31
; 6
  rgbf  0,  0,  0
  rgbf 13, 13, 13
  rgbf 22, 22, 22
  rgbf 31, 31, 31
; 7 status line?
  rgbf  0,  0,  0
  rgbf 13, 13, 13
  rgbf 22, 22, 22
  rgbf 31, 31, 31
