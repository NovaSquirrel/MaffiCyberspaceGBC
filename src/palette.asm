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

SECTION "Palette", ROM0

UploadGameplayPalette::
  ldh a, [IsGameBoyColor]
  cp $11
  ret nz

  ld a, BCPSF_AUTOINC   ; index zero, auto increment
  ldh [rBCPS], a        ; background palette index
  ld hl, BG_Palette
  ld b, 2*4*8
.loop:
  ld a, [hl+]
  ld [rBCPD], a
  dec b
  jr nz, .loop

; Now for sprites
  ld a, OCPSF_AUTOINC   ; index zero, auto increment
  ldh [rOCPS], a        ; background palette index
  ld hl, Sprite_Palette
  ld b, 2*4*8
.loop2:
  ld a, [hl+]
  ld [rOCPD], a
  dec b
  jr nz, .loop2
  ret

BG_Palette:
; Background palette
; 0 terrain
  rgb 39/8-3, 65/8-3,  45/8-3
  rgb 61/8-3, 111/8-3, 67/8-3
  rgb 66/8-3, 164/8-3, 89/8-3
  rgb 89/8-3, 207/8-3, 147/8-3
; 1 wall
  rgb 58/8-3,  81/8-3,  73/8-3
  rgb 93/8-3,  155/8-3, 121/8-3
  rgb 134/8-3, 198/8-3, 154/8-3
  rgb 181/8-3, 231/8-3, 203/8-3
; 2 parallax
  rgb 27/8, 36/8, 71/8
  rgb 43/8, 78/8, 149/8
  rgb 22, 22, 22 ; unused
  rgb 31, 31, 31 ; unused
; 3 purple
  rgb  0,  0,  0
  rgb 13, 13, 13
  rgb 22, 22, 22
  rgb 31, 31, 31
; 4 blue
  rgb  0,  0,  0
  rgb 13, 13, 13
  rgb 22, 22, 22
  rgb 31, 31, 31
; 5 orange
  rgb  0,  0,  0
  rgb 13, 13, 13
  rgb 22, 22, 22
  rgb 31, 31, 31
; 6
  rgb  0,  0,  0
  rgb 13, 13, 13
  rgb 22, 22, 22
  rgb 31, 31, 31
; 7 status line?
  rgb  0,  0,  0
  rgb 13, 13, 13
  rgb 22, 22, 22
  rgb 31, 31, 31

Sprite_Palette:
; Sprite palette
; 0 red
  rgb  0,  0,  0
  rgb  0,  0,  0
  rgb 31,  0,  0
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
; 5 
  rgb  0,  0,  0
  rgb  0,  0,  0
  rgb  0,  0,  0
  rgb  0,  0,  0
; 6
  rgb  0,  0,  0
  rgb  0,  0,  0
  rgb  0,  0,  0
  rgb  0,  0,  0
; 7 Maffi
  rgb  0,  0,  0
  rgb  6,  6,  6
  rgb  15-3, 8-3,  31-3
  rgb  24-2, 24-2, $14 ;24-2
