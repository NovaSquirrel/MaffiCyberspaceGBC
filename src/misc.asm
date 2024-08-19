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

SECTION "miscellaneous", ROM0

ScreenOff::
  call WaitVblank
  xor a
  ldh [rLCDC], a
  ret

ScreenOn::
  ld a, LCDCF_ON|LCDCF_OBJ16|LCDCF_OBJON|LCDCF_BGON|LCDCF_BG8800|LCDCF_WIN9C00|LCDCF_WINON
  ldh [rLCDC],a
  ret

vblank::
  push af
  ld a, [framecount]
  inc a
  ld [framecount], a
  pop af
  reti

WaitVblank::
  push hl
  push af
  ld a, %00011
  ldh [rIE],a     ; Enable vblank interrupt
  ei

  ld   hl, framecount
  ld   a, [hl]
.loop:
  halt
  cp   a, [hl]
  jr   z, .loop
  pop af
  pop hl
  ret

timer::
serial::
joypad::
  reti

memclear::
  xor a
  ld [hl+], a
  dec bc
  ld a,b
  or c
  jr nz, memclear
  ret

memset::
  ld a, e
  ld [hl+], a
  dec bc
  ld a, b
  or c
  jr nz, memset
  ret

memcpy::
  ld a, [hl+]
  ld [de], a
  inc de
  dec bc
  ld a, b 
  or c
  jr nz, memcpy
  ret

memcpy8::
  ld a, [hl+]
  ld [de], a
  inc de
  dec c
  jr nz, memcpy8
  ret

memclear8::
  xor a
memset8::
  ld [hl+], a
  dec c
  jr nz, memset8
  ret

strcpy:
  ld a, [hl+]
  or a
  ret z
  ld [de], a
  inc de
  jr strcpy
  ret

ReadKeys::
  ldh a, [KeyDown]
  ldh [KeyLast], a

  ld a, P1F_GET_BTN
  call .onenibble
  and $f
  ld b, a

  ld a, P1F_GET_DPAD
  call .onenibble
  and $f
  swap a
  or b
  cpl
  ldh [KeyDown], a

  ld a,P1F_GET_NONE ; Stop asking for any keys
  ldh [rP1],a

  ldh a, [KeyLast]
  cpl
  ld b, a
  ldh a, [KeyDown]
  and b
  ldh [KeyNew], a
  ret

.onenibble:
  ldh [rP1],a     ; switch the key matrix
  call .knownret  ; burn 10 cycles calling a known ret
  ldh a,[rP1]     ; ignore value while waiting for the key matrix to settle
  ldh a,[rP1]
  ldh a,[rP1]     ; this read counts
.knownret:
  ret

DoKeyRepeat::
  ; Apply key repeat
  ld a, [KeyDown]
  and PADF_LEFT | PADF_DOWN | PADF_UP | PADF_RIGHT
  ld b, a
  ld a, [KeyLast]
  and PADF_LEFT | PADF_DOWN | PADF_UP | PADF_RIGHT
  cp b
  jr nz, .stop_repeat

  ld a, [KeyRepeat] ; Delay before auto-repeat
  cp 16
  jr nz, .no_repeat_yet
  ld a, [framecount]  ; Only repeat every 4 frames
  and 3
  ret nz

  ; Get the d-pad only
  ld a, [KeyDown]
  and PADF_LEFT | PADF_DOWN | PADF_UP | PADF_RIGHT
  ld b, a
  ; repeat those directions
  ld a, [KeyNew]
  or b
  ld [KeyNew], a

  jr .did_repeat
.no_repeat_yet:
  ld a, [KeyRepeat]
  inc a
  ld [KeyRepeat], a
  ret
.stop_repeat:
  xor a
  ld [KeyRepeat],a
.did_repeat:
  ret

ClearNametable::
; Initialize nametables
  ld hl, _SCRN0
  ld bc, 1024*2
  ld e, " "
  jp memset

ClearAttributes::
  ldh a, [IsNotGameBoyColor]
  or a
  ret nz
  vram_bank_1
  ld hl, _SCRN0
  ld bc, 1024*2
  call memclear
  vram_bank_0
  ret

; -----------------------------------------

ClearOAM::
  ld hl, OamBuffer
  xor a
  ldh [OamWrite], a
.clear_sprites:
  ld [hl+], a
  inc l
  inc l
  inc l
  jr nz, .clear_sprites
  ret

ClearAndWriteOAM::
  call ClearOAM
  ld a, OamBuffer>>8
  jp RunOamDMA
