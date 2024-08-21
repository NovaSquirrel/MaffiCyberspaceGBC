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
  ld a, IEF_VBLANK|IEF_STAT
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

; -----------------------------------------------

ClearAllOAM::
	ld hl, OamBuffer
	xor a
ClearOAM_Unrolled:
	rept 39
	ld [hl+], a
	inc l
	inc l
	inc l
	endr
	ld [hl+], a
	ret

ClearPreviouslyUsedOAM::
	ld a, [PreviousOAMWrite]
	ld b, a
	ldh a, [OAMWrite]
	cp b
	ret nc ; If New >= Old, so don't do anything

	; New < Old
	; Jump to ClearOAM_Unrolled + (40*4 - (Previous - New))
	; Conveniently every loop iteration is 4 bytes, and every OAM entry is 4 bytes, so I can use the same counts for both
	cpl
	inc a
	add b
	cpl
	inc a
	add 40*4
	ld hl, ClearOAM_Unrolled
	add_hl_a
	push hl

	; Clear out just the part at the end that is now stale data
	ld h, HIGH(OamBuffer)
	ldh a, [OAMWrite]
	ld l, a

	xor a ; Clear with zero
	ret   ; Do the jump that was calculated earlier

ClearAndWriteOAM::
  call ClearAllOAM
  ld a, OamBuffer>>8
  jp RunOamDMA

; -----------------------------------------------
; Adapted from http://wiki.nesdev.com/w/index.php/Random_number_generator/Linear_feedback_shift_register_(advanced)
RandomByte::
	push bc
	; rotate the middle bytes left
	ldh a, [seed+0]
	ld c, a

	ldh a, [seed+1]
	ldh [seed+2], a
	; compute seed+1 ($C5>>1 = %1100010)
	ldh a, [seed+3] ; original high byte
	srl a
	ld b, a ; reverse: 100011
	srl a
	srl a
	srl a
	srl a
	xor b
	srl a
	xor b
	xor c ; combine with original low byte
	ldh [seed+1], a
	; compute seed+0 ($C5 = %11000101)

	ldh a, [seed+2] ; will move to seed+3 at the end
	ld c, a         ; save it for then

	ldh a, [seed+3] ; original high byte
	ld b, a
	add a
	xor b
	add a
	add a
	add a
	add a
	xor b
	add a
	add a
	xor b
	ldh [seed+0], a

	; finish rotating byte 2 into 3
	ld a, c
	ldh [seed+3], a
	pop bc

	ldh a, [seed+0]
	ret

; -----------------------------------------------
; Block related ROM0 routines

; Sets HL to a pointer at a point on the map where D=X and E=Y
MapPointerDE_XY::
           ;     E           A
	xor a  ; ..yy yyyy | .... ....
	srl e
	rra    ; ...y yyyy | y... ....
	srl e
	rra    ; .... yyyy | yy.. ....
	or d
	ld l, a

	ld a, e
	or high(Playfield)
	ld h, a
	ret

; Sets HL to a pointer at a point on the map where B=X and C=Y
MapPointerBC_XY::
           ;     C           A
	xor a  ; ..yy yyyy | .... ....
	srl c
	rra    ; ...y yyyy | y... ....
	srl c
	rra    ; .... yyyy | yy.. ....
	or b
	ld l, a

	ld a, c
	or high(Playfield)
	ld h, a
	ret

; Sets HL to a pointer at a point on the map where L=X and H=Y
MapPointerLH_XY::
	; yyyy yyxx xxxx
           ;     H           A
	xor a  ; ..yy yyyy | .... ....
	srl h
	rra    ; ...y yyyy | y... ....
	srl h
	rra    ; .... yyyy | yy.. ....
	or l
	ld l, a

	ld a, h
	or high(Playfield)
	ld h, a
	ret

; Get map flags for the block where L=X and H=Y
MapFlagsLH_XY::
	call MapPointerLH_XY
	ld a, [hl]
	ld h, HIGH(BlockFlags)
	ld l, a
	ld a, [hl]
	ret
