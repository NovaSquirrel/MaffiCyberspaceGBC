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

include "include/defines.inc"
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
memset::
    ; Increment B if C is non-zero
    dec bc
    inc b
    inc c
.loop
    ld [hl+], a
    inc de
    dec c
    jr nz, .loop
    dec b
    jr nz, .loop
	ret

memcpy::
    ; Increment B if C is non-zero
    dec bc
    inc b
    inc c
.loop
    ld a, [hl+]
    ld [de], a
    inc de
    dec c
    jr nz, .loop
    dec b
    jr nz, .loop
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

BlockChangeForPlayer::
	call BlockChangeForActor
	ld a, BANK(RunPlayer)
	ld [rROMB0], a
	ret

; HL = Block to change
; A = Block to change it to
BlockChangeForActor::
	cp [hl]
	ret z  ; If it's already correct, skip everything else
	ld  [hl], a
	ldh [temp1], a
	ld a, BANK(UpdateRow)
	ld [rROMB0], a

	push bc
	push de

	; Check if the block is actually on-screen
	; ???? yyyy | yyxx xxxx 
	ld a, l
	and 63
	ld b, a ; B = block X position
	ldh [temp2], a
	
	ldh a, [CameraX+1]
	sub 1
	jr c, .OkLeft
	cp b
	jp nc, .Exit
	.OkLeft:

	ldh a, [CameraX+1]
	add 10+1
	jr c, .OkRight
	cp b
	jp c, .Exit
	.OkRight:

	ld a, l ; ???? yyyy | yyxx xxxx 
	add a
	rl h    ; ???y yyyy | yxxx xxx0 
	add a
	rl h	; ??yy yyyy | xxxx xx00 
	ld a, h
	and 63
	ld b, a
	ldh [temp3], a

	ldh a, [CameraY+1]
	sub 1
	jr c, .OkUp
	cp b
	jr nc, .Exit
	.OkUp:

	ldh a, [CameraY+1]
	add 9+1
	jr c, .OkDown
	cp b
	jr c, .Exit
	.OkDown:

	; temp2 and temp3 have the block coordinates
	; Y position
	ldh a, [temp3]
	and 15
	add a
	ld l, a
	ld h, 0
	; Multiply by 5
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	ld de, _SCRN0
	add hl, de

	; X position
	ldh a, [temp2]
	and 15
	add a
	add_hl_a

	; HL = the tilemap position now
	push hl
	ldh a, [temp1] ; Block type
	add a
	add a
	ld de, BlockAppearance
	add_de_a

	wait_vram
	ld a, [de]  ; 2
	ld [hl+], a ; 2
	inc e       ; 1
	ld a, [de]  ; 2
	ld [hl], a  ; 2
	ld bc, 32-1 ; =  9
	add hl, bc
	inc e
	wait_vram
	ld a, [de]  ; 2
	ld [hl+], a ; 2
	inc e       ; 1
	ld a, [de]  ; 2
	ld [hl], a  ; 2
	            ; = 9
	pop hl

	; -----------------------
	; On Game Boy Color, do the attribute too
	ldh a, [IsNotGameBoyColor]
	or a
	jr nz, .Exit
	ld a, 1
	ldh [rVBK], a

	; Get the attribute to write
	ldh a, [temp1] ; Block type
	ld de, BlockAppearanceColor
	add_de_a
	ld a, [de]
	ld b, a

	wait_vram
	ld [hl], b ; 2
	inc l      ; 1
	ld [hl], b ; 2
	set 5, l   ; 2
	ld [hl], b ; 2
	dec l      ; 1
	ld [hl], b ; 2
	           ; = 12
	xor a
	ldh [rVBK], a
.Exit:
	pop de
	pop bc
	ld a, BANK(RunActors)
	ld [rROMB0], a
	ret

; -----------------------------------------------
; Actor related ROM0 routines

FindFreeActorSlot::
	; I could use "add hl, bc" to iterate, but that would only be one cycle faster and
	; also potentially require pushing/popping BC, which would eat 8 cycles unconditionally,
	; so I think this is faster overall.
	ld hl, ActorData
.FindFree:
	ld a, [hl]
	or a
	jr z, .Found
	ld a, l
	add ACTOR_SIZE
	ld l, a
	jr nz, .FindFree
	or a ; False
	ret
.Found:
	scf ; True
	ret

ClearActorHL::
	call .Clear16
	inc h
	call .Clear16
	dec h
	ret
.Clear16:
	push hl
	xor a
	ld [hl+], a
	ld [hl+], a
	ld [hl+], a
	ld [hl+], a
	ld [hl+], a
	ld [hl+], a
	ld [hl+], a
	ld [hl+], a
	ld [hl+], a
	ld [hl+], a
	ld [hl+], a
	ld [hl+], a
	ld [hl+], a
	ld [hl+], a
	ld [hl+], a
	ld [hl+], a
	pop hl
	ret

; Input: B (camera position high byte), A (entity position high byte),
;        C (entity position low byte; should already have subtracted camera position low byte)
; Output: A (pixel position)
SharedCameraSubtractCode::
	sbc b
	ld b, a

	; BC = entity position - camera
	ld a, c
	rept 4
		sra b
		rra
	endr
	adc 0
	; A = low half of BC>>4
	ret

; -----------------------------------------------
; LCD copy routines, by ISSOtm

SECTION "LCDMemsetSmallFromB", ROM0

; Writes a value to all bytes in an area of memory
; Works when the destination is in VRAM, even while the LCD is on
; @param hl Beginning of area to fill
; @param c Amount of bytes to write (0 causes 256 bytes to be written)
; @param a Value to write
; @return c 0
; @return hl Pointer to the byte after the last written one
; @return b Equal to a
; @return f Z set, C reset
LCDMemsetSmall::
	ld b, a
; Writes a value to all bytes in an area of memory
; Works when the destination is in VRAM, even while the LCD is on
; Protip: you may want to use `lb bc,` to set both B and C at the same time
; @param hl Beginning of area to fill
; @param c Amount of bytes to write (0 causes 256 bytes to be written)
; @param b Value to write
; @return c 0
; @return hl Pointer to the byte after the last written one
; @return b Equal to a
; @return f Z set, C reset
LCDMemsetSmallFromB::
	wait_vram
	ld a, b
	ld [hli], a
	dec c
	jr nz, LCDMemsetSmallFromB
	ret

SECTION "LCDMemset", ROM0

; Writes a value to all bytes in an area of memory
; Works when the destination is in VRAM, even while the LCD is on
; @param hl Beginning of area to fill
; @param bc Amount of bytes to write (0 causes 65536 bytes to be written)
; @param a Value to write
; @return bc 0
; @return hl Pointer to the byte after the last written one
; @return d Equal to parameter passed in a
; @return a 0
; @return f Z set, C reset
LCDMemset::
	ld d, a
; Writes a value to all bytes in an area of memory
; Works when the destination is in VRAM, even while the LCD is on
; @param hl Beginning of area to fill
; @param bc Amount of bytes to write (0 causes 65536 bytes to be written)
; @param d Value to write
; @return bc 0
; @return hl Pointer to the byte after the last written one
; @return a 0
; @return f Z set, C reset
LCDMemsetFromD::
	wait_vram
	ld a, d
	ld [hli], a
	dec bc
	ld a, b
	or c
	jr nz, LCDMemsetFromD
	ret

SECTION "LCDMemcpySmall", ROM0

; Copies a block of memory somewhere else
; Works when the source or destination is in VRAM, even while the LCD is on
; @param de Pointer to beginning of block to copy
; @param hl Pointer to where to copy (bytes will be written from there onwards)
; @param c Amount of bytes to copy (0 causes 256 bytes to be copied)
; @return de Pointer to byte after last copied one
; @return hl Pointer to byte after last written one
; @return c 0
; @return a Last byte copied
; @return f Z set, C reset
LCDMemcpySmall::
	wait_vram
	ld a, [de]
	ld [hli], a
	inc de
	dec c
	jr nz, LCDMemcpySmall
	ret

SECTION "LCDMemcpy", ROM0

; Copies a block of memory somewhere else
; Works when the source or destination is in VRAM, even while the LCD is on
; @param de Pointer to beginning of block to copy
; @param hl Pointer to where to copy (bytes will be written from there onwards)
; @param bc Amount of bytes to copy (0 causes 65536 bytes to be copied)
; @return de Pointer to byte after last copied one
; @return hl Pointer to byte after last written one
; @return bc 0
; @return a 0
; @return f Z set, C reset
LCDMemcpy::
	wait_vram
	ld a, [de]
	ld [hli], a
	inc de
	dec bc
	ld a, b
	or c
	jr nz, LCDMemcpy
	ret
