;
; Super Game Boy driver for GB port of Magic Floor
;
; Copyright 2020 Damian Yerrick
;
; This software is provided 'as-is', without any express or implied
; warranty.  In no event will the authors be held liable for any damages
; arising from the use of this software.
; 
; Permission is granted to anyone to use this software for any purpose,
; including commercial applications, and to alter it and redistribute it
; freely, subject to the following restrictions:
; 
; 1. The origin of this software must not be misrepresented; you must not
;    claim that you wrote the original software. If you use this software
;    in a product, an acknowledgment in the product documentation would be
;    appreciated but is not required.
; 2. Altered source versions must be plainly marked as such, and must not be
;    misrepresented as being the original software.
; 3. This notice may not be removed or altered from any source distribution.
;
; Original source code: https://github.com/pinobatch/libbet
; Modified extensively to work outside of that game, and support this game's needs
; Code taken from sgb.z80, ppuclear.z80, and pads.z80

include "include/hardware.inc/hardware.inc"
INCLUDE "include/defines.inc"
INCLUDE "include/macros.inc"

DEF SGB_COMMAND_PAL01    = $00
DEF SGB_COMMAND_PAL23    = $01
DEF SGB_COMMAND_PAL03    = $02
DEF SGB_COMMAND_PAL12    = $03
DEF SGB_COMMAND_ATTR_BLK = $04
DEF SGB_COMMAND_ATTR_LIN = $05
DEF SGB_COMMAND_ATTR_DIV = $06
DEF SGB_COMMAND_ATTR_CHR = $07
DEF SGB_COMMAND_SOUND    = $08
DEF SGB_COMMAND_SOU_TRN  = $09
DEF SGB_COMMAND_PAL_SET  = $0A
DEF SGB_COMMAND_PAL_TRN  = $0B
DEF SGB_COMMAND_ATRC_EN  = $0C
DEF SGB_COMMAND_TEST_EN  = $0D
DEF SGB_COMMAND_ICON_EN  = $0D
DEF SGB_COMMAND_DATA_SND = $0F
DEF SGB_COMMAND_DATA_TRN = $10
DEF SGB_COMMAND_MLT_REQ  = $11
DEF SGB_COMMAND_JUMP     = $12
DEF SGB_COMMAND_CHR_TRN  = $13
DEF SGB_COMMAND_PCT_TRN  = $14
DEF SGB_COMMAND_ATTR_TRN = $15
DEF SGB_COMMAND_ATTR_SET = $16
DEF SGB_COMMAND_MASK_EN  = $17
DEF SGB_COMMAND_OBJ_TRN  = $18
DEF SGB_COMMAND_PAL_PRI  = $19

section "sgbwram", WRAM0
sgb_cmd_buf:: ds 16
IsSuperGameBoy:: ds 1

section "sgbcode", ROM0

; Reads the controller but doesn't do anything with it
read_pad_and_discard:
  ; Poll half the controller
  ld a,P1F_GET_BTN
  call .onenibble
  ld b,a  ; B7-4 = 1; B3-0 = unpressed buttons

  ; Poll the other half
  ld a,P1F_GET_DPAD
  call .onenibble

  ; And release the controller
  ld a,P1F_GET_NONE
  ldh [rP1],a
  ret

.onenibble:
  ldh [rP1],a     ; switch the key matrix
  call .knownret  ; burn 10 cycles calling a known ret
  ldh a,[rP1]     ; ignore value while waiting for the key matrix to settle
  ldh a,[rP1]
  ldh a,[rP1]     ; this read counts
  or $F0   ; A7-4 = 1; A3-0 = unpressed keys
.knownret:
  ret

;;
; Busy-waits for blanking and turns off rendering.
;
; The Game Boy PPU halts entirely when rendering is off.  Stopping
; the signal outside vblank confuses the circuitry in the LCD panel,
; causing it to get stuck on a scanline.  This stuck state is the
; same as the dark horizontal line when you turn off the Game Boy.
;
; Turning rendering on, by contrast, can be done at any time and
; is done by writing the nametable base addresses and sprite size
; to rLCDC with bit 7 set to true.
; @return A = 0
lcd_off_busywait::
  call busy_wait_vblank

  xor a
  ldh [rLCDC],a
  ret

;;
; Busy-wait for being out of vblank.  Use this for game loop timing
; if interrupts aren't in use yet.
wait_not_vblank::
  ldh a, [rLY]
  cp 144
  jr nc, wait_not_vblank
  ret

;;
; Waits for forced blank (rLCDC bit 7 clear) or vertical blank
; (rLY >= 144).  Caution: this is a "room heater", best used before
; clearing rLCDC bit 7.
busy_wait_vblank::
  ; If rLCDC bit 7 already clear, we're already in forced blanking
  ldh a,[rLCDC]
  rlca
  ret nc

  ; Otherwise, wait for LY to become 144 through 152.
  ; Most of line 153 is prerender, during which LY reads back as 0.
.wait:
  ldh a, [rLY]
  cp 144
  jr c, .wait
  ret


; Super Game Boy detection ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
; Sets IsSuperGameBoy to 1 if the player number is 2 or 4.
; rP1 must be $30 (key matrix released), such as after sgb_send or
; read_pad_and_discard.  When this happens, SGB returns 4 - player number in
; bits 1-0, where 3 means player 1, 2 means player 2, etc.
; Fully reading advances to the next player.
capa_1_if_player_2:
  ldh a, [rP1]
  rra  ; players 1 or 3: carry set; players 2 or 4: carry clear
  ret c
  ld a, $01
  ld [IsSuperGameBoy],a
  ret
;;
; Sets IsSuperGameBoy to $01 if on a Super Game Boy.
detect_sgb::
  ; Make sure it's zero
  xor a
  ld [IsSuperGameBoy], a
  ; Try to set the SGB to 2-player mode
  di
  ld b, 30  ; Takes the SGB a few frames to warm up
  .sgbwarmupwait:
    call wait_not_vblank
    call busy_wait_vblank
    dec b
    jr nz, .sgbwarmupwait
  ld b, 1
  call sgb_set_bplus1_players
  call sgb_wait
  call capa_1_if_player_2
  call read_pad_and_discard
  call capa_1_if_player_2

  ; Now turn off 2-player mode
  ld b, 0
  fallthrough sgb_set_bplus1_players

;;
; Set the number of controllers to read to B + 1, where B is
; 0, 1, or 3 for 1, 2, or 4 (multitap only) players.
sgb_set_bplus1_players::
  ld a, (SGB_COMMAND_MLT_REQ << 3) + 1
  fallthrough sgb_send_ab

;;
; Send a 1-packet SGB command whose first two bytes are A and B
; and whose remainder is zero filled.
sgb_send_ab::
  ld c, 0
  fallthrough sgb_send_abc

;;
; Send a 1-packet SGB command whose first three bytes are A, B, and C
; and whose remainder is zero filled.
sgb_send_abc::
  ld hl, sgb_cmd_buf
  push hl
  ld [hl+], a
  ld a, b
  ld [hl+], a
  ld a, c
  ld [hl+], a
  xor a
  ld c, 13
  rst MemsetSmall
  pop hl
  jr sgb_send

def SIZEOF_SGB_PACKET EQU 16
def CHAR_BIT EQU 8

;;
; Clears the Super Game Boy attribute table to 0.
clear_sgb_attr::
  ld hl, sgb_cmd_clear_attrs
  fallthrough sgb_send_if_sgb
sgb_send_if_sgb::
  ld a, [IsSuperGameBoy]
  rra
  ret nc
  fallthrough sgb_send

;;
; Sends a Super Game Boy packet starting at HL.
; Assumes no IRQ handler does any sort of controller autoreading.
sgb_send::

  ; B: Number of remaining bytes in this packet
  ; C: Number of remaining packets
  ; D: Remaining bit data
  ld a,$07
  and [hl]
  ret z
  ld c,a

.packetloop:
  call sgb_send_immediate
  call sgb_wait
  dec c
  jr nz,.packetloop
  ret

;;
; Waits about 4 frames for Super Game Boy to have processed a command
; 4 frames is 4*154 = 616 scanlines or 4*154*114 = 70224 M-cycles.
; Each iteration of the inner loop takes 4 cycles.
; Thus we wait 4*154*114/4 = 17556 iterations
sgb_wait:
  ld de, 65536 - (114 * 154 * 4 / 4)
.loop:
  inc e
  jr nz, .loop
  inc d
  jr nz, .loop
  ret

;;
; Immediately sends a single 16-byte packet to the SGB ICD2.
; Useful in fade commands
sgb_send_immediate::
  ; Start transfer by asserting both halves of the key matrix
  ; momentarily.  (This is like strobing an NES controller.)
  xor a
  ldh [rP1],a
  ld a,$30
  ldh [rP1],a
  ld b,SIZEOF_SGB_PACKET
.byteloop:
  ld a,[hl+]  ; Read a byte from the packet

  ; Put bit 0 in carry and the rest (and a 1) into D.  Once this 1
  ; is shifted out of D, D is 0 and the byte is finished.
  ; (PB16 and IUR use the same principle for loop control.)
  scf      ; A = hgfedcba, CF = 1
  rra      ; A = 1hgfedcb, CF = a
  ld d,a
.bitloop:
  ; Send a 1 as $10 then $30, or a 0 as $20 then $30.
  ; This is constant time thanks to ISSOtm, unlike SGB BIOS
  ; which takes 1 mcycle longer to send a 0 then a 1.
  ld a,$10
  jr c, .bitIs1
  add a,a ; ld a,$20
.bitIs1:
  ldh [rP1],a
  ld a,$30
  ldh [rP1],a

  ldh a, [rIE]  ; Burn 3 cycles to retain original loops's speed

  ; Advance D to next bit (this is like NES MMC1)
  srl d
  jr nz,.bitloop
  dec b
  jr nz,.byteloop

  ; Send $20 $30 as end of packet
  ld a,$20
  ldh [rP1],a
  ld a,$30
  ldh [rP1],a
  ret

; SGB palette commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

sgb_freeze:
  ; Freeze display while doing transfers
  call busy_wait_vblank
  ld a, SGB_COMMAND_MASK_EN*8+1
  ld b, $01  ; Freeze current screen
  jr sgb_send_ab

;;
; Turns off the LCD, sets scroll to 0, sets BGP to identity ($E4),
; and sets up an identity tilemap in _SCRN0 for Super Game Boy
; *_TRN commands.
; Rewritten by Nova
sgb_load_trn_tilemap:
  call lcd_off_busywait
  ld a, %11100100
  ldh [rBGP], a

  ld hl, _SCRN0
  xor a
  ldh [rSCX], a
  ldh [rSCY], a
  ld b, 20
  ld de, 32-20
.loop:
  ld [hl+], a
  dec b
  jr nz, :+
    ld b, 20
    add hl, de
  :
  inc a
  jr nz, .loop
  ret

sgb_send_pal_trn:
  ; Do the transfer
  ld a, SGB_COMMAND_PAL_TRN*8+1
  ld b, 0
  fallthrough sgb_send_trn_ab

;;
; Turns on rendering, sends a *_TRN packet with first two bytes
; A and B, and turns rendering back off.
sgb_send_trn_ab:
  ld l, a
  ld a,LCDCF_ON|LCDCF_BGON|LCDCF_BG8000|LCDCF_BG9800
  ldh [rLCDC],a
  ld a, l
  call sgb_send_ab
  jp lcd_off_busywait

;;
; Sets Super Game Boy palettes and unfreezes the display.
; Four consecutive palettes are chosen: B, B+1, B+2, B+3.
; Does nothing on non-SGB.
; @param B index into last PAL_TRN for subpalette 0
sgb_set_palettes_from_b::
  ld c, b
  inc c
  ld d, c
  inc d
  ld e, d
  inc e
  jr sgb_set_palettes_bcde

sgb_set_palette_gray::
  ld b, 32
  fallthrough sgb_set_palette_b

;;
; Sets Super Game Boy palettes and unfreezes the display.
; Does nothing on non-SGB.
; @param B index into last PAL_TRN for all four subpalettes
sgb_set_palette_b::
  ld c, b
  ld d, b
  ld e, b
  fallthrough sgb_set_palettes_bcde

;;
; Sets Super Game Boy palettes and unfreezes the display.
; Does nothing on non-SGB.
; @param B index into last PAL_TRN for subpalette 0
; @param C index into last PAL_TRN for subpalette 1
; @param D index into last PAL_TRN for subpalette 2
; @param E index into last PAL_TRN for subpalette 3
sgb_set_palettes_bcde::
  ld a, [IsSuperGameBoy]
  rra
  ret nc
  ld a, $40
  fallthrough sgb_set_palettes_bcde_attr_a

;;
; Sets Super Game Boy palettes defined through PAL_TRN
; and optionally loads an attribute table defined through ATTR_TRN.
; @param B index into last PAL_TRN for subpalette 0
; @param C index into last PAL_TRN for subpalette 1
; @param D index into last PAL_TRN for subpalette 2
; @param E index into last PAL_TRN for subpalette 3
; @param A bit 7: load attribute table whose index is in bits 0-5;
;   bit 6: unfreeze display
sgb_set_palettes_bcde_attr_a::
  ld hl, sgb_cmd_buf
  push hl
  push af
  push bc

  ; Clear unused bytes of packet
  push hl
  xor a
  ld c, 16
  rst MemsetSmall
  pop hl

  ; Populate packet
  ld a, SGB_COMMAND_PAL_SET*8+1
  ld [hl+], a
  pop bc
  ld a, b
  ld [hl+], a
  inc hl
  ld a, c
  ld [hl+], a
  inc hl
  ld a, d
  ld [hl+], a
  inc hl
  ld a, e
  ld [hl+], a
  inc hl
  pop af  ; A = unfreeze and predefuattribute table command
  ld [hl+], a

  ; And send it to the SGB
  pop hl
  jp sgb_send

; Border ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The border consists
; Up to 64 tiles in Super NES format, compressed with PB16
; 28 rows of tilemap data, compressed with PB16
; 32 bytes: palette

sgb_load_border::
  call sgb_freeze
  call sgb_load_trn_tilemap

  ; Load tiles
  ld de, sgbborder
  ld a, [de]
  inc de
  ld b, a
  ld hl, _VRAM8000
  call pb16_unpack_block
  ; ld b, 0  ; guaranteed by pb16_unpack_block
  push de
  ld a, SGB_COMMAND_CHR_TRN<<3|1
  call sgb_send_trn_ab
  pop de

  ; Load map data
  ld b, 32*28/16
  ld hl, _VRAM8800
  call pb16_unpack_block
  push de

  ld bc, -32*28
  ld hl, _VRAM8800
  ld de, _VRAM8000
  .tilemaploop:
    ; Expand each tilemap byte on this row to a tile number and
    ; a flip attribute
    ld a, [hl]
    and $3F
    ld [de], a  ; tile number
    inc de
    xor [hl]
    inc hl
    or $10
    ld [de], a  ; attributes: VH001PP0
    inc de
    inc c
    jr nz, .tilemaploop
    inc b
    jr nz, .tilemaploop

  pop de
  ; And last: the palette
  ld c, 32
  ld hl, _VRAM8800
  rst MemcpySmall
  ld b, 0

  ; Push tilemap and palette to the SGB
  ld a, SGB_COMMAND_PCT_TRN<<3|1
  jp sgb_send_trn_ab


;--------------------------------------
SetupSGB::
	ld a, [IsSuperGameBoy]
	or a
	ret z

	; TODO: display something first
	call sgb_freeze

	call sgb_load_trn_tilemap

	; Copy in the palettes
	ld hl, sgb_palettes
	ld de, _VRAM
	ld c, sgb_palettes_end - sgb_palettes
	call memcpy8
	call sgb_send_pal_trn

	; Copy in the attribute screens
	ld hl, sgb_attributes
	ld de, _VRAM
	ld c, sgb_attributes_end - sgb_attributes
	call memcpy
	ld a, SGB_COMMAND_ATTR_TRN<<3|1
	ld b, 0
	call sgb_send_trn_ab

	; Set the palettes and attribute screen
	ld b, 0
	ld c, 1
	ld d, 2
	ld e, 3
	ld a, %11000001
	call sgb_set_palettes_bcde_attr_a

	jp lcd_off_busywait

section "sgb_palettes", ROM0

sgb_cmd_clear_attrs::
	db SGB_COMMAND_ATTR_BLK*8+1  ; 1 packet holds up to 2 rectangles
	db 1        ; number of rectangles

	db %00000111  ; bit 0: inside, 1: border, 2: outside
	db %00000000  ; inside and border to palette 0, outside to palette 0
	db 0, 0, 19, 17

	db 0, 0, 0, 0, 0, 0, 0, 0

;--------------------------------------
sgb_palettes:
	; Main
	rgb 31, 31, 31
	rgb $59/8, $cf/8, $93/8
	rgb $6a/8, $88/8, $e9/8
	rgb 0, 0, 0

	; Critters left to find (yellow/orange)
	rgb 31, 31, 31
	rgb $f8/8, $c5/8, $3a/8
	rgb $e8/8, $8a/8, $36/8
	rgb 0, 0, 0

	; Paint bar
	rgb 31, 31, 31
	rgb $9c/8,$8b/8,$db/8
	rgb $78/8,$64/8,$c6/8
	rgb $49/8,$41/8,$82/8

	; Health
	rgb 31, 31, 31
	rgb $e2/8,$72/8,$85/8
	rgb $b2/8,$52/8,$66/8
	rgb 0, 0, 0
sgb_palettes_end:

sgb_attributes:
; ATF 0: blank
	rept 18
	db 0, 0, 0, 0, 0
	endr

; ATF 1: gameplay
	rept 17
	db 0, 0, 0, 0, 0
	endr
	db %01010101, %01011010, %10101010, %10101111, %11111111
sgb_attributes_end:




sgbborder: db 0 ;incbin "obj/gb/sgbborder.border"
