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

include "include/hardware.inc/hardware.inc"
include "include/macros.inc"
include "include/defines.inc"
include "res/block_enum.inc"

SECTION "BlockCode", ROM0

RunBlockPrize::
RunBlockLock::
RunBlockBomb::
RunBlockFire::
RunBlockToggleButton::
RunBlockSpring::
RunBlockExit::
RunBlockTeleporter::
	ret
RunBlockKey::
RunBlockDot::
	ld a, BlockType_Floor
	jp BlockChangeForPlayer

RunBlockHeart::
	ld a, 4
	ld [PlayerHealth], a

	; Refill the hearts in the status bar
	ld hl, _SCRN1+14
	wait_vram
	ld a, $F6
	ld [hl+], a
	ld [hl+], a
	ld [hl+], a
	ld [hl+], a

	ld a, BlockType_Floor
	jp BlockChangeForPlayer

RunBlockStar::
	ld a, 255
	ld [PaintAmount], a

	ld a, BlockType_Floor
	jp BlockChangeForPlayer

RunRescueCritter::
	ld a, BlockType_Floor
	jp BlockChangeForPlayer
