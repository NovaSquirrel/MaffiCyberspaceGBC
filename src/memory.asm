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

SECTION "HRAM", HRAM
seed::  ds 4 ; Random number state
temp1:: ds 1 ; Local variables
temp2:: ds 1
temp3:: ds 1
temp4:: ds 1
temp5:: ds 1
temp6:: ds 1
temp7:: ds 1
temp8:: ds 1
temp9:: ds 1
temp10:: ds 1
temp11:: ds 1
temp12:: ds 1
IsNotGameBoyColor:: ds 1 ; Zero if it IS a Game Boy Color, nonzero if not
framecount:: ds 1
RunOamDMA::  ds 8 ; OAM DMA routine
OAMWrite:: ds 1 ; OAM write pointer
KeyDown:: ds 1
KeyLast:: ds 1
KeyNew::  ds 1

PlayerPXL:: ds 1
PlayerPXH:: ds 1
PlayerPYL:: ds 1
PlayerPYH:: ds 1
CameraX::   ds 2
CameraY::   ds 2
NegativeCameraX::   ds 2
NegativeCameraY::   ds 2
LYC_Interrupt_LCDC:: ds 1 ; Value to write to LCDC when the LYC interrupt happens

HoldingPaintButton:: ds 1

; 8-bit collision coordinates, relative to the screen
PlayerCollisionX::     ds 1
PlayerCollisionY::     ds 1
ProjectileCollisionX:: ds 1
ProjectileCollisionY:: ds 1
EnemyCollisionX::      ds 1
EnemyCollisionY::      ds 1
RollingCollisionX::    ds 1
RollingCollisionY::    ds 1

SECTION "BSS", WRAM0
KeyRepeat::    ds 1
PlayerDrawDirection:: ds 1
PlayerAnimationFrame:: ds 1        ; Current wanted frame
PlayerAnimationFrameInVRAM:: ds 1  ; Current frame that's in VRAM
CameraXPixel:: ds 2
CameraYPixel:: ds 2
DoUpdateRow::    ds 1
DoUpdateColumn:: ds 1
ParallaxSource:: ds 2


LevelID:: ds 1

PlayerHealth:: ds 1

PaintAmount::                 ds 1 ; The paint bar amount
ZeroWhenLevelStarts:: ;------------------------------------
PaintAmountShownOnscreen::    ds 1 ; For detecting when to update the bar
PaintShootDirection::         ds 1 ; 0=right, 1=down right, 2=down, etc.
PaintShootDiagonalDirection:: ds 1
PaintShootDirectionLock::     ds 1
PaintShotID::                 ds 1 ; Increments every shot
DMG_PlayerAnimationFrame_Page:: ds 1 ; For double buffering
PreviousOAMWrite::            ds 1
DMG_PlayerDrawDirection::     ds 1 ; Set when the animation frame copy finishes; used for the actual positioning/flipping decisions in DrawPlayer
DieAnimationActive::          ds 1
DieAnimationVYL::             ds 1
DieAnimationPYL::             ds 1
DieAnimationPYH::             ds 1
DieAnimationVXL::             ds 1
DieAnimationPXL::             ds 1
DieAnimationPXH::             ds 1
HintTarget::                  ds 1 ; hhhttttt, where the top bits are the actual target and t is the timer

; Timers - note the code in player.asm that will need to be updated if this is changed around
PaintShootDiagonalTimer:: ds 1
PaintShootingTimer::      ds 1
PlayerInvincibleTimer::   ds 1
PaintRefillCooldown::     ds 1
PlayerNotMovingTimer::    ds 1
ZeroWhenLevelStarts_End:: ;--------------------------------


; Variables for the alternative player frame streaming that's used when DMA is not available
DMG_PlayerAnimationFrame_InProgress::   ds 1 ; If 1, write the second set of tiles
DMG_PlayerAnimationFrame_Destination1:: ds 1 ; Low byte of the address of the first tile to write (high byte is always $80)
DMG_PlayerAnimationFrame_Source1::      ds 2 ; Address of the first tile to copy into the destination
DMG_PlayerAnimationFrame_Destination2:: ds 1 ; Low byte of the address of the second tile to write (high byte is always $80)
DMG_PlayerAnimationFrame_Source2::      ds 2 ; Address of the second tile to copy into the destination
DMG_PlayerAnimationFrame_Destination3:: ds 1 ; Low byte of the address of the third tile to write (high byte is always $80)
DMG_PlayerAnimationFrame_Source3::      ds 2 ; Address of the third tile to copy into the destination
DMG_BufferedPlayerDrawDirection::       ds 1 ; Set when the animation frame copy starts

EnemyCount:: ds 1

HaveGameplayGraphicsInVRAM:: ds 1

MazeExitX:: ds 1
MazeExitY:: ds 1
CritterXYList:: ds 7*2 ; Must not have more than 7
RescueCritterCount:: ds 1
RescueCritterOriginalCountTimes32:: ds 1

UseBrighterPalettes:: ds 1 ; Use an alternative, brighter palette, for darker screens

VblankIndirectJump:: ds 3 ; Should be C3, address

SECTION "ParallaxRAM", WRAM0, ALIGN[4]
ParallaxShifts::
	ds 256    ; 16 * 8 (with each tile repeated twice)

SECTION "Stack", WRAM0
Stack::
	ds 256
StackEnd::

SECTION "Queue", WRAM0, ALIGN[9]
	UNION
; Flood fill mode
FloodQueueHi::
	ds 256
	NEXTU
; Gameplay mode (used for actors)
ActorData::
	ds 256 ; 16 entries of 16 bytes each
	ENDU

; -------------------------------------

	UNION
; Flood fill mode
FloodQueueLo::
	ds 256
; These will be aligned to the start of a page
PlaceholderPointers::
	ds 8*4
PlaceholderPointers_End::

	NEXTU
; Gameplay mode
ActorExtra::
	ds 256 ; 16 entries of 16 bytes each - "set 0,h" to get the other variables
PlayerProjectiles::
	ds ACTOR_SIZE * PLAYER_PROJECTILE_COUNT
PlayerProjectilesEnd::
	ENDU

SECTION "OAM Data", WRAM0, ALIGN[8]
OamBuffer::
	ds 256

SECTION "Maze data", WRAMX, BANK[1]
Playfield::
	ds 4096
PlayfieldEnd::

SECTION "GBC data", WRAMX, BANK[2], ALIGN[8]
BG_Palette_24bit_Current::  ds 8*4*3
OBJ_Palette_24bit_Current:: ds 8*3*3
	ds (256 - 8*4*3 - 8*3*3)
BG_Palette_24bit_Delta::    ds 8*4*3
OBJ_Palette_24bit_Delta::   ds 8*3*3
	ds (256 - 8*4*3 - 8*3*3)
BG_Palette_24bit_Target::  ds 8*4*3
OBJ_Palette_24bit_Target:: ds 8*3*3
	ds (256 - 8*4*3 - 8*3*3)
BG_Palette_15bit::         ds 8*4*2
OBJ_Palette_15bit::        ds 8*3*2
	ds (256 - 8*4*2 - 8*3*2)