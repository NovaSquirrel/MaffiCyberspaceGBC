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
include "include/defines.inc"
include "include/hardware.inc/hardware.inc"

include "res/block_enum.inc"
include "res/actor_enum.inc"

SECTION "MainLoop", ROM0

; -----------------------------------------------------------------------------

;PlayerAnimationFrame

StartLevel::
	call GenerateMaze

	; Initialize actor data
	ld hl, ActorData
	ld bc, 512
	call memclear

	; Test enemy
	;ld a, ActorType_EnemySpawning
	;ld [ActorData], a
	;ld a, 36
	;ld [ActorData + actor_pyh], a
	;ld [ActorData + actor_pxh], a
	;ld a, $80
	;ld [ActorData + actor_pyl], a
	;ld [ActorData + actor_pxl], a
	;ld [ActorData + actor_pyl + ACTOR_SIZE], a
	;ld [ActorData + actor_pxl + ACTOR_SIZE], a
	;ld a, $10
	;ld [ActorData + actor_health], a
	;ld [ActorData + actor_health + ACTOR_SIZE], a

	;ld a, ActorType_EnemySpawning
	;ld [ActorData + ACTOR_SIZE], a
	;ld a, 28
	;ld [ActorData + actor_pyh + ACTOR_SIZE], a
	;ld [ActorData + actor_pxh + ACTOR_SIZE], a

	ld a, 7
	ldh [rWX], a ; X position + 7, so 7 is writing 0
	ld a, 144-8
	ldh [rWY], a
	; Do an interrupt at the bottom of the screen
	ldh [rLYC], a
	ld a, STATF_LYC
	ldh [rSTAT], a
	ld a, LCDCF_ON|LCDCF_OBJ16|LCDCF_OBJOFF|LCDCF_BGON|LCDCF_BG8800|LCDCF_WIN9C00|LCDCF_WINON
	ldh [LYC_Interrupt_LCDC], a

	ld a, 255
	ld hl, PaintAmount
	ld [hl+], a ; PaintAmount
	xor a
	ld [hl+], a ; PaintAmountShownOnscreen
	ld [hl+], a ; PaintShootDirection
	ld [hl+], a ; PaintShootDiagonalDirection
	ld [hl+], a ; PaintShootDirectionLock
	ld [hl+], a ; PaintShotID
	ld [hl+], a ; PlayerShootDiagonalTimer
	ld [hl+], a ; PlayerShootingTimer

	; Clear all of OAM
	ld hl, OamBuffer
	ld c, 0
	call memclear8
	ld a, OamBuffer>>8
	call RunOamDMA

	; On Game Boy Color, get rid of the face overlay tiles to make room for the player
	ldh a, [IsNotGameBoyColor]
	or a
	jr z, :+
		ld hl, _VRAM8000
		ld c, 0
		call memclear8
	:

Gameplay::
; -------------------------------------------------------------------------
; Main loop
	ld a, 32
	ldh [PlayerPXH], a
	ldh [PlayerPYH], a
	ld a, 255
	ld [DoUpdateRow], a
	ld [DoUpdateColumn], a
	xor a
	ld [PlayerAnimationFrame], a
	ldh [OAMWrite], a
	dec a
	ld [PlayerAnimationFrameInVRAM], a ; Initialize it with -1 so it'll get sent no matter what

	ld a, LOW(ParallaxShifts)
	ld [ParallaxSource+0], a
	ld a, HIGH(ParallaxShifts)
	ld [ParallaxSource+1], a

	; Status bar
	ld hl, _SCRN1
	ld a, $f0
	ld c, 20
	call memset8
	ld hl, _SCRN1+2 ; Critters left
	ld a, $f8
	ld [hl+], a
	ld a, $f9
	ld [hl+], a
	ld hl, _SCRN1+6 ; Paint bar
	ld a, $f5
	ld c, 8
	call memset8
	inc l
	ld a, $f6       ; Hearts
	ld c, 4
	call memset8

;	ld a, 1
;	ldh [rVBK], a
;	ld hl, _SCRN1
;	ld a, 7
;	ld c, 20
;	call memset8
;	xor a
;	ldh [rVBK], a

	call InitCamera

	ld a, BANK(RenderLevelScreen)
	ld [rROMB0], a
	call RenderLevelScreen

	call ScreenOn
forever:
	; .----------------------
	; | Pre-vblank tasks
	; '----------------------

	; On DMG, skip over the DMA code
	ldh a, [IsNotGameBoyColor]
	or a
	jp nz, PreVblankForDMG

	; Calculate a pointer to the frame graphics
	ld a, [PlayerAnimationFrameInVRAM] ; <-- Check if the graphics are already present, which means the copy can be skipped
	ld b, a
	ld a, [PlayerAnimationFrame]
	cp b
	jr nz, :+
		call WaitVblank
		jr AfterVblankForDMG
	:
	ld [PlayerAnimationFrameInVRAM], a
	add a, a ; * 2
	add a, a ; * 4
	add a, a ; * 8
	ld h, 0
	ld l, a
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	ld de, PlayerAnimationFrameGraphics+16
	add hl, de
	push hl

	; .----------------------
	; | Vblank
	; '----------------------
	call WaitVblank

	; Copy in the player frame
	ld a, BANK(PlayerAnimationFrameGraphics)
	ld [rROMB0], a
	pop hl
	ld a, h
	ldh [rHDMA1], a
	ld a, l
	ldh [rHDMA2], a
	ld a, HIGH($8010)
	ldh [rHDMA3], a
	ld a, LOW($8010)
	ldh [rHDMA4], a
	ld a, 7-1
	ldh [rHDMA5], a
	
AfterVblankForDMG: ; The DMG-specific code will jump here once it's done

	; Set scroll
	ld a, [CameraXPixel]
	ldh [rSCX], a
	ld a, [CameraYPixel]
	ldh [rSCY], a
	; Turn on rendering (reenabling sprites)
	ld a, LCDCF_ON|LCDCF_OBJ16|LCDCF_OBJON|LCDCF_BGON|LCDCF_BG8800|LCDCF_WIN9C00|LCDCF_WINON
	ldh [rLCDC],a

	ld a, OamBuffer>>8
	call RunOamDMA

	; Parallax upload
	ld hl, ParallaxSource
	ld a, [hl+]
	ld h, [hl]
	ld l, a
	ld de, $9000
	ld b, 2
:	; Semi-unrolled loop
	call WriteOneTile

	ld a, BANK(UpdateRow)
	ld [rROMB0], a

	; Ideally the row/column updates happen in vblank, but may spill over
	ld a, [DoUpdateRow]
	rlca
	jr c, :+
		rrca
		call UpdateRow
	:

	ld a, [DoUpdateColumn]
	rlca
	jr c, :+
		rrca
		call UpdateColumn
	:

	; .----------------------
	; | Clean up after vblank
	; '----------------------
	ld a, 255
	ld [DoUpdateRow], a
	ld [DoUpdateColumn], a

	; .----------------------
	; | Game logic
	; '----------------------
	call ReadKeys

	xor a
	ldh [OAMWrite], a

	ld a, BANK(RunPlayer)
	ld [rROMB0], a
	call RunPlayer

	call AdjustCamera

	ld a, BANK(DrawPlayer)
	ld [rROMB0], a
	call DrawPlayer
	call DrawPaintBar

	ld a, BANK(RunActors)
	ld [rROMB0], a
	call RunActors

	call ClearPreviouslyUsedOAM
	ldh a, [OAMWrite]
	ld [PreviousOAMWrite], a


	ld a, [framecount]
	and 63
	jr nz, .NoSpawnEnemy
		call SpawnEnemy
	.NoSpawnEnemy:

	; Randomly swap two actors to implement flickering
	call RandomByte
	and $f0
	ld h, HIGH(ActorData)
	ld l, a
	ld a, [framecount]
	swap a
	and $f0
	ld d, HIGH(ActorData)
	ld e, a
	push hl
	push de
	call SwapSixteenBytes
	pop de
	pop hl
	inc h
	inc d
	call SwapSixteenBytes

	jp forever

; Swaps sixteen bytes starting from HL and DE
SwapSixteenBytes:
	rept 15
	ld b, [hl]
	ld a, [de]
	ld [hl+], a
	ld a, b
	ld [de], a
	inc e
	endr
	ld b, [hl]
	ld a, [de]
	ld [hl], a
	ld a, b
	ld [de], a
	ret

; .----------------------------------------------------------------------------
; | DMG-specific vblank tasks
; '----------------------------------------------------------------------------

PreVblankForDMG:
	; If it's in progress keep going!!
	ld a, [DMG_PlayerAnimationFrame_InProgress]
	or a
	jr nz, .KeepGoing

	; Start a new animation frame copy?
	ld a, [PlayerAnimationFrameInVRAM] ; <-- Check if the graphics are already present, which means the copy can be skipped
	ld b, a
	ld a, [PlayerAnimationFrame]
	cp b
	jr nz, :+
		; Allow flipping from left/right instantly
		ld a, [PlayerDrawDirection]
		ld [DMG_BufferedPlayerDrawDirection], a

		call WaitVblank
		jp AfterVblankForDMG
	:

	; Starting a new animation frame copy, so set the variables
	ld [PlayerAnimationFrameInVRAM], a
	ld a, [PlayerDrawDirection]
	ld [DMG_BufferedPlayerDrawDirection], a

.KeepGoing:
	ld a, [DMG_PlayerAnimationFrame_InProgress]
	inc a
	ld [DMG_PlayerAnimationFrame_InProgress], a
	cp 2
	jr z, .SecondHalf
.FirstHalf:
	ld a, $10
	ld [DMG_PlayerAnimationFrame_Destination1], a
	ld a, $30
	ld [DMG_PlayerAnimationFrame_Destination2], a
	ld a, $40
	ld [DMG_PlayerAnimationFrame_Destination3], a

	jr .WasFirstHalf
.SecondHalf:
	ld a, $50
	ld [DMG_PlayerAnimationFrame_Destination1], a
	ld a, $60
	ld [DMG_PlayerAnimationFrame_Destination2], a
	ld a, $70
	ld [DMG_PlayerAnimationFrame_Destination3], a
.WasFirstHalf:

	; Calculate a pointer to the frame graphics
	ld a, [PlayerAnimationFrameInVRAM]
	add a, a ; * 2
	add a, a ; * 4
	add a, a ; * 8
	ld h, 0
	ld l, a
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	ld de, PlayerAnimationFrameGraphics
	add hl, de
	push hl ; Save it two times for the second and third tiles
	push hl

	; Calculate the source addresses using the destination offsets
	ld a, [DMG_PlayerAnimationFrame_Destination1]
	add_hl_a
	ld a, l
	ld [DMG_PlayerAnimationFrame_Source1+0], a
	ld a, h
	ld [DMG_PlayerAnimationFrame_Source1+1], a

	pop hl
	ld a, [DMG_PlayerAnimationFrame_Destination2]
	add_hl_a
	ld a, l
	ld [DMG_PlayerAnimationFrame_Source2+0], a
	ld a, h
	ld [DMG_PlayerAnimationFrame_Source2+1], a

	pop hl
	ld a, [DMG_PlayerAnimationFrame_Destination3]
	add_hl_a
	ld a, l
	ld [DMG_PlayerAnimationFrame_Source3+0], a
	ld a, h
	ld [DMG_PlayerAnimationFrame_Source3+1], a


	ld a, [DMG_PlayerAnimationFrame_Page]
	or a
	jr nz, :+
		ld hl, DMG_PlayerAnimationFrame_Destination1
		set 7, [hl]
		ld hl, DMG_PlayerAnimationFrame_Destination2
		set 7, [hl]
		ld hl, DMG_PlayerAnimationFrame_Destination3
		set 7, [hl]
	:

	; If the second set of tiles is being copied, finish the process
	ld a, [DMG_PlayerAnimationFrame_InProgress]
	cp 2
	jr nz, :+
		ld a, [DMG_PlayerAnimationFrame_Page]
		xor 1
		ld [DMG_PlayerAnimationFrame_Page], a

		xor a
		ld [DMG_PlayerAnimationFrame_InProgress], a	

		ld a, [DMG_BufferedPlayerDrawDirection]
		ld [DMG_PlayerDrawDirection], a
	:

	; .----------------------
	; | Vblank
	; '----------------------

	ld a, BANK(PlayerAnimationFrameGraphics)
	ld [rROMB0], a

	call WaitVblank

	ld hl, DMG_PlayerAnimationFrame_Destination1
	call GetDestinationAndSourceThenWriteOneTile
	ld hl, DMG_PlayerAnimationFrame_Destination2
	call GetDestinationAndSourceThenWriteOneTile
	ld hl, DMG_PlayerAnimationFrame_Destination3
	call GetDestinationAndSourceThenWriteOneTile

	jp AfterVblankForDMG

GetDestinationAndSourceThenWriteOneTile:
	ld d, HIGH(_VRAM8000)
	ld a, [hl+]
	ld e, a
	ld a, [hl+]
	ld h, [hl]
	ld l, a
WriteOneTile:
	rept 15
	ld a, [hl+]
	ld [de], a
	inc e
	endr
	ld a, [hl]
	ld [de], a
	ret

; .----------------------------------------------------------------------------
; | Enemy spawning
; '----------------------------------------------------------------------------
;all_tiles = set()
;for radius in range(80, 100):
;	for angle_i in range(360):
;		angle = angle_i / 360.0 * 2 * math.pi
;		x = math.cos(angle) * radius;
;		y = math.sin(angle) * radius;
;		all_tiles.add((x//16, y//16))
;print(", ".join("%d,%d" % pair for pair in all_tiles))
;print(len(all_tiles))
EnemySpawnLocations: ; There's 88 entries here
	db 4,0, -5,-3, 5,1, -6,1, 1,-6, 0,5, 4,2, 5,3, -6,3, 2,-5, 2,4, -4,-6, -4,3, 4,-5, 5,-4, -6,-4, -5,4, -4,-4, -1,-7, -2,-7, -4,5, -6,-1, 5,-1, -6,-2, 5,-2, -3,-6, 0,-7, -2,-5, -1,-5, -1,4, -2,4, 3,-6, 3,3, 5,0, -6,0, 0,-5, -3,5, 1,5, 6,1, -7,1, 3,-4, 3,5, 5,2, -6,2, -5,1, 4,4, 4,-3, -5,3, -4,-5, -4,4, -6,-3, 5,-3, -5,-4, 1,-7, 0,4, -7,-1, -7,-2, -1,5, -1,6, -2,6, 4,1, -5,-2, -5,-1, -3,-5, -3,4, 1,-5, 2,-6, 1,4, 0,6, -7,0, 3,-5, 3,4, 4,3, -5,0, 1,6, 2,5, 4,-4, -5,2, 6,-1, 6,-2, -1,-6, -2,-6, 4,-2, 4,-1, -5,-5, 0,-6, 6,0, -2,5

SpawnEnemy:
	ld a, [EnemyCount]
	cp 12
	ret nc
	ld b, 20 ; How many tries to do
.TrySpawnEnemy:
:	call RandomByte
	and 127
	cp 88
	jr nc, :-
	add a
	ld hl, EnemySpawnLocations
	add_hl_a

	ldh a, [PlayerPXH]
	add [hl]
	ld d, a
	ldh [temp1], a
	cp 64
	jr nc, .FailSpawnEnemy

	inc hl

	ldh a, [PlayerPYH]
	add [hl]
	ld e, a
	ldh [temp2], a
	cp 64
	jr nc, .FailSpawnEnemy

	ldh a, [temp1] ; X
	ld l, a
	ldh a, [temp2] ; Y
	ld h, a
	call MapPointerLH_XY
	ld a, [hl]
	cp BlockType_Floor
	jr nz, .FailSpawnEnemy

	call FindFreeActorSlot
	ret nc ; Oops there's no room for an enemy anyway
	call ClearActorHL

	ld a, ActorType_EnemySpawning
	ld [hl], a ; HL: actor_type
	switch_hl_to_field actor_type, actor_pyl
	ld [hl], $80   ; actor_pyl
	inc l
	ldh a, [temp2] ; actor_plh
	ld [hl+], a
	ld [hl], $80   ; actor_pxl
	inc l
	ldh a, [temp1]
	ld [hl], a     ; actor_pyh

	inc h
	ld a, l
	and %11110000 ; Move to actor_health
	ld l, a
	ld [hl], $10
	ret
.FailSpawnEnemy:
	dec b
	jr nz, .TrySpawnEnemy
	ret
