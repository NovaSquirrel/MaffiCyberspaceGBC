SECTION "HRAM", HRAM
seed::  ds 4
temp1:: ds 1
temp2:: ds 1
temp3:: ds 1
temp4:: ds 1
temp5:: ds 1
temp6:: ds 1
temp7:: ds 1
temp8:: ds 1
IsGameBoyColor:: ds 1
framecount:: ds 1
RunOamDMA::  ds 8 ; OAM DMA routine
OamWrite:: ds 1 ; OAM write pointer
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

SECTION "BSS", WRAM0
KeyRepeat::    ds 1
PlayerDrawDirection:: ds 1
CameraXPixel:: ds 2
CameraYPixel:: ds 2

SECTION "Queue", WRAM0, ALIGN[8]
	UNION     ; Flood fill mode
FloodQueueHi::
	ds 256

	NEXTU     ; Gameplay mode

	ENDU

; -------------------------------------

	UNION     ; Flood fill mode
FloodQueueLo::
	ds 256

	NEXTU     ; Gameplay mode

	ENDU

SECTION "OAM Data", WRAM0, ALIGN[8]
OamBuffer::
	ds 256

SECTION "Maze data", WRAMX, BANK[1]
Playfield::
	ds 4096
PlayfieldEnd::
