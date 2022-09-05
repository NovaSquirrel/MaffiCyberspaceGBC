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

SECTION "BSS", WRAM0


SECTION "Queue", WRAM0, ALIGN[8]
FloodQueueHi::
	ds 256
FloodQueueLo::
	ds 256

SECTION "OAM Data", WRAM0, ALIGN[8]
OamBuffer::
	ds 256

SECTION "Maze data", WRAMX, BANK[1]
Playfield::
	ds 4096
PlayfieldEnd::
