include "include/hardware.inc/hardware.inc"

SECTION "Header", ROM0[$100]

	; This is your ROM's entry point
	; You have 4 bytes of code to do... something
	di
	jp EntryPoint

	; Make sure to allocate some space for the header, so no important
	; code gets put there and later overwritten by RGBFIX.
	; RGBFIX is designed to operate over a zero-filled header, so make
	; sure to put zeros regardless of the padding value. (This feature
	; was introduced in RGBDS 0.4.0, but the -MG etc flags were also
	; introduced in that version.)
	ds $150 - @, 0

SECTION "Entry point", ROM0

EntryPoint:
	ld sp, Playfield

	ld a, %11100100
	ldh [rBGP], a

	ld a, 1
	ldh [seed], a
	inc a
	ldh [seed+1], a
	inc a
	ldh [seed+2], a
	inc a
	ldh [seed+3], a
	ld bc, 1234
	call SeedRandomLCG

	call GenerateMaze

	ld a, %00011011
	ldh [rBGP], a

	; Here is where the fun begins, happy coding :)
	jr @
