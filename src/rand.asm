;
; Pseudorandom number generator
;
; Copyright 2018, 2020 Damian Yerrick
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

; Modified by NovaSquirrel to use different label names

section "rand_ram",WRAM0
randstate: ds 4

; The formula is
; x[i + 1] = (x[i] + 0xB3) * 0x01010101
; or equivalently
; x[i + 1] = x[i] * 0x01010101 + 0xB3B3B3B3
; Prior to cc65 commit 3994fee595 it was
; x[i + 1] = x[i] * 0x01010101 + 0x31415927

section "rand",ROM0

;;
; Generates a pseudorandom 16-bit integer in BC
; using the LCG formula from cc65 rand():
; x[i + 1] = x[i] * 0x01010101 + 0xB3B3B3B3
; @return A=B=state bits 31-24 (which have the best entropy),
; C=state bits 23-16, HL trashed
RandomByteLCG::
  ; Add 0xB3 then multiply by 0x01010101
  ld hl, randstate+0
  ld a, [hl]
  add a, $B3
  ld [hl+], a
  adc a, [hl]
  ld [hl+], a
  adc a, [hl]
  ld [hl+], a
  ld c, a
  adc a, [hl]
  ld [hl], a
  ld b, a
  ret

;;
; Sets the random seed to BC.
; C expects startup code to behave as if srand(1) was called.
; AHL trashed
SeedRandomLCG::
  ld hl,randstate+3
  xor a
  ld [hl-],a
  ld [hl-],a
  ld a,b
  ld [hl-],a
  ld [hl],c
  ret

;
; According to tools/rand.py, after srand(1) then ten rand() calls,
; first ten BC results should be
; b4b4 85d1 8e08 9b0d 2d92
; 794b 64eb 8a25 35ab 6731
; Verify this with SHOW_RNG in placeholder.z80