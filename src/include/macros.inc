; Game Boy assembly macros
; some from https://pastebin.com/fDYFKsfH

; -------------------------------------
; Enum
; -------------------------------------
MACRO enum_start
	IF _NARG == 0
		enum_set 0
	ELSE
		enum_set \1
	ENDC
ENDM

MACRO enum_set
def enum_value = \1
ENDM

MACRO enum_skip
	enum_set (enum_value + 1)
ENDM

MACRO enum_elem
def \1 = enum_value
	enum_skip
ENDM

; -------------------------------------
; 8-bit Utility
; -------------------------------------
MACRO lda
    ld a, \2
    ld \1, a
ENDM

MACRO sex
  rla
  sbc a
ENDM

MACRO neg
	; negate a (x86)
	cpl
	inc a
ENDM

MACRO hswap
	ldh a, \1
	push af
	ldh a, \2
	ldh \1, a
	pop af
	ld \2, a
ENDM

MACRO cooldown
  ld a, \1
  or a
  jr z, .no\@
  dec a
  ld \1, a
.no\@:
ENDM

MACRO ldaq
	; a = carry ? \1 : \2
	; e.g. ldaq 0, 1
	; https://github.com/pret/pokecrystal/wiki/Optimizing-assembly-code#set-a-to-one-constant-or-another-depending-on-the-carry-flag
	if ((\1) == $ff || (\1) == -1) && (\2) == 0
		sbc a
	elif (\1) == 0 && ((\2) == $ff || (\2) == -1)
		ccf
		sbc a
	elif (\1) == 0 && (\2) == 1
		sbc a
		inc a
	elif (\1) == $ff || (\1) == -1
		sbc a
		or \2
	elif (\2) == 0
		sbc a
		and \1
	elif (\1) + 1 == (\2)
		sbc a
		add \2
	elif (\1) + 2 == (\2)
		sbc a
		sbc -(\2)
	elif (\1) == 0
		ccf
		sbc a
		and \2
	elif (\2) == $ff || (\2) == -1
		ccf
		sbc a
		or \1
	elif (\1) - 1 == (\2)
		ccf
		sbc a
		add \1
	elif (\1) - 2 == (\2)
		ccf
		sbc a
		sbc -(\1)
	else
		sbc a
		and (\1) ^ (\2)
		xor \2
	endc
ENDM

; -------------------------------------
; 16-bit Utility
; -------------------------------------
MACRO add_hl_a
	add a,l
	ld l,a
	jr nc, @+3
	inc h
ENDM

MACRO rst_add_hl_a
	rst $08
ENDM

MACRO add_de_a
	add a,e
	ld e,a
	jr nc, @+3
	inc d
ENDM

MACRO add_bc_a
	add a,c
	ld c,a
	jr nc, @+3
	inc b
ENDM

MACRO ld_hl_memory
	ld hl, \1
	ld a, [hli]
	ld h, [hl]
	ld l, a
ENDM

MACRO ld_hl_indirect
	; hl = *hl, little-endian
	; e.g. gethl hl
	ld a, [hli]
	ld HIGH(\1), [hl]
	ld LOW(\1), a
ENDM

MACRO ld_hl_indirect_be
	; hl = *hl, big-endian
	; e.g. gethl_be hl
	ld a, [hli]
	ld LOW(\1), [hl]
	ld HIGH(\1), a
ENDM

MACRO ld16
	; load 16-bit register
	; e.g. ld16 hl, bc
	ld LOW(\1), LOW(\2)
	ld HIGH(\1), HIGH(\2)
ENDM

MACRO ex
	; exchange registers (z80)
	; e.g. ex hl, de
	push \1
	ld16 \1, \2
	pop \2
ENDM

MACRO sl16
	; left shift 16-bit register
	; e.g. sl16 hl
	sla LOW(\1)
	rl HIGH(\1)
ENDM

MACRO sr16
	; right shift 16-bit register
	; e.g. sr16 hl
	srl HIGH(\1)
	rr LOW(\1)
ENDM

MACRO ld16a
	; \1 = a
	; e.g. ld16a hl
	ld LOW(\1), a
	ld HIGH(\1), 0
ENDM

MACRO add16a
	; \1 += a (+ \2)
	; e.g. add16a hl
	; e.g. add16a hl, Label
	; https://github.com/pret/pokecrystal/wiki/Optimizing-assembly-code#set-a-16-bit-register-to-a-plus-a-constant
	if _NARG == 1
		add LOW(\1)
		ld LOW(\1), a
		adc HIGH(\1)
		sub LOW(\1)
		ld HIGH(\1), a
	else
		if !ISCONST(\2)
			add LOW(\2)
			ld LOW(\1), a
			adc HIGH(\2)
			sub LOW(\1)
			ld HIGH(\1), a
		elif (\2) == 0
			lda16 \1
		elif (\2) < $100
			sub LOW(-(\2))
			ld LOW(\1), a
			sbc a
			inc a
			ld HIGH(\1), a
		else
			add LOW(\2)
			ld LOW(\1), a
			adc HIGH(\2)
			sub LOW(\1)
			ld HIGH(\1), a
		endc
	endc
ENDM

; -------------------------------------
; Hardware
; -------------------------------------
MACRO vram_bank_0
  xor a
  ldh [$ff4f], a
ENDM

MACRO vram_bank_1
  ld a, 1
  ldh [$ff4f], a
ENDM

MACRO wait_vram
.waitVRAM\@
	ldh a, [rSTAT]
	and STATF_BUSY
	jr nz, .waitVRAM\@
	; Now there's 16 "safe" cycles to write during hblank.
	; If STAT is read on the very last cycle of hblank or vblank, the OAM scan will start immediately after,
	; and when this macro ends, you're left with 16 cycles remaining of OAM scan (which is also safe to write VRAM during)
ENDM

; -------------------------------------
; Load constants in parts
; -------------------------------------

MACRO lb
	; load bytes
	; e.g. lb bc, 12, 34
	ld \1, ((\2) & $ff) << 8 | ((\3) & $ff)
ENDM

MACRO ln
	; load nybbles
	; e.g. ln a, 12, 3
	ld \1, ((\2) & $f) << 4 | ((\3) & $f)
ENDM

MACRO lc
	; load crumbs
	; e.g. ld a, 0, 1, 2, 3
	ld \1, ((\2) & %11) << 6 | ((\3) & %11) << 4 | ((\4) & %11) << 2 | ((\5) & %11)
ENDM

; -------------------------------------
; Direct constants
; -------------------------------------

MACRO dn
	; declare nybbles
	; e.g. dn 1,10, 2,12
	rept _NARG / 2
		db ((\1) & $f) << 4 | ((\2) & $f)
		shift 2
	endr
ENDM

MACRO dc
	; declare crumbs
	; e.g. dc 0,1,2,3, 3,2,1,0
	rept _NARG / 4
		db ((\1) & %11) << 6 | ((\2) & %11) << 4 | ((\3) & %11) << 2 | ((\4) & %11)
		shift 4
	endr
ENDM

MACRO dw_be
	; declare big-endian words
	; e.g. dw_be Foo, Bar
	rept _NARG
		db HIGH(\1), LOW(\1)
		shift
	endr
ENDM

MACRO dba
	; bank+address
	; e.g. dba wFoo, wBar
	rept _NARG
		db BANK(\1)
		dw \1
		shift
	endr
ENDM

MACRO dab
	; address+bank
	; e.g. dab wFoo, wBar
	rept _NARG
		dw \1
		db BANK(\1)
		shift
	endr
ENDM

MACRO rgb
	; RGB555 colors
	; e.g. RGB 31,31,31, 0,0,0
	rept _NARG / 3
		dw ((\3) & %11111) << 10 | ((\2) & %11111) << 5 | ((\1) & %11111)
		shift 3
	endr
ENDM

; -------------------------------------
; Miscellaneous
; -------------------------------------

MACRO jmp
	; jp that warns if it can be jr
	; e.g. jmp z, .next
	jp \#
	assert warn, (\<_NARG>) - @ > 127 || (\<_NARG>) - @ < -129, "jmp can be jr"
ENDM

MACRO rst38cc
	; conditional rst $38
	; e.g. rst38cc z
	; e.g. rst38cc z, MyRst38
	if _NARG == 2
		assert BANK(\2) == 0 && (\2) == $0038, "\2 is not rst $38"
	endc
	jr \1, @ + 1
ENDM

macro fallthrough
  assert (\1) == @
endm
