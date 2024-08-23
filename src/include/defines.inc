DEF DIRECTION_RIGHT EQU 0
DEF DIRECTION_DOWN  EQU 1
DEF DIRECTION_LEFT  EQU 2
DEF DIRECTION_UP    EQU 3

DEF PALETTE_FACE    EQU 6
DEF PALETTE_PLAYER  EQU 7

DEF DIRECTION8_EAST      EQU 0
DEF DIRECTION8_SOUTHEAST EQU 1
DEF DIRECTION8_SOUTH     EQU 2
DEF DIRECTION8_SOUTHWEST EQU 3
DEF DIRECTION8_WEST      EQU 4
DEF DIRECTION8_NORTHWEST EQU 5
DEF DIRECTION8_NORTH     EQU 6
DEF DIRECTION8_NORTHEAST EQU 7

; .----------------------------------------------------------------------------
; | Actor information
; '----------------------------------------------------------------------------

; Actor information (Main)
; 0 Type and direction (most significant bit)
; 1 State
; 2 Timer to reset state
; 3 Y velocity low
; 4 Y velocity high
; 5 Y low
; 6 Y high
; 7 X low
; 8 X high
; 9 X velocity low
; A X velocity high
; B Variable 1
; C Variable 2
; D Variable 3
; E Variable 4
; F ?
;

DEF actor_type   = 0   ; %0000
DEF actor_state  = 1   ; %0001
DEF actor_timer  = 2   ; %0010
DEF actor_vyl    = 3   ; %0011
DEF actor_vyh    = 4   ; %0100
DEF actor_pyl    = 5   ; %0101
DEF actor_pyh    = 6   ; %0110
DEF actor_pxl    = 7   ; %0111
DEF actor_pxh    = 8   ; %1000
DEF actor_vxl    = 9   ; %1001
DEF actor_vxh    = 10  ; %1010
DEF actor_var1   = 11  ; %1011
DEF actor_var2   = 12  ; %1100
DEF actor_var3   = 13  ; %1101
DEF actor_var4   = 14  ; %1110
DEF actor_unused = 15  ; %1111

; Actor information (Extra)
; 0 Health
; 1 Damaged by ID
; 2
; 3
; 4
; 5
; 6
; 7
; 8
; 9
; A
; B
; C
; D
; E
; F
;

DEF actor_health        = 256 + 0
DEF actor_damaged_by_id = 256 + 1

; Actor states
DEF ACTOR_STATE_NORMAL = 0
DEF ACTOR_STATE_KNOCKBACK = 1

; Other actor stuff
DEF ACTOR_SIZE  = 16
DEF ACTOR_COUNT = 16
DEF PLAYER_PROJECTILE_COUNT = 7

; .----------------------------------------------------------------------------
; | Game-specific macros
; '----------------------------------------------------------------------------

MACRO get_block_flags
	ld h, HIGH(BlockFlags)
	ld l, a
	ld a, [hl]
ENDM

; This macro is meant for whe HL is already pointing at one item within an actor,
; and it will switch from one field to another. One cycle faster than the "add hl, de" way.
MACRO switch_hl_to_field
	if \2 - \1 == 1
		inc l
	elif \2 - \1 == 2 
		inc l
		inc l
	elif \2 - \1 == 3
		inc l
		inc l
		inc l
	elif \2 - \1 == -1
		dec l
	elif \2 - \1 == -2
		dec l
		dec l
	elif \2 - \1 == -3
		dec l
		dec l
		dec l
	else
		ld a, l
		add \2 - \1
		ld l, a
	endc
ENDM
