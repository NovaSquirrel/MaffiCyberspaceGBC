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
DEF actor_vxl    = 3   ; %0011
DEF actor_vyl    = 4   ; %0100
DEF actor_pyl    = 5   ; %0101
DEF actor_pyh    = 6   ; %0110
DEF actor_pxl    = 7   ; %0111
DEF actor_pxh    = 8   ; %1000
DEF actor_var1   = 9   ; %1001
DEF actor_var2   = 10  ; %1010
DEF actor_var3   = 11  ; %1011
DEF actor_var4   = 12  ; %1100
DEF actor_unused = 13  ; %1101
DEF actor_unused2= 14  ; %1110
DEF actor_unused3= 15  ; %1111

; Actor information (Extra)
; 0 Health
; 1 Damaged by ID
; 2 Knockback timer
; 3 Knockback sign X
; 4 Knockback sign Y
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

DEF actor_health           = 256 + 0
DEF actor_damaged_by_id    = 256 + 1
DEF actor_knockback_timer  = 256 + 2
DEF actor_knockback_sign_x = 256 + 3
DEF actor_knockback_sign_y = 256 + 4

; Actor states
DEF ACTOR_STATE_NORMAL = 0
DEF ACTOR_STATE_KNOCKBACK = 1

; Other actor stuff
DEF ACTOR_SIZE  = 16
DEF ACTOR_COUNT = 16
DEF PLAYER_PROJECTILE_COUNT = 7

DEF PLAYER_COLLISION_WIDTH = 10
DEF ENEMY_COLLISION_WIDTH = 10
DEF PROJECTILE_COLLISION_WIDTH = 10

DEF PLAYER_COLLISION_HEIGHT = 4
DEF ENEMY_COLLISION_HEIGHT = 4
DEF PROJECTILE_COLLISION_HEIGHT = 10

; Main projectile
DEF PlayerProjectiles_type EQUS "(PlayerProjectiles + actor_type)"
DEF PlayerProjectiles_vxl  EQUS "(PlayerProjectiles + actor_vxl)"
DEF PlayerProjectiles_vyl  EQUS "(PlayerProjectiles + actor_vyl)"
DEF PlayerProjectiles_pyl  EQUS "(PlayerProjectiles + actor_pyl)"
DEF PlayerProjectiles_pyh  EQUS "(PlayerProjectiles + actor_pyh)"
DEF PlayerProjectiles_pxl  EQUS "(PlayerProjectiles + actor_pxl)"
DEF PlayerProjectiles_pxh  EQUS "(PlayerProjectiles + actor_pxh)"
DEF PlayerProjectiles_var1 EQUS "(PlayerProjectiles + actor_var1)"
DEF PlayerProjectiles_var2 EQUS "(PlayerProjectiles + actor_var2)"
DEF PlayerProjectiles_var3 EQUS "(PlayerProjectiles + actor_var3)"
DEF PlayerProjectiles_var4 EQUS "(PlayerProjectiles + actor_var4)"

; .----------------------------------------------------------------------------
; | Game-specific macros
; '----------------------------------------------------------------------------

MACRO get_block_flags
	ld h, HIGH(BlockFlags)
	ld l, a
	ld a, [hl]
ENDM

; This macro is meant for when HL is already pointing at one item within an actor,
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
