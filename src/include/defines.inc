DEF DIRECTION_RIGHT EQU 0
DEF DIRECTION_DOWN  EQU 1
DEF DIRECTION_LEFT  EQU 2
DEF DIRECTION_UP    EQU 3


DEF PALETTE_FACE    EQU 6
DEF PALETTE_PLAYER  EQU 7

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
