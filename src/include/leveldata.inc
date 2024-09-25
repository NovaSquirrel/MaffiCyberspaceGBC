rsreset

; End the level
def LC_END                   rw 1 ;

; Switch the type to be used by commands that don't include one
def LC_TYPE                  rw 1 ; type

; Rectangle fill
def LC_RECT                  rw 1 ; x, y, w, h

; Rectangle fill but more compact
def LC_COMPACT_RECT          rw 1 ; xy, wh

; Place a single tile onto the level
def LC_SINGLE                rw 1 ; x, y

; Add random walls to the areas where LEVEL_AREA_1 through LEVEL_AREA_8 are, flood fill to make sure level is traversable, and try to fix it if it's not
def LC_ADD_WALLS             rw 1 ; count, type...

; Check to make sure the level is traversable enough, turn LEVEL_AREA_1 through LEVEL_AREA_8 into tiles, apply autotiling
def LC_ADD_FLOORS            rw 1 ; count, type...

; Put a tile of a given type *somewhere* in the maze; should happen after LC_ADD_WALLS
def LC_PUT_ANYWHERE_VISITED  rw 1 ; count, type

; Put a tile of a given type *somewhere* within a rectangle, on a visited tile; should happen after LC_ADD_WALLS
def LC_PUT_WITHIN_RECT       rw 1 ; xy, wh, count, type


; ---------------------------------------------------------

MACRO LevelRect ; X, Y, W, H
	IF ((\1 % 4) == 0) && ((\2 % 4) == 0) && ((\3 % 4) == 0) && ((\4 % 4) == 0)
		db LC_COMPACT_RECT, ((\1/4)<<4)|(\2/4), ((\3/4)<<4)|(\4/4)
	ELSE
		db LC_RECT, \1, \2, \3, \4
	ENDC
ENDM

MACRO LevelSingle ; X, Y
	db LC_SINGLE, \1, \2
ENDM

MACRO LevelPutWithinRect ; X, Y, W, H, Count, Type
	IF ((\1 % 4) == 0) && ((\2 % 4) == 0) && ((\3 % 4) == 0) && ((\4 % 4) == 0)
		db LC_PUT_WITHIN_RECT, ((\1/4)<<4)|(\2/4), ((\3/4-1)<<4)|(\4/4-1)
		db \5, \6|128
	ELSE
		fail "LevelPutWithinRect requires parameters that are a multiple of 4"
	ENDC
ENDM

MACRO LevelPutAnywhere ; Count, Type
	db LC_PUT_ANYWHERE_VISITED, \1, \2|128
ENDM

MACRO LevelType ; Count, Type
	db LC_TYPE, \1
ENDM

MACRO LevelAddWalls ; List of wall types
	IF _NARG == 1
		db LC_ADD_WALLS, \1|128
	ELIF _NARG == 2
		db LC_ADD_WALLS, \1, \2|128
	ELIF _NARG == 3
		db LC_ADD_WALLS, \1, \2, \3|128
	ELIF _NARG == 4
		db LC_ADD_WALLS, \1, \2, \3, \4|128
	ELIF _NARG == 5
		db LC_ADD_WALLS, \1, \2, \3, \4, \5|128
	ELIF _NARG == 6
		db LC_ADD_WALLS, \1, \2, \3, \4, \5, \6|128
	ELIF _NARG == 7
		db LC_ADD_WALLS, \1, \2, \3, \4, \5, \6, \7|128
	ELIF _NARG == 8
		db LC_ADD_WALLS, \1, \2, \3, \4, \5, \6, \7, \8|128
	ELSE
		fail "LevelAddWalls should have 1 to 8 parameters"
	ENDC
ENDM

MACRO LevelAddFloors ; List of floor types
	IF _NARG == 1
		db LC_ADD_FLOORS, \1|128
	ELIF _NARG == 2
		db LC_ADD_FLOORS, \1, \2|128
	ELIF _NARG == 3
		db LC_ADD_FLOORS, \1, \2, \3|128
	ELIF _NARG == 4
		db LC_ADD_FLOORS, \1, \2, \3, \4|128
	ELIF _NARG == 5
		db LC_ADD_FLOORS, \1, \2, \3, \4, \5|128
	ELIF _NARG == 6
		db LC_ADD_FLOORS, \1, \2, \3, \4, \5, \6|128
	ELIF _NARG == 7
		db LC_ADD_FLOORS, \1, \2, \3, \4, \5, \6, \7|128
	ELIF _NARG == 8
		db LC_ADD_FLOORS, \1, \2, \3, \4, \5, \6, \7, \8|128
	ELSE
		fail "LevelAddFloors should have 1 to 8 parameters"
	ENDC
ENDM

; ---------------------------------------------------------
def LEVEL_AREA_1 EQUS "BlockType_PlaceholderArea1"
def LEVEL_AREA_2 EQUS "BlockType_PlaceholderArea2"
def LEVEL_AREA_3 EQUS "BlockType_PlaceholderArea3"
def LEVEL_AREA_4 EQUS "BlockType_PlaceholderArea4"
def LEVEL_AREA_5 EQUS "BlockType_PlaceholderArea5"
def LEVEL_AREA_6 EQUS "BlockType_PlaceholderArea6"
def LEVEL_AREA_7 EQUS "BlockType_PlaceholderArea7"
def LEVEL_AREA_8 EQUS "BlockType_PlaceholderArea8"

; ---------------------------------------------------------
rsreset
def WALL_NONE rb
def WALL_25P rb
def WALL_50P rb
def WALL_75P rb
def WALL_93P rb
def WALL_GRID rb

; ---------------------------------------------------------
rsreset
def FLOOR_PLAIN rb
def FLOOR_RARE_STARS rb
def FLOOR_SOME_STARS rb
