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
def LC_ADD_WALLS             rw 1 ; 

; Check to make sure the level is traversable enough, turn LEVEL_AREA_1 through LEVEL_AREA_8 into tiles, apply autotiling
def LC_ADD_FLOORS            rw 1 ; 

; Put a tile of a given type *somewhere* in the maze; should happen after LC_ADD_WALLS
def LC_PUT_ANYWHERE_VISITED  rw 1 ; type, count

; Put a tile of a given type *somewhere* within a rectangle, on a visited tile; should happen after LC_ADD_WALLS
def LC_PUT_WITHIN_RECT       rw 1 ; xy, wh, type, count

; ---------------------------------------------------------
def LEVEL_AREA_1 EQUS "BlockType_PlaceholderArea1"
def LEVEL_AREA_2 EQUS "BlockType_PlaceholderArea2"
def LEVEL_AREA_3 EQUS "BlockType_PlaceholderArea3"
def LEVEL_AREA_4 EQUS "BlockType_PlaceholderArea4"
def LEVEL_AREA_5 EQUS "BlockType_PlaceholderArea5"
def LEVEL_AREA_6 EQUS "BlockType_PlaceholderArea6"
def LEVEL_AREA_7 EQUS "BlockType_PlaceholderArea7"
def LEVEL_AREA_8 EQUS "BlockType_PlaceholderArea8"
