IF !DEF(_G_CONSTANTS)
_G_CONSTANTS EQU "true"

; Max depth of the call stack. Currently 13 due to space limitations
; in the AltTileGrid when scrolling, since we want to share that grid with
; data stack graphics, which take up the first 19 rows. That leaves us with 13 rows.
CALL_STACK_MAX EQU 13

; Max depth of the data stack. Only real constraint on this is memory usage.
; Keep in mind the user can only see the top 18.
DATA_STACK_MAX EQU 64

ENDC
