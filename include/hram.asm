IF !DEF(_G_HRAM)
_G_HRAM EQU "true"

RSSET $ff80

; Index of CallStackScrolls to set next row to. The index is then decremented, so we draw
; from top of stack down. Reset to top of stack during vblank.
CallStackScrollsIndex rb 1

; The next ScrollX value to set for the next call stack row.
; Pre-computed as the timing between STAT interrupt and row start is tight (20 clks).
NextCallStackScroll rb 1

ENDC
