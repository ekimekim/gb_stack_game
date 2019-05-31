
; The main simulation screen is divided into three areas:
;   The main area in the top-left, taking up all space unused by other areas.
;     Content TBD. Served out of the main TileGrid using the background map.
;   The stack area on the right, showing the contents of the program's data stack.
;     Occupies the right-most 4 columns. Served out of rows 0-19 of the AltTileGrid,
;     using the window. Scrolling is expensive, done by re-rendering entire area
;     (3x20 max, est. 400 cycles), smooth scrolling by moving WindowY up to 7 pixels.
;   The program area at the bottom, showing the program instructions and call stack
;     as rows of functions, centered on the current instruction of each function
;     (with a cursor overlaid). Served out of rows 20-31 (lowest is main function)
;     of AltTileGrid, using the background map (switching from main tile grid at border during H-blank).
;     Area expands to accommodate up to 6 rows, after which it scrolls for remaining 6.
;     Each row is scrolled individually left-right by changing ScrollX during H-blank.


include "hram.asm"
include "longcalc.asm"
include "macros.asm"
include "ioregs.asm"
include "vram.asm"

LCDC_MAIN_AND_WINDOW EQU %11110001
LCDC_ALT_NO_WINDOW   EQU %11011001


; NOTE: should be RAM later
SECTION "Graphics data", ROM0, ALIGN[8]

; "staging" data, which contains data sourced from simulation state, but already modified
; as required so we can quickly copy into VRAM during V/H blanks.

; Array of scroll positions (in pixels) into each row, bottom of call stack first.
; Aligned to 8 bits for quick lookup from CallStackIndex.
CallStackScrolls:
	db 6 * 8 - 76
	db 4 * 8 - 76
	db 0 * 8 - 76

; The top 20 values of the stack, first-out first,
; stored as 3-length ascii digits (-99 to 99), or spaces for blank slots.
; It is copied over every frame.
DataStack:
	db "  5"
	db "  5"
	db "  8"
	db " -3"
	db "-45"
REPT 15
	db "   "
ENDR

; This data will probably eventually live elsewhere, putting it here for testing for now.

; Number of items in CallStack
CallStackSize:
	db 3

; Stack of routine IDs, max 12
CallStack:
	db "!"
	db "#"
	db "%"

; Array of 16-instruction routines (larger later)
; Index is routine symbol - 1
Routines:
	; !
	db "I5*~7#C+O       " ; output (-5 * input + #(7))
	; "
	; #
	db "~11%CD1RDE      " ; fib wrapper
	; "
	; %
	db "U3R+2R1+UZ2R%T  " ; fib

; invented for above, besides obvious (literals, +, -, *)
; I: input (-> input)
; ~: negate (n -> -n)
; C: call (routine ->)
; O: output (output ->)
; U: dUplicate (x -> x x)
; R: roll n (kn ... k2 k1 x n -> x kn ... kn k2 k1)
; Z: return if n == 0 (n ->)
; T: tail call (routine ->)
; D: drop
; E: rEturn


SECTION "Graphics textures", ROMX


GraphicsTextures:

; 0 to 14: font symbols, placeholders for routine symbols:  !"#$%&'<>*+,-.
include "assets/symbols.asm"
; 15 to 31: blank
	ds 16 * 17
; 32 up: printable ascii
include "assets/font.asm"

EndGraphicsTextures:

TEXTURES_SIZE EQU EndGraphicsTextures - GraphicsTextures


SECTION "Graphics methods", ROM0


; Load textures, etc.
GraphicsInit::

	; Textures into unsigned tilemap
	ld HL, GraphicsTextures
	ld BC, TEXTURES_SIZE
	ld DE, BaseTileMap
	LongCopy

	; Set up STAT interrupt
	ld A, %01000100 ; interrupt when LY == LYC
	ld [LCDStatus], A

	; Zero main and alt tile grids
	xor A
	ld BC, $800
	ld HL, TileGrid
.zeroloop
	ld [HL+], A
	dec BC
	or C
	jr nz, .zeroloop
	or B
	jr nz, .zeroloop

	; temp for now - routines into AltTileGrid
	ld B, 0
.routineloop
	ld A, B
	LongAddToA CallStack, HL
	ld A, [HL] ; A = CallStack[B]
	dec A
	LongAddToA Routines, DE ; DE = &Routines[A-1]
	ld A, 31
	sub B
	ld H, 0
	ld L, A
REPT 5
	add HL, HL ; HL = 32 * (31 - B) = offset of first tile of row
ENDR
	LongAdd HL, AltTileGrid, HL ; HL = address of first tile in row

	ld C, 16
.routine_inner
	ld A, [DE]
	ld [HL+], A
	inc DE
	dec C
	jr nz, .routine_inner

	inc B
	ld A, [CallStackSize]
	cp B
	jr nz, .routineloop

	ret


VBlankHandler::
	push AF
	push BC
	push DE
	push HL
	call VBlank
	pop HL
	pop DE
	pop BC
	pop AF
	reti


VBlank:

	; Unused for now, but clear ScrollX so that changes from prev frame are reset
	xor A
	ld [ScrollX], A

	; Switch back to main background + window
	ld A, LCDC_MAIN_AND_WINDOW
	ld [LCDControl], A

	; Reset CallStackScrollsIndex
	ld A, [CallStackSize]
	dec A
	ld [CallStackScrollsIndex], A

	; Draw data stack
	ld HL, DataStack
	ld DE, AltTileGrid ; starts in top-left
	ld B, 20
.draw_data_stack
	ld A, [HL+]
	ld [DE], A
	inc E ; we know DE is aligned since it is at start of row
	ld A, [HL+]
	ld [DE], A
	inc E
	ld A, [HL+]
	ld [DE], A
	; add 30, bringing DE to start of next row
	ld A, 30
	LongAddToA DE, DE
	dec B
	jr nz, .draw_data_stack

	; Set LYC to first routine display row.
	ld A, [CallStackSize]
	ld B, A
	ld A, 20
	sub B
	rla
	rla
	rla ; A = 8 * (20 - [CallStackSize])
	ld [LCDYCompare], A

	; Cache first CallStackScroll
	call GetNextCallStackScroll

	ret


; From interrupt (with jump to here) to finished critical work: 14 cycles. We have 20.
StatHandler::
	push AF

	; Switch background maps and disable window
	ld A, LCDC_ALT_NO_WINDOW
	ld [LCDControl], A

	; Set scroll for row
	ld A, [NextCallStackScroll]
	ld [ScrollX], A

	push HL
	call GetNextCallStackScroll
	pop HL

	pop AF
	reti


GetNextCallStackScroll:
	ld A, [CallStackScrollsIndex]
	ld H, HIGH(CallStackScrolls)
	ld L, A
	dec A
	ret c ; return early if carry, instead of updating the index, so it doesn't go below 0.
	ld [CallStackScrollsIndex], A ; write back updated index

	ld A, [HL]
	ld [NextCallStackScroll], A ; save looked up value

	ret