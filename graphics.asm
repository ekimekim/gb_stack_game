
; The main simulation screen is divided into three areas:
;   The main area in the top-left, taking up all space unused by other areas.
;     Content TBD. Served out of the main TileGrid using the background map.
;   The stack area on the right, showing the contents of the program's data stack.
;     Occupies the right-most 4 columns. Served out of rows 0-19 of the AltTileGrid,
;     using the window. Scrolling is expensive, done by re-rendering entire area
;     (3x18 max, est. 400 cycles), smooth scrolling by moving WindowY up to 7 pixels.
;   The program area at the bottom, showing the program instructions and call stack
;     as rows of functions, centered on the current instruction of each function
;     (with a cursor overlaid). Served out of rows 18-31 (lowest is main function)
;     of AltTileGrid, using the background map (switching from main tile grid at border during H-blank).
;     Area expands to accommodate up to 6 rows, after which it scrolls for remaining 6.
;     Each row is scrolled individually left-right by changing ScrollX during H-blank.


include "hram.asm"
include "longcalc.asm"
include "macros.asm"
include "ioregs.asm"
include "vram.asm"

LCDC_MAIN_AND_WINDOW EQU %11110011
LCDC_ALT_NO_WINDOW   EQU %11011011


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

; The top 18 values of the stack, first-out first,
; stored as 3-length ascii digits (-99 to 99), or spaces for blank slots.
; It is copied over every frame.
DataStack:
	db "  5"
	db "  5"
	db "  8"
	db " -3"
	db "-45"
REPT 13
	db "   "
ENDR

; This data will probably eventually live elsewhere, putting it here for testing for now.

; Number of items in CallStack
CallStackSize:
	db 3

; Stack of routine IDs, max 12
CallStack:
	db 1
	db 3
	db 5

; Array of 16-instruction routines (larger later)
; Index is routine symbol - 1
Routines:
	; !
	db "I5*~7#C+O       " ; output (-5 * input + #(7))
	; "
	ds 16
	; #
	db "~11%CD1RDE      " ; fib wrapper
	; $
	ds 16
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
; 15: cursor
include "assets/cursor.asm"
; 16 to 31: blank
	ds 16 * 16
; 32 up: printable ascii
include "assets/font.asm"

EndGraphicsTextures:

TEXTURES_SIZE EQU EndGraphicsTextures - GraphicsTextures


SECTION "Graphics methods", ROM0


; Load textures, etc.
GraphicsInit::

	; Identity palette
	ld A, %11100100
	ld [TileGridPalette], A
	ld [SpritePalette0], A
	; Inverted palette
	ld A, %00011011
	ld [SpritePalette1], A

	; Textures into unsigned tilemap
	ld HL, GraphicsTextures
	ld BC, TEXTURES_SIZE
	ld DE, BaseTileMap
	LongCopy

	; Set up STAT interrupt
	ld A, %01000100 ; interrupt when LY == LYC
	ld [LCDStatus], A

	; Put window on the right side, 3 columns showing.
	ld A, 17 * 8 + 7
	ld [WindowX], A
	xor A
	ld [WindowY], A

	; Zero main and alt tile grids
	xor A
	ld BC, $800
	ld HL, TileGrid
.zeroloop
	ld [HL+], A
	dec BC
	cp C
	jr nz, .zeroloop
	cp B
	jr nz, .zeroloop

	; Zero sprites
	ld B, 160
	xor A
	ld HL, SpriteTable
.spriteloop
	ld [HL+], A
	dec B
	jr nz, .spriteloop

	; temp for now - hard-code cursor sprites
	ld HL, SpriteTable
Sprite: MACRO
	ld A, \1
	ld [HL+], A
	ld A, \2
	ld [HL+], A
	ld A, \3
	ld [HL+], A
	ld A, \4
	ld [HL+], A
ENDM
	Sprite 136, 84, 15, %1001000 ; top call stack (%) cursor, inverted
	Sprite 144, 84, 15, %0000000 ; second call stack (#) cursor
	Sprite 152, 84, 15, %0000000 ; third call stack (!) cursor

	; temp for now - routines into AltTileGrid
	ld B, 0
.routineloop
	ld A, B
	LongAddToA CallStack, HL
	ld A, [HL] ; A = CallStack[B]
	dec A
	swap A
	LongAddToA Routines, DE ; DE = &Routines[16 * (A-1)]
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

	; Unused for now, but clear scrolls so that changes from prev frame are reset
	xor A
	ld [ScrollY], A
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
	ld B, 18
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
	ld A, 18
	sub B
	rla
	rla
	rla
	dec A ; A = 8 * (18 - [CallStackSize]) - 1
	ld [LCDYCompare], A

	ret


; We get interrupted a line before we need to do our writes.
; We need to wait at least 63 cycles (plus more if sprites in play), at most 134.
; We arrive 4 cycles in due to the jump to here.
; We wait such that we do our critical work in cycles 114-134.
StatHandler::
	push AF
	push HL

	; up to 12 cycles.

	; calculate scroll x, 13 cycles
	ld A, [CallStackScrollsIndex]
	ld H, HIGH(CallStackScrolls)
	ld L, A
	ld A, [HL]
	ld H, A ; store calculated value in H.
	dec L ; decrement index. set c if this was last row.
	ld A, L
	ld [CallStackScrollsIndex], A ; write back updated index

	; increment LYC by 8 for next row, unless this is last. 13 cycles.
	jr c, .no_update_lyc
	ld A, [LCDYCompare]
	add 8
	ld [LCDYCompare], A
	jr .after_update_lyc
.no_update_lyc
REPT 10
	nop
ENDR
.after_update_lyc

	; up to 38 cycles.

	; wait 76 cycles. at loop exit we're at cycle 114.
	ld A, 19
.waitloop
	dec A
	jr nz, .waitloop

	; Switch background maps and disable window, 5 cycles
	ld A, LCDC_ALT_NO_WINDOW
	ld [LCDControl], A

	; Set Y scroll for call stack - todo un-hardcode this later. 5 cycles.
	ld A, 14 * 8
	ld [ScrollY], A

	; Set X scroll for row, 4 cycles.
	ld A, H
	ld [ScrollX], A

	; critical section done.

	pop HL
	pop AF
	reti


GetNextCallStackScroll:
	ret
