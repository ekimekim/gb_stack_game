
; The main simulation screen is divided into three areas:
;   The main area in the top-left, taking up all space unused by other areas.
;     Content TBD. Served out of the main TileGrid using the background map.
;   The stack area on the right, showing the contents of the program's data stack.
;     Occupies the right-most 4 columns. Served out of rows 0-18 of the AltTileGrid,
;     using the window. Scrolling is expensive, done by re-rendering entire area
;     (3x18 max, est. 400 cycles), smooth scrolling by moving WindowY up to 7 pixels.
;     We use 19 rows even though we only show 18 because when scrolling we show half of
;     row 0 and half of row 18.
;   The program area at the bottom, showing the program instructions and call stack
;     as rows of functions, centered on the current instruction of each function
;     (with a cursor overlaid). Served out of rows 19-31 (lowest is main function)
;     of AltTileGrid, using the background map (switching from main tile grid at border during H-blank).
;     Area expands to accommodate up to 6 rows, after which it scrolls for remaining 7.
;     Each row is scrolled individually left-right by changing ScrollX during H-blank.


include "constants.asm"
include "hram.asm"
include "longcalc.asm"
include "macros.asm"
include "ioregs.asm"
include "vram.asm"

LCDC_MAIN_AND_WINDOW EQU %11110011
LCDC_ALT_NO_WINDOW   EQU %11011011


SECTION "Graphics data", WRAM0, ALIGN[8]

; "staging" data, which contains data sourced from simulation state, but already modified
; as required so we can quickly copy into VRAM during V/H blanks.

; Array of scroll positions (in pixels) into each row, bottom of call stack first.
; Aligned to 8 bits for quick lookup from CallStackIndex.
CallStackScrolls::
	ds CALL_STACK_MAX

; A formatted copy of the data stack.
; Stored as 3-length ascii digits (-99 to 99), or a routine symbol.
; The relevant section as per DataStackSize is copied over every frame.
DataStackDisplay::
	ds 3 * DATA_STACK_MAX


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
	Sprite 136, 84, 15, 0 ; top call stack (%) cursor

	; temp for now - routines into AltTileGrid
	ld B, 0
.routineloop
	call DrawRoutine
	inc B
	ld A, [CallStackSize]
	cp B
	jr nz, .routineloop

	ret


; Draw the routine in callstack slot B to the AltTileGrid in the correct position.
; Clobbers all but B.
DrawRoutine:
	ld A, B
	add B ; A = 2 * B
	LongAddToA CallStack, HL ; HL = &CallStack[B]
	ld A, [HL] ; A = first byte of CallStack[B] = routine id
	; TODO change this when routine ids are no longer indexes into 16-byte-item array
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
.routine_copy
	ld A, [DE]
	ld [HL+], A
	inc DE
	dec C
	jr nz, .routine_copy

	ret


; Draw top 18 values of DataStackDisplay to top-left of AltTileGrid,'
; filling in remainder with "   " if < 18.
; Clobbers all.
DrawDataStack::
	ld A, [DataStackSize]
	ld B, A ; B = [DataStackSize]
	add A ; A = 2 * [DataStackSize]
	add B ; A = 3 * [DataStackSize]
	dec A ; A = 3 * [DataStackSize] - 1
	LongAddToA DataStackDisplay, HL ; HL = &(DataStackDisplay[DataStackSize] - 1) = last char of top value
	ld C, 18
	ld DE, AltTileGrid + 2 ; last char of first row of 3 chars
	inc B ; B = [DataStackSize] + 1

	; TODO when scrolling, insert blank entry for first row

.copy_loop
	; B = number of values remaining + 1
	; C = number of display slots remaining
	; DE = dest pointer, counts down for each char then up to next row
	; HL = src pointer, counts down for each char

	dec B ; set z if B == 0, indicating we're out of values. do this first in case [DataStackSize] == 0
	jr z, .clear

	; Copy 3 chars
	ld A, [HL-]
	ld [DE], A
	dec E ; we know DE is aligned for each row
	ld A, [HL-]
	ld [DE], A
	dec E
	ld A, [HL-]
	ld [DE], A
	; add 34, bringing DE to start of next row (32) + 2 chars
	ld A, 34
	LongAddToA DE, DE

	dec C ; set z if C == 0, indicating we've written all 18 values
	jr nz, .copy_loop

	ret

; If we reach here, we're out of stack values to write but C > 0 so we have to fill rest
; of DE with blanks.
.clear
	ld H, D ; No longer need a src pointer, so using HL is faster
	ld L, E
	ld DE, 34 ; This then frees up DE to hold a constant for later use
	xor A
.clear_loop
	ld [HL-], A
	ld [HL-], A
	ld [HL], A
	add HL, DE ; HL += 34, bringing it to start of next row (32) + 2 chars

	dec C ; set z if C == 0, indicating we've written all 18 values
	jr nz, .clear_loop

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

	; Draw data stack to top-left of AltTileGrid
	call DrawDataStack

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
