
; Code for running the virtual machine.
; The VM is a stack-based harvard architecture,
; with both a call stack and a data stack.
; Instructions are either 'literals' that push a respective value
; to the data stack, or 'operations' that manipulate the call and data stacks.
; There are no jumps - program flow is entirely done by operations that
; push (call), pop (return) or replace (tail call) routines on the call stack.
; Values are either numeric in the range -99 to 99, or references to routines.
; In general, arithmetic operations will saturate (cap to -99/99, not wrap),
; and invalid operations (eg. adding a routine label, calling a number) will not produce
; output / perform their normal action.


include "constants.asm"
include "macros.asm"


SECTION "VM state", WRAM0

; Note most of these vars are private. We copy them into graphics vars when
; we want the screen to update. This seperates actual state from display, so
; we can continue to run without messing up frames mid-render.

; Number of items in DataStack
DataStackSize:
	db

; Stack of data values. -99 to 99 are numeric, 112-127 (ie. $70-$7f) are routine symbols 1-16.
; Grows upward.
DataStack:
	ds DATA_STACK_MAX

; Number of items in CallStack.
; Grows upward.
CallStackSize:
	db

; Stack of (routine ID, position in routine)
CallStack:
	ds 2 * CALL_STACK_MAX

; For now, a 5-long array of 16-instruction routines.
; Later, may replace this with something more indirect so we aren't paying
; for space we aren't using.
; Index into this array is routine symbol number - 1
; This one is fine to be public since it's immutable once runtime begins
Routines::
	ds 5 * 16


; TEMP: Initial VM state for testing
SECTION "VM initial state", ROM0

; This is literally just raw memory copied in, so order must match VM state above
InitialState:
	db 5 ; data stack size
	db -45, -3, 8, 5, 5 ; data stack
	ds DATA_STACK_MAX + (-5) ; rest of data stack
	db 3 ; call stack size
	db 1,6, 3,4, 5,0 ; call stack pairs (id, position)
	ds 2 * (CALL_STACK_MAX + (-3))
	; Routines:
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
_InitialStateEnd:
INITIAL_STATE_SIZE EQU _InitialStateEnd - InitialState

; invented for above, besides obvious (literals, +, -, *)
; I: input (-> input)
; ~: negate (n -> -n)
; C: call (routine ->)
; O: output (output ->)
; U: dUplicate (x -> x x)
; R: roll n (kn ... k2 k1 x n -> x kn ... k2 k1)
; Z: return if n == 0 (n ->)
; T: tail call (routine ->)
; D: drop
; E: rEturn
; /: divmod (n d -> n/d n%d)


SECTION "VM Methods", ROM0

VMInit::
	; TODO replace test initial state with actual init
	ld BC, INITIAL_STATE_SIZE
	ld HL, InitialState
	ld DE, DataStackSize
	LongCopy
	ret


; Write values to graphics staging data based on current state.
; This should be done after the current frame is finished rendering
; (up to 8 rows before vblank) to avoid concurrent access / split frame.
UpdateDisplayData::

	; Calculate call stack scroll values
	; Scroll position is position * 8 pixels/tile - 76 to center current position in screen
	ld DE, CallStackScrolls
	ld HL, CallStack + 1 ; skip routine number, start at first position value
	ld A, [CallStackSize]
	ld B, A
.call_stack_scrolls_loop
	ld A, [HL+] ; A = position
	inc HL ; HL += 2, so it's at next position
	add A
	add A
	add A ; A = 8 * position
	sub 76 ; A = 8 * position - 76
	ld [DE], A ; write value
	inc DE ; move DE to next scroll value
	dec B
	jr nz, .call_stack_scrolls_loop

	; Update CallStackDrawState, setting each actual routine, and then setting
	; 0 on the next one as a terminator.
	ld HL, CallStack
	ld DE, CallStackDrawState
	ld A, [CallStackSize]
	ld B, A
.call_stack_state_loop
	ld A, [HL+] ; A = routine id
	inc HL ; HL += 2, so it's at next position
	ld [DE], A ; write actual
	inc DE
	inc DE ; DE += 2, so it's at next position
	dec B
	jr nz, .call_stack_state_loop
	; finally, write 0 to next actual routine slot as terminator
	xor A
	ld [DE], A

	; Write new DataStackDisplaySize,
	; write every value in data stack to DataStackDisplay

	ld A, [DataStackSize]
	ld B, A
	ld HL, DataStackDisplaySize
	ld [HL+], A ; set DataStackDisplaySize, HL = DataStackDisplay
	ld DE, DataStack
.data_stack_loop
	ld A, [DE]
	inc DE
	ld C, A
	push BC
	push DE
	call ValueToString
	ld A, B
	ld [HL+], A
	ld A, C
	ld [HL+], A
	ld A, D
	ld [HL+], A
	pop DE
	pop BC
	dec B
	jr nz, .data_stack_loop

	ret

; Takes a value C and converts it to a 3-char string, returned in regs B, C, D
; Clobbers A.
ValueToString:
	; First, check if it's a routine id (encoded as $7x)
	ld A, C
	and $f0
	cp $70 ; set z if C of form $7x
	jr z, .is_routine

	; Check if it's negative or not to set first char ("-" or " ")
	and $80 ; set z if positive. note that A still contains top half from above.
	ld B, " " ; default case is space
	jr z, .is_positive
	; if negative, negate it and set first char as "-"
	ld B, "-"
	xor A
	sub C
	ld C, A ; C = -C
.is_positive

	call ValueToBCD ; A = BCD version of C
	ld D, A ; temp storage
	swap A
	and $0f ; set z if top digit is 0
	jr z, .leading_zero
	add "0"
	ld C, A ; C = non-zero digit corresponding to top nibble of D
	jr .after_leading_zero
.leading_zero
	; if a leading zero, move the first char ("-" or " ") to the second char and blank the first
	ld C, B
	ld B, " "
.after_leading_zero
	ld A, D
	and $0f
	add "0"
	ld D, A ; D = digit corresponding to bottom nibble of D
	ret

.is_routine
	; Extract routine char from the bottom nibble, and add leading spaces
	ld A, C
	and $0f ; A = bottom nibble = routine character
	ld B, " " ; B = " "
	ld C, B ; C = " "
	ret


; Takes a value C in [0, 99] and converts it to a BCD-encoded value, returned in A.
; Clobbers C
ValueToBCD:
	; BCD conversion algo:
	;  For each bit of input (MSB first)
	;   Double current decimal value
	;   Add bit to the decimal value
	; Noting that all operations on the decimal value are BCD arithmetic.
	; Note we need to use ADC A to double and add, not RLA, as RLA doesn't set the h flag
	; which DAA needs to work properly.
	xor A
	sla C ; skip first bit, we know it's 0 since input must be positive
REPT 7
	sla C ; put MSB of C into carry, and shift remaining bits up
	adc A ; A = 2 * A + carry
	daa ; fix up previous operation to be correct for BCD
ENDR
	ret
