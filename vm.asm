
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

; Number of items in DataStack
DataStackSize::
	db

; Stack of data values. -99 to 99 are numeric, 112-127 (ie. $70-$7f) are routine symbols 1-16.
; Grows upward.
DataStack::
	ds DATA_STACK_MAX

; Number of items in CallStack.
; Grows upward.
CallStackSize::
	db

; Stack of (routine ID, position in routine)
CallStack::
	ds 2 * CALL_STACK_MAX

; For now, a 5-long array of 16-instruction routines.
; Later, may replace this with something more indirect so we aren't paying
; for space we aren't using.
; Index into this array is routine symbol number - 1
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
; R: roll n (kn ... k2 k1 x n -> x kn ... kn k2 k1)
; Z: return if n == 0 (n ->)
; T: tail call (routine ->)
; D: drop
; E: rEturn


SECTION "Test code", ROM0

InitState::
	ld BC, INITIAL_STATE_SIZE
	ld HL, InitialState
	ld DE, DataStackSize
	LongCopy
	ret


; Write values to graphics staging data based on current state
UpdateDisplayData::
	; Hard-code for now
	; call stack scrolls
	ld HL, CallStackScrolls
	ld A, 6 * 8 - 76
	ld [HL+], A
	ld A, 4 * 8 - 76
	ld [HL+], A
	ld A, 0 * 8 - 76
	ld [HL+], A
	; data stack display
	ld HL, TempDataStackDisplay
	ld DE, DataStackDisplay
	ld B, 15
	Copy
	ret

TempDataStackDisplay:
	db "-45"
	db " -3"
	db "  8"
	db "  5"
	db "  5"
