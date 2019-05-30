
Section "Stack", WRAM0

StackBase:
	ds 128
Stack::


Section "Main", ROM0

Start::

	; Disable LCD and audio.
	; Disabling LCD must be done in VBlank.
	; On hardware start, we have about half a normal vblank, but this may depend on the hardware variant.
	; So this has to be done quick!
	xor A
	ld [SoundControl], A
	ld [LCDControl], A

	; Use core stack
	ld SP, CoreStack

