
# avoid implicit rules for clarity
.SUFFIXES: .asm .o .gb
.PHONY: bgb clean debug

ASMS := $(wildcard *.asm)
OBJS := $(ASMS:.asm=.o)
DEBUGOBJS := $(addprefix build/debug/,$(OBJS))
RELEASEOBJS := $(addprefix build/release/,$(OBJS))
INCLUDES := $(wildcard include/*.asm)
ASSETS := $(shell find assets/ -type f)

all: build/release/rom.gb

include/assets/.uptodate: $(ASSETS) tools/assets_to_asm.py
	python tools/assets_to_asm.py assets/ include/assets/
	touch $@

build/debug/%.o: %.asm $(INCLUDES) include/assets/.uptodate build/debug
	rgbasm -DDEBUG=1 -i include/ -v -o $@ $<

build/release/%.o: %.asm $(INCLUDES) include/assets/.uptodate build/release
	rgbasm -DDEBUG=0 -i include/ -v -o $@ $<

build/debug/rom.gb: $(DEBUGOBJS)
# note padding with 0x40 = ld b, b = BGB breakpoint
	rgblink -n $(@:.gb=.sym) -o $@ -p 0x40 $^
	rgbfix -v -p 0x40 $@

build/release/rom.gb: $(RELEASEOBJS)
	rgblink -n $(@:.gb=.sym) -o $@ $^
	rgbfix -v -p 0 $@

build/debug build/release:
	mkdir -p $@

debug: build/debug/rom.gb
	bgb $<

bgb: build/release/rom.gb
	bgb $<

clean:
	rm -f build/*/*.o build/*/rom.sym build/*/rom.gb rom.gb include/assets/.uptodate include/assets/*.asm
