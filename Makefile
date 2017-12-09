.PHONY = all

TARGETS=cypherbasic.prg

all: $(TARGETS)

cypherbasic.prg: cypherbasic.s Makefile
	cl65 -o $@ -t c64 -C cypherbasic.cfg $<
#	cl65 -o $@ -u __EXEHDR__ -t c64 -C c64-asm.cfg $<
#	ca65 -t c64 $< 
#	ld65 --config c64-asm.cfg -o $@ $(basename $<).o c64.lib

#%.o: %.asm
#	nasm -felf64 $< -o $@

disasm:
	r2 -a6502 -m0x8ffe -A cypherbasic.prg

clean:
	rm *.o $(TARGETS)
