.PHONY = all

TARGETS=cypherbasic.prg

all: $(TARGETS)

cypherbasic.prg: cypherbasic.s
	ca65 -t c64 $< 
	ld65 --config c64-asm.cfg -o $@ $(basename $<).o c64.lib

#%.o: %.asm
#	nasm -felf64 $< -o $@


clean:
	rm *.o $(TARGETS)
