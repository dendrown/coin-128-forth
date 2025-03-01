TARGET := coin-128

CONFIG := c128-asm.cfg
ALL_ASM := $(wildcard *.asm) $(wildcard *.inc)

all: $(ALL_ASM)
	cl65 -C $(CONFIG) -o $(TARGET) -l $(TARGET).lst $<
	@ls -l $(TARGET)

clean:
	rm -f $(TARGET) *.lst *.o
