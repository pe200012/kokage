# Wine helper Makefile for SHIORI bridge

# Compilers
WINEGCC = winegcc

# Targets
BRIDGE = shiori_bridge.exe
BRIDGE32 = shiori_bridge32.exe

.PHONY: all clean

all: $(BRIDGE) $(BRIDGE32)

# 64-bit Wine compilation (native Wine DLL loading)
$(BRIDGE): shiori_bridge.c
	$(WINEGCC) -o $@ $<

# 32-bit cross-compilation (for 32-bit SHIORI DLLs)
$(BRIDGE32): shiori_bridge.c
	$(WINEGCC) -m32 -o $@ $<

clean:
	rm -f *.exe *.exe.so *.o
