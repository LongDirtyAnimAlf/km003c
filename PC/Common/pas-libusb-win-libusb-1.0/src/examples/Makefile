
#UNAME_S := $(shell uname -s)
#$(info OS: $(OS) ARCH: $(ARCH))
#$(info UNAME_S = $(UNAME_S))

#find your shell and adjust your OS and ARCH accordingly
#ifeq ($(findstring MINGW64,$(UNAME_S)), MINGW64)
#  OS = Windows_NT
  #ARCH = x86_64
#endif

$(info OS: $(OS) ARCH: $(ARCH))

#Change the parth to your fpc here if necessary:
FPC = fpc

EXECUTABLES = test1library test2controlsync test3controlasync testfirmware
FPC_OPTS = -Fu..
ifeq ($(ARCH), x86_64)
  FPC_OPTS += -Twin64
endif

all: $(EXECUTABLES)

%: %.pas
	$(FPC) $(FPC_OPTS) $<

clean:
	rm -f *.o *.ppu *~ *.bak $(EXECUTABLES)
