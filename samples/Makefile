SRCS = $(wildcard *.erl)

CORES = $(patsubst %.erl,%.core,$(SRCS))
ASMS = $(patsubst %.erl,%.S,$(SRCS))

all: $(CORES) $(ASMS)

%.core: %.erl
	erlc +to_core $<

%.S: %.erl
	erlc +to_asm $<
