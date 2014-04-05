.SUFFIXES: # delete freaking default suffxies which screw up asm compilation

SRCS = $(wildcard *.erl)

CORES = $(SRCS:.erl=.core)
ASMS = $(SRCS:.erl=.S)

all: $(CORES) $(ASMS)

%.core: %.erl
	@echo 'Building target core: $@. First dep: $<'
	erlc +to_core $<

%.S: %.erl
	@echo 'Building target asm: $@. First dep: $<'
	erlc +to_asm $<
