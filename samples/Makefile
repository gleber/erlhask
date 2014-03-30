SRCS = $(wildcard *.erl)

CORES = $(patsubst %.erl,%.core,$(SRCS))

all: $(CORES)

%.core: %.erl
	erlc +to_core $<
