CXX=g++
CXXFLAGS=-Wall -Wextra -O3 -MMD --std=c++17
LDFLAGS=-Wl,--no-gc-sections
LDLIBS=

EXE=nci
OBJS= \
    circularbuffer.o \
    tokenizer.o \
    syntaxtree.o \
    parser.o \
    nci.o

LINK.o=$(LINK.cc)

all: $(EXE)

$(EXE): $(OBJS)

clean:
	-rm -f $(EXE) *.o *.d 

rebuild: clean all

run: all
	./$(EXE) | dot -Tpdf > output.pdf

.PHONY: all clean rebuild run

-include *.d

