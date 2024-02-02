##
# rubik's cube solver using old pochman
# (c) Felix Drees - GNU GENERAL PUBLIC LICENSE v2
#
# @file
# @version 0.1

CC=g++
INCDIRS=-I.
OPT=3
CFLAGS=-Wall -Wextra -Wpedantic -g $(INCDIRS) -O$(OPT)

OBJECTS=rubiks-cube.o main.o
TEST-OBJECTS=rubiks-cube-test.o rubiks-cube.o
MAIN=main

all: $(MAIN) test checkstyle

$(MAIN): $(OBJECTS)
	$(CC) $^ -o $@

%.o: %.cc
	$(CC) -c $^ -o $@ $(CFLAGS)

test: $(TEST-OBJECTS)
	$(CC) $^ -l gtest -l pthread -o run-test.o $(LDFLAGS)
	./run-test.o

format:
	clang-format -i -style=llvm $(MAIN)

checkstyle:
	cpplint --linelength=80 --repository=. *.cc *.hh

run:
	./$(MAIN)

clean:
	rm -f *.o $(MAIN)

# end
