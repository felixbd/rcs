##
# rubiks cube solver using old pochman
#
# @file
# @version 0.1

CC=g++
INCDIRS=-I.
OPT=3
CFLAGS=-Wall -Wextra -g $(INCDIRS) -O$(OPT)

OBJECTS=rubiks-cube.o main.o
TEST-OBJECTS=rubiks-cube-test.o rubiks-cube.o
MAIN=main

all: $(MAIN) test checkstyle

$(MAIN): $(OBJECTS)
	$(CC) $^ -o $@

%.o: %.cc
	$(CC) -c $^ -o $@ $(CFLAGS)

test: $(TEST-OBJECTS)
	$(CC) $^ -l gtest -l pthread -o test.o
	./test.o

checkstyle:
	python3 cpplint.py  --repository=. *.cc *.hh

run:
	./$(MAIN)

clean:
	rm -f *.o $(MAIN)

# end
