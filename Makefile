# Makefile for Advent of Code.

#ADVENT_OPT = -O2 -Wall -threaded
ADVENT_OPT = -O2 -dynamic -Wall
#ADVENT_OPT = -O2 -dynamic -threaded -with-rtsopts="-N4"

all : solutions_2022

solutions_2022 : solutions_2022.hs Solution_01.o Solution_02.o Solution_03.o Solution_04.o Solution_05.o Solution_06.o Solution_07.o Solution_08.o Solution_09.o Solution_10.o Solution_11.o Solution_12.o Solution_13.o Solution_14.o Solution_15.o Solution_16.o Solution_17.o Solution_18.o Solution_19.o Solution_20.o Solution_21.o Solution_22.o Solution_23.o Solution_24.o Solution_25.o Parsers.o Utilities.o
	ghc $(ADVENT_OPT) solutions_2022.hs

Solution_01.o : Solution_01.hs
	ghc $(ADVENT_OPT) Solution_01.hs

Solution_02.o : Solution_02.hs Parsers.hs
	ghc $(ADVENT_OPT) Solution_02.hs

Solution_03.o : Solution_03.hs
	ghc $(ADVENT_OPT) Solution_03.hs

Solution_04.o : Solution_04.hs Parsers.hs
	ghc $(ADVENT_OPT) Solution_04.hs

Solution_05.o : Solution_05.hs Parsers.hs
	ghc $(ADVENT_OPT) Solution_05.hs

Solution_06.o : Solution_06.hs
	ghc $(ADVENT_OPT) Solution_06.hs

Solution_07.o : Solution_07.hs
	ghc $(ADVENT_OPT) Solution_07.hs

Solution_08.o : Solution_08.hs
	ghc $(ADVENT_OPT) Solution_08.hs

Solution_09.o : Solution_09.hs
	ghc $(ADVENT_OPT) Solution_09.hs

Solution_10.o : Solution_10.hs
	ghc $(ADVENT_OPT) Solution_10.hs

Solution_11.o : Solution_11.hs
	ghc $(ADVENT_OPT) Solution_11.hs

Solution_12.o : Solution_12.hs
	ghc $(ADVENT_OPT) Solution_12.hs

Solution_13.o : Solution_13.hs
	ghc $(ADVENT_OPT) Solution_13.hs

Solution_14.o : Solution_14.hs
	ghc $(ADVENT_OPT) Solution_14.hs

Solution_15.o : Solution_15.hs
	ghc $(ADVENT_OPT) Solution_15.hs

Solution_16.o : Solution_16.hs
	ghc $(ADVENT_OPT) Solution_16.hs

Solution_17.o : Solution_17.hs
	ghc $(ADVENT_OPT) Solution_17.hs

Solution_18.o : Solution_18.hs
	ghc $(ADVENT_OPT) Solution_18.hs

Solution_19.o : Solution_19.hs
	ghc $(ADVENT_OPT) Solution_19.hs

Solution_20.o : Solution_20.hs
	ghc $(ADVENT_OPT) Solution_20.hs

Solution_21.o : Solution_21.hs
	ghc $(ADVENT_OPT) Solution_21.hs

Solution_22.o : Solution_22.hs
	ghc $(ADVENT_OPT) Solution_22.hs

Solution_23.o : Solution_23.hs
	ghc $(ADVENT_OPT) Solution_23.hs

Solution_24.o : Solution_24.hs
	ghc $(ADVENT_OPT) Solution_24.hs

Solution_25.o : Solution_25.hs
	ghc $(ADVENT_OPT) Solution_25.hs

Parsers.o : Parsers.hs
	ghc $(ADVENT_OPT) Parsers.hs

Utilities.o : Utilities.hs
	ghc $(ADVENT_OPT) Utilities.hs

clean :
	rm *.o *~ *.hi; echo

hlint : 
	hlint $(wildcard Solution_*.hs) Parsers.hs Utilities.hs solutions_2022.hs

clean_solutions :
	rm solutions_2022
