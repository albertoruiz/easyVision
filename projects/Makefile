SRC  = $(wildcard *.hs)
EXEC = $(SRC:.hs=)

all: $(EXEC)

ghci:
	ghc -e '(writeFile "session" . unlines . map (":l "++) . words) "$(SRC)"'
	ghci < session
	rm session

%: %.hs
	ghc --make -O2 -threaded $*
	rm $*.o $*.hi

clean:
	rm -rf $(wildcard *.o)
	rm -rf $(wildcard *.hi)
	rm -rf $(EXEC)

