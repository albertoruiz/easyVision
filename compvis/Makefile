ghci:
	ghc -e '(writeFile "session" . unlines . map (":l "++) . words) "$(SRC)"'
	ghci < session
	rm session

SRC  = $(wildcard *.hs)
EXEC = $(SRC:.hs=)

clean:
	rm -rf $(wildcard *.o)
	rm -rf $(wildcard *.hi)
	rm -rf $(EXEC)

