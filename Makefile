all: ccgserver

ccgserver: Main.hs
	ghc --make Main.hs -o ccgserver

clean:
	rm -rf *.hi *.o ccgserver
