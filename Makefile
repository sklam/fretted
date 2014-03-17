compile:
	ghc main.hs

lily:
	./main > demo.ly
	./lilyize.sh demo.ly

clean:
	rm demo.ly demo.midi
