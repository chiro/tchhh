all : Base.hs TL.hs main.hs
	ghc --make main

clean :
	rm *.o *.hi main
