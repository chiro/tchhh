all : Secret.hs Common.hs Config.hs TL.hs Main.sh
	ghc --make Main

clean :
	rm *.o *.hi Main
