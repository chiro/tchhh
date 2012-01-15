all : Base.hs TL.hs Post.hs Main.hs
	ghc --make Main

clean :
	rm *.o *.hi Main
