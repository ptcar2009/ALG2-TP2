
.PHONY: haskell
haskell: 
	cd haskell && ghc -o ../main Main.hs  -outputdir obj

cpp:
	g++ -o main cpp/src/* 

.PHONY: generate
generate:
	bash generate.sh
