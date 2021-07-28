
DEPEND += Tokens.hs Grammar.hs Interpreter.hs

all: $(DEPEND) csvql


csvql : $(DEPEND) Interpreter.hs
	ghc -o csvql Interpreter.hs


Grammar.hs : Grammar.y
	@rm -f Grammar.hs
	happy Grammar.y
	@chmod -w Grammar.hs


YAIPLTokens.hs : Tokens.x
	@rm -f Tokens.hs
	alex Tokens.x
	@chmod -w Tokens.hs

# Clean up the directory
clean::
	rm -rf Tokens.hs Grammar.hs *.hi *.o *.info *.exe