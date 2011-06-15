
NAME = gtdnextday

all: $(NAME)


$(NAME): $(NAME).hs
	ghc --make $^ -o $@

clean:
	rm -f $(NAME)
	rm -f *.hi *.o
