DEST = $(shell stack path --local-install-root)
NAME = format-converter

all:
	stack build
	cp $(DEST)/bin/$(NAME)-exe ./$(NAME)

tests_run:
	stack test

clean:
	stack clean

fclean: clean
		rm -f $(NAME)

re: fclean all

.PHONY: all tests_run clean fclean re
