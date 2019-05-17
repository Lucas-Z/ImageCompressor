##
## EPITECH PROJECT, 2019
## FUN_imageCompressor_2018
## File description:
## Makefile
##

SRCDIR		=	imageCompressor
FILEPATH	=	$(shell stack path --local-install-root)

TARGET		=		imageCompressor

all:		$(TARGET)

$(TARGET):
	stack build
	cp $(FILEPATH)/bin/imageCompressor-exe .
	mv imageCompressor-exe $(TARGET)

fclean:
		rm -f $(TARGET)

re:		fclean all

.PHONY:		all re fclean clean