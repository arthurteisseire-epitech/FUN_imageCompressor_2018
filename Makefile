##
## EPITECH PROJECT, 2018
## FUN_deBruijn_2018
## File description:
## Makefile
##

all:
	stack build
	stack install --local-bin-path ./

clean:
	stack clean
	rm imageCompressor
