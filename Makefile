# Modified By: Christopher J. Kelly
# Due Date:    4/8/2020
# Assignment:  #6
# Course:      Compilers II
# Professor:   Schwesinger
# Filename:    Makefile
# Purpose:     all - creates main.native
#							 compile - runs 'compile.sh' for everything in ./input/
#						   run - runs all *.run files in ./output/


OC=~schwesin/.opam/default/bin/ocamlbuild

all: main

main: main.ml
	$(OC) main.native


.PHONY compile:
compile: main | output
	-for i in ./input/*.int; do ./compile.sh $$i; done
	-mv *.s *.run output

output:
	@mkdir -p $@

.PHONY run:
run: compile
	for i in ./output/*.run; do printf "%s:  " $$i; ./$$i; done


.PHONY clean:
clean:
	rm -rf output
	rm -rf main.native
	rm -rf _build
	rm -rf *~ ./input/*~
	rm -rf *.s *.run
