.SILENT:

all:
	corebuild -quiet -classic-display src/main.native
	#ocamlbuild -tag thread -use-ocamlfind -quiet -pkg core src/main.native

run: all
	./main.native

.PHONY: all run
