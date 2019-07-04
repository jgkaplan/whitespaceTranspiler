.PHONY: all build clean

all: build

build:
	ocamlbuild -use-ocamlfind -use-menhir -I src main.byte

clean:
	ocamlbuild -clean
