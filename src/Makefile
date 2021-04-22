#
# Makefile for building command line app to be called by interface.r

BIN_LIST = cell-ontology-mapping

cell-ontology-mapping: $(wildcard src/*.hs) $(wildcard app/*.hs) package.yaml stack.yaml
	~/.local/bin/stack install
