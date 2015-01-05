CC = mono
CI = fsharpi

LEX = ../bin/fslex.exe
PAR = ../bin/fsyacc.exe

all: build

build:
	$(CC) $(LEX) Lexer.fsl
	$(CC) $(PAR) --module Parser Parser.fsy
	$(CI) Script.fsx

.PHONY: all build
