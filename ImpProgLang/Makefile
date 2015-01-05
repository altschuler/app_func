CC = mono
CI = fsharpi

LEXGEN = ../bin/fslex.exe
PARGEN = ../bin/fsyacc.exe

LEX = Lexer
PAR = Parser

all: build

$(LEX).fs: $(LEX).fsl
	@echo Generating lexer...
	$(CC) $(LEXGEN) $(LEX).fsl

$(PAR).fs: $(PAR).fsy
	@echo Generating parser...
	$(CC) $(PARGEN) --module $(PAR) $(PAR).fsy

clean:
	@rm -rf $(LEX).fs $(PAR).fs

build: $(LEX).fs $(PAR).fs
	@echo Building...
	$(CI) Script.fsx

.PHONY: all clean build
