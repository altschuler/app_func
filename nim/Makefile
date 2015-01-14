CC = fsharpc
MO = mono

SRC = src/Core.fs src/Util.fs src/Main.fs
EXE = bin/nim.exe

all: run

clean:
	@rm -rf bin/*

$(EXE): $(SRC)
	$(CC) $(SRC) -o $(EXE)

build: clean $(EXE)

run: build
	@echo ''
	$(MO) $(EXE)

.PHONY: all clean build run
