CC = fsharpc
MO = mono

SRC = src/Utils.fs src/Exceptions.fs src/Core.fs \
	src/Service.fs src/UI.fs src/GUI.fs \
	src/CLI.fs src/Driver.fs src/Main.fs
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

cli: build
	@echo ''
	$(MO) $(EXE) -nw

.PHONY: all clean build run cli
