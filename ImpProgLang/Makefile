build:
	mono ../fslex.exe Lexer.fsl
	mono ../fsyacc.exe --module Parser Parser.fsy
	fsharpi Script.fsx
