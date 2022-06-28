all: saida entrada.txt
	./tradutor < entrada.txt

lex.yy.c: tradutor.l
	lex tradutor.l

saida: lex.yy.c main.cc 
	g++ -Wall -std=c++17 lex.yy.c -ll -o tradutor
	
clean:
	rm tradutor lex.yy.c