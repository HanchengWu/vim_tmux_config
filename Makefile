all:
	flex -o expr.lex.c expr.lex
	bison -d -o expr.y.c expr.y
	gcc -o expr expr.y.c expr.lex.c -ly

scanner:
	flex -o expr.lex.c expr.lex
	gcc -o expr expr.lex.c -ll

clean:
	rm -Rf expr.lex.c expr.y.c expr.y.h *.o expr *~
