all:
	flex -o expr.lex.c expr.lex
	bison -d -o expr.y.c expr.y
	gcc -c -o expr.y.o expr.y.c `llvm-config --cflags`
	gcc -c -o expr.lex.o expr.lex.c `llvm-config --cflags`
	g++ -o expr expr.y.o expr.lex.o `llvm-config --ldflags --libs` -ly -ll `llvm-config --system-libs` 

scanner:
	flex -o expr.lex.c expr.lex
	gcc -Wno-implicit-function-declaration -o expr expr.lex.c -ll

clean:
	rm -Rf expr.lex.c expr.y.c expr.y.h *.o expr *~
