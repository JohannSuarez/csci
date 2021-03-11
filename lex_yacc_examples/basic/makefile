interp: interpd
	lex basic.lex
	yacc basic.yacc
	gcc y.tab.c lex.yy.c -o interp

interpd:	basic.lex basic.yacc
	lex -d basic.lex
	yacc -d basic.yacc
	gcc y.tab.c lex.yy.c -o interpd

clean:
	rm -f y.tab.c y.tab.h lex.yy.c interpd interp
