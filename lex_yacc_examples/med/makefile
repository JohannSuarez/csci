interp: interpd
	lex med.lex
	yacc med.yacc
	gcc y.tab.c lex.yy.c -o interp

interpd:	med.lex med.yacc
	lex -d med.lex
	yacc -d med.yacc
	gcc y.tab.c lex.yy.c -o interpd

clean:
	rm -f y.tab.c y.tab.h lex.yy.c interpd interp
