interp: interpd
	lex simp.lex
	yacc simp.yacc
	gcc y.tab.c lex.yy.c -o interp

interpd:	simp.lex simp.yacc
	lex -d simp.lex
	yacc -d simp.yacc
	gcc y.tab.c lex.yy.c -o interpd

clean:
	rm -f y.tab.c y.tab.h lex.yy.c interpd interp
