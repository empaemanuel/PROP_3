/*** 
Grupp 42
Mohammed Hussein & Emil Madrell
***/

/***
A skeleton for Assignment 3 on PROP HT2019 at DSV/SU.
Peter Idestam-Almquist, 2019-12-09.
***/

/*** 
Load the tokenizer (tokenize/2) and the file_writer (write_to_file/3).
***/

:-[tokenizer].
:-[filewriter].

/***
The top level predicate run/2 of the solution.
To be called like this:
?- run('program3.txt','myparsetree3.txt').
***/

run(InputFile,OutputFile):-
	tokenize(InputFile,Program),
	parse(ParseTree,Program,[]),
	evaluate(ParseTree,[],VariablesOut), 

	write_to_file(OutputFile,ParseTree,VariablesOut).
	%write_to_file(OutputFile,ParseTree,[a=1111]).

/***
parse(-ParseTree)-->
	A grammar defining your programming language,
	and returning a parse tree.
***/

/**
The parser should take a list of lexemes/tokens as input, and from 
that list of lexemes/tokens create a parse tree as output.
**/


parse(block(LeftCurly,Statements,RightCurly))-->
	left_curly(LeftCurly),
	statements(Statements),
	right_curly(RightCurly).

parse(block(LeftCurly,RightCurly))-->
	left_curly(LeftCurly),
	right_curly(RightCurly).

statements(statements(Assignment,statements))-->
	assignment(Assignment).

statements(statements(Assignment,Statements)) -->
	assignment(Assignment),
	statements(Statements).

assignment(assignment(Id,AssignOperator,Expr,Semicolon)) -->
	ident(Id), 
	assign_op(AssignOperator),
	expression(Expr),
	semicolon(Semicolon).

expression(expression(Term)) -->
	term(Term).

expression(expression(Term,AddOp,Expression)) -->
	term(Term),
	add_op(AddOp),
	expression(Expression).

expression(expression(Term,SubOp,Expression)) -->
	term(Term),
	sub_op(SubOp),
	expression(Expression).

term(term(Factor)) -->
	factor(Factor).

term(term(Factor,MultOp,Term)) -->
	factor(Factor),
	mult_op(MultOp),
	term(Term).

term(term(Factor,DivOp,Term))-->
	factor(Factor),
	div_op(DivOp),
	term(Term).

factor(factor(Int))-->
	int(Int).

factor(factor(Id))-->
	ident(Id).

factor(factor(LeftParen,Expression,RightParen))-->
	left_paren(LeftParen),
	expression(Expression),
	right_paren(RightParen).

add_op(add_op)-->['+'].
sub_op(sub_op)-->['-'].
mult_op(mult_op)-->['*'].
div_op(div_op)-->['/'].
assign_op(assign_op) --> [=].
left_paren(left_paren) --> ['('].
right_paren(right_paren) --> [')'].
left_curly(left_curly)--> ['{'].
right_curly(right_curly)--> ['}'].
semicolon(semicolon)-->[;].

ident(ident(Ident))-->[Ident].
int(int(Int))-->[Int],{number(Int)}.


/***
evaluate(+ParseTree,+VariablesIn,-VariablesOut):-
	Evaluates a parse-tree and returns the state of the program
	after evaluation as a list of variables and their values in 
	the form [var = value, ...].
***/

evaluate(ParseTree,VarsIn,VarsOut):-
	ParseTree=block(_,Statements,_),
	VarsIn=[],
	evaluate(Statements,VarsIn,VarsOut).

evaluate(Statement, VarsIn, VarsOut):-
	Statement=statements(Assignment, RecStatement),
	evaluate(Assignment, VarsIn, NewAssignments),
	evaluate(RecStatement, NewAssignments, VarsOut).

evaluate(Statement,Vars,Vars):-
	Statement=statements.

evaluate(Assignment,VarsIn,[(Id=Value)|VarsIn]):-
	Assignment=assignment(ident(Id),_,Expression,_),
	evaluate(Expression, VarsIn, Value).

evaluate(Expression,Vars,Value):-
	Expression=expression(Term),
	evaluate(Term,Vars,Value).

evaluate(Expression,Vars,Sum):-
	Expression=expression(Term,sub_op,RecExpression),
	evaluate(Term,Vars,TermValue),
	evaluate(RecExpression,Vars,ExpressionValue),
	Sum=TermValue-ExpressionValue.

evaluate(Term,_,X):-
	Term=term(factor(int(X))).

evaluate(Term,[],0):-
	Term=term(factor(ident(Id))).

evaluate(Term,[H|VarsIn],X):-
	Term=term(factor(ident(Id))),
	H=(Id=X).

evaluate(Term,[_|VarsIn],X):-
	Term=term(factor(ident(Id))),
	evaluate(Term,VarsIn,X).


/*
evaluate(term(factor(ident(Id))),[],0).

evaluate(Term,[H|Vars],X):-
	Term=term(factor(ident(Id)),
	write_to_file("deb.txt",H,[a=2])

evaluate(Term,[H|Vars],X):-
	Term=term(factor(ident(Id))),
	evaluate(Term,Vars,X).*/












