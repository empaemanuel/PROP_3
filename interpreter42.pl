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

statements(statements(Assignment))-->
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

int(int(I))-->[I].
ident(ident(A)) --> [A].



/***
evaluate(+ParseTree,+VariablesIn,-VariablesOut):-
	Evaluates a parse-tree and returns the state of the program
	after evaluation as a list of variables and their values in 
	the form [var = value, ...].
***/

evaluate(block(LeftCurly,Statements,RightCurly),VarsIn,VarsOut):-
	evaluate(Statements,VarsIn,VarsOut).

evaluate(statements(Assignment, Statements),VarsIn,VarsOut):-
	evaluate(Assignment,VarsIn,Out),
	evaluate(Statements,Out,VarsOut).

evaluate(statements(Assignment),VarsIn,VarsOut):-
	evaluate(Assignment,VarsIn,VarsOut).

evaluate(assignment(Id,AssignmentOperator,Expression,Semicolon),VarsIn,[(VarOut=ValOut)|VarsIn]):-
	evaluate(Id,VarsIn,VarOut),
	evaluate(Expression,VarsIn,ValOut).

evaluate(expression(Term),VarsIn,VarsOut):-
	evaluate(Term,VarsIn,VarsOut).

evaluate(expression(Term,Operator,Expression),VarsIn,Out):-
	Operator=sub_op,
	evaluate(Term,VarsIn,ValueFirst),
	evaluate(Expression,VarsIn,ValueSecond),
	Out=ValueFirst - ValueSecond.

evaluate(expression(Term,Operator,Expression),VarsIn,Out):-
	Operator=add_op,
	evaluate(Term,VarsIn,ValueFirst),
	evaluate(Expression,VarsIn,ValueSecond),
	Out=ValueFirst + ValueSecond.

evaluate(term(Factor),VarsIn,VarsOut):-
	evaluate(Factor,VarsIn,VarsOut).

evaluate(term(Factor, Operator, Term),VarsIn,Out):-
	Operator = mult_op,
	evaluate(Factor,VarsIn,ValueFirst),
	evaluate(Term,VarsIn,ValueSecond),
	Out=ValueFirst * ValueSecond.

evaluate(term(Factor, Operator, Term),VarsIn,Out):-
	Operator = div_op,
	evaluate(Factor,VarsIn,ValueFirst),
	evaluate(Term,VarsIn,ValueSecond),
	Out=ValueFirst / ValueSecond.

evaluate(factor(Int),VarsIn,VarsOut):-
	evaluate(Int,VarsIn,VarsOut).

evaluate(factor(LeftParen, Expression, RightParen),VarsIn,VarsOut):-
	evaluate(Expression,VarsIn,VarsOut).

evaluate(int(Int),VarsIn,Int).
evaluate(ident(Id),VarsIn,Id).


	
	















