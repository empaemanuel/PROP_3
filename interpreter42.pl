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
	%evaluate(ParseTree,[],VariablesOut), 

	%write_to_file(OutputFile,ParseTree,VariablesOut).
	write_to_file(OutputFile,ParseTree,[a = 0.0]).

/***
parse(-ParseTree)-->
	A grammar defining your programming language,
	and returning a parse tree.
***/

/**
The parser should take a list of lexemes/tokens as input, and from 
that list of lexemes/tokens create a parse tree as output.
**/

parse(assignment(Id,AssignOperator,Expr,Semicolon)) -->
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
semicolon(semicolon)-->[;].

int(int(I))-->[I].
ident(ident(A)) --> [A].




/***
evaluate(+ParseTree,+VariablesIn,-VariablesOut):-
	Evaluates a parse-tree and returns the state of the program
	after evaluation as a list of variables and their values in 
	the form [var = value, ...].
***/

/* WRITE YOUR CODE FOR THE EVALUATOR HERE */
