% Aaron Shehan
% ats0109


cdr([X|Xs], Xs).
car([X|Xs], X).

findName([], Name).
findName([H|T], Name) :-
	car(H, Tmp),
	(Tmp \= Name),
	findName(T, Name).

add(OldSymbolTable, Name, Category, Type, Value, NewSymbolTable) :-
	findName(OldSymbolTable, Name),
	append(OldSymbolTable, [[Name, Category, Type, Value]], NewSymbolTable).

entry([], Name, Category, Type, Value) :- !, fail.
entry(SymbolTable, Name, Category, Type, Value) :-
	[H0|T0] = SymbolTable,
	[H|T] = H0,
	(H = Name ->
		[H2|T2] = T,
		Category = H2,
		[H3|T3] = T2,
		Type = H3,
		car(T3, Value);
		entry(T0, Name, Category, Type, Value)).




category([], Name, Category) :- !, fail.
category(SymbolTable, Name, Category) :-
	[H0|T0] = SymbolTable,
	[H|T] = H0,
	(H = Name ->
		car(T, Category);
		category(T0, Name, Category)).


type([], Name, Type) :- !, fail.
type(SymbolTable, Name, Type) :-
	[H0|T0] = SymbolTable,
	[H|T] = H0,
	(H = Name ->
		cdr(T, Tmp),
		car(Tmp, Type);
		type(T0, Name, Type)).

value([], Name, Value) :- !, fail.
value(SymbolTable, Name, Value) :-
	[H0|T0] = SymbolTable,
	[H|T] = H0,
	(H = Name ->
		cdr(T, Tmp),
		cdr(Tmp, Tmp2),
		car(Tmp2, Value);
		value(T0, Name, Value)).