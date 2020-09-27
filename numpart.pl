:- lib(ic).

sumlist([],0).
sumlist([X|T],Total):-
	sumlist(T,Sum1),
	Total is X + Sum1.

createSum(0,Sums).
createSum(N,[S|Sums]):-
	S is N,
	N1 is N - 1,
	createSum(N1,Sums).

createSquare(0,Squares).
createSquare(N,[S|Squares]):-
	S is N*N,
	N1 is N - 1,
	createSquare(N1,Squares).

sorted([_]).
sorted([]).
sorted([H1,H2|Rest]):-
	H1 #< H2,
	sorted([H2|Rest]).

constraints(L1,L2,S1,S2,N, New1, New2):-
	N >= 8,
	ModN is mod(N,4), 
	ModN =:= 0,

	append(L1,L2,L3),
	alldifferent(L3),

	sum(L1) #= New1,
	sum(S1) #= New2,
	min(L1) #=< min(L2),

	sorted(L1),
	sorted(L2).

makeSquare([],_,New2).
makeSquare([X|L1],[Hs|S1],New2) :-
	X*X #=< New2,!,
	Hs #= X*X,
	makeSquare(L1,S1,New2).

numpart(N,L1,L2):-
	ListLength is div(N,2),
	length(L1,ListLength),
	length(L2,ListLength),
	length(S1,ListLength),
	length(S2,ListLength),
	L1 #:: 1..N,
	L2 #:: 1..N,

	length(Sums,N),
	length(Squares,N),
	createSum(N,Sums),
	createSquare(N,Squares),
	sumlist(Sums,Total1),
	sumlist(Squares,Total2),
	New1 is div(Total1,2),
	New2 is div(Total2,2),

	makeSquare(L1,S1,New2),
	makeSquare(L2,S2,New2),
	constraints(L1,L2,S1,S2,N,New1,New2),
	search(L1,0,occurence,indomain_middle,complete,[]),
	search(L2,0,largest,indomain_reverse_split,complete,[]).