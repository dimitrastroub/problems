addlast([X|L],L1,L2):-append(L1,[X],L2).

printing([]).
printing([X|B]):-
	write(X),
	printing(B).

doloop([],Pattern,K,B,[]) :- 
	printing(B),nl.

doloop([],Pattern,K,B,N) :- 
	reverse(B,A),
	printing(A),nl.

doloop(Width,[],K,B,N):-
	doloop(Width,K,K,B,N).

doloop([W|Width],[P|Pattern],K,[],N):-
	append([P],[],B),
	doloop(Width,Pattern,K,B,N).

doloop([W|Width],[P|Pattern],K,Help,N):-
	addlast([P],Help,B),
	doloop(Width,Pattern,K,B,N).

changePattern(L2,[],[],B,Width2,[],[]).
changePattern(L2,[],[F|Height],B,Width2,[],[]):-
	append([],[a],N),
	doloop(Width2,L2,L2,[],N),
	changePattern(L2,Width2,Height,B,Width2,[],N).

changePattern([P|Pattern],[W|Width],Height,B,Width2,[],[]):-
	addlast([P],Pattern,L2),
	changePattern(L2,Width,Height,B,Width2,[],[]).


changePattern(L2,[],[],B,Width2,[],N).
changePattern(L2,[],[F|Height],B,Width2,[],N):-
	doloop(Width2,L2,L2,[],[]),
	changePattern(L2,Width2,Height,B,Width2,[],[]).

changePattern([P|Pattern],[W|Width],Height,B,Width2,[],N):-
	addlast([P],Pattern,L2),
	changePattern(L2,Width,Height,B,Width2,[],N).


disnake(Pattern,Width,[F|Height]):-
	append([],Pattern,K),
	append([],Width,B),
	append([],Width,Width2),
	doloop(Width,Pattern,K,[],[]),
	changePattern(Pattern,Width2,Height,B,Width2,[],[]).

