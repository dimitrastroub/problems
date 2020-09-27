sumlist([],0).
sumlist([X|T],Total):-
	sumlist(T,Sum1),
	Total is X + Sum1.

eachTime([],0).
eachTime([H|T],Total):-
	activity(H,act(S,E)),
	Sum is E-S,
	eachTime(T,NewS),
	Total is Sum + NewS.

between(First,End, Elem):-
	First < End,
	Firstnew is First + 1,
	between(Firstnew,End,Elem).

makeTemplateASP(Size, Size,[Size-_-_]).
makeTemplateASP(I,Size,[I-Acts-Count|Tail]):-
	I < Size,
	Inew is I + 1,
	makeTemplateASP(Inew,Size,Tail).

makeTemplateASA([],[]).
makeTemplateASA([Head|L],[Head-Person|Tail]):-
	makeTemplateASA(L,Tail).

restrictionASA(A-Person,[],ST,Duration1).  
restrictionASA(A-Person,[A1-P1|L],ST,Duration1):-
	P1 =:= Person,
	activity(A,act(S1,E1)),
	activity(A1,act(S2,E2)),
	Duration2 is E2 - S2,
	Sum is Duration1 + Duration2,
	Sum < ST + 1,
	(S1 > E2 ; E1 < S2),
	activity(A1,act(S2,E2)),
	Duration2 is E2 - S2,
	NewD is Duration1 + Duration2,
	restrictionASA(A-Person,L,ST,NewD).
	
restrictionASA(A-Person,[A1-P1|L],ST,Duration1):-
	Person =\= P1,
	Duration1 < ST + 1,
	activity(A1,act(S2,E2)),
	Duration2 is E2 - S2,
	NewD is Duration1 + Duration2,
	restrictionASA(A-Person,L,ST,Duration1).

solveASA(_,_,[]).
solveASA(NP,ST,[A-Person|Rest]):-
	solveASA(NP,ST,Rest),
	activity(A,act(S1,E1)),
	Duration1 is E1 - S1,
	between(1,NP,Person),
	restrictionASA(A-Person,Rest,ST,Duration1).
	
makeASP(ASA,[]).
makeASP(ASA,[P1-L1-Total|Tail]):-
	findall(A, member(A-P1,ASA), L1),
	eachTime(L1,Total),
	makeASP(ASA,Tail).

assignment(NP,ST,ASA,ASP):-
	makeTemplateASP(1,NP,ASP),
	findall(X,activity(X,_),L),
	makeTemplateASA(L,ASA),
	append(ASP,[],ASP1),
	solveASA(NP,ST,ASA),
	makeASP(ASA,ASP).