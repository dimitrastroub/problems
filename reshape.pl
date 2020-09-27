
check([],K,[]).
check(L1,K,K).

remakeList([],[]).
remakeList([[KHead|H]|Tail],[H|Tail2]):-
	remakeList(Tail,Tail2).

eachFirstElement([],[]).
eachFirstElement([[H|T]|L],[H|M]):-
	eachFirstElement(L,M).

sol([],[],0).
sol([H|L],[T|M],_):-
	eachFirstElement([H|L],T),
	remakeList([H|L],K),
	sameList(K,L1),
	length(L1,Len1),
	check(L1,K,New),!,
	sol(New,M,Len1).

reshape(List,M):-
	sol(List,M,Len).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%00000

numberList(0,[]).
numberList(X,[M|L]):-
	H is div(X,10),
	M is mod(X,10),
	numberList(H,L).

numberList2([],[],Counter).
numberList2([HH|L],[M1|L1],Counter):-
	M1 is Counter*HH,
	NewC is Counter*10,
	numberList2(L,L1,NewC).

 comparison(L1,L2,0):-
 	sort(L1,T1),
 	sort(L2,T2),
 	numberList2(T1,New1,1),
 	numberList2(T2,New2,1),
 	sumlist(New1,N1),
 	sumlist(New2,N2),
 	N1 =:= N2.

 comparison(L1,L2,1):-
 	sort(L1,T1),
 	sort(L2,T2),
 	numberList2(T1,New1,1),
 	numberList2(T2,New2,1),
 	sumlist(New1,N1),
 	sumlist(New2,N2),
 	N1 =\= N2.


checkVal(L,N1,NMax,0,N1).
checkVal(L,N1,NMax,1,B):-
	solut(L,N1,NMax,B).

solut(L,NMax,NMax,0).
solut(L,Nmin,NMax,B):-
	N1 is Nmin +1,
	numberList(N1,T),
	reverse(T,T1),
	comparison(L,T1,Val),
	checkVal(L,N1,NMax,Val,B).
	%an einai 0 stamataw
	%an einai 1 synexizw
	%solut(L,N1,NMax,B).

newnum(X,B):-
	numberList(X,L),
	msort(L,Min1),
	reverse(Min1,Max1),
	numberList2(Max1,Min,1),
	numberList2(Min1,Max,1),
	sumlist(Max,NMax),
	sumlist(Min,NMin),
	solut(L,X,NMax,B),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

timesExist(_, [], 0).
timesExist(X, [X | T], N) :-!, 
  timesExist(X, T, N1),
  N is N1 + 1.

timesExist(X, [_ | T], N) :-
  timesExist(X, T, N).


rev([],[]).
rev([F|Final],[A|Tail]):-
	reverse(F,A),
	rev(Final,Tail).

breakHereFirst(N,[],[]).
breakHereFirst(N,[H|L],[L|Cut]):-
	N == H,
	length(L,N1),
	N1 >= 2,
	breakHereFirst(N,L,Cut).

breakHereFirst(N,[H|L],Cut):-
	breakHereFirst(N,L,Cut).


breakHereSec(N,[],[]).
breakHereSec(N,[H|L],[L|Cut]):-
	N == H,
	length(L,N1),
	N1 >= 1,
	breakHereSec(N,L,Cut).

breakHereSec(N,[H|L],Cut):-
	breakHereSec(N,L,Cut).

restList(_,_,[],0):-!.

restList([HH|L1],L2,[HH|L3],Dif):-
	length(L1,Len1),
	length(L2,Len2),
	NDif is Len1 - Len2,
	restList(L1,L2,L3,NDif).


tailList([A|TSort],TSort).

otherList(_,[],[]).
otherList(L,[N|NewCut],[Here|MakeHere]):-
	restList(L,N,Here,1),
	otherList(L,NewCut,MakeHere).

headListFirst([A|TSort],A,L):-
	nth0(X,L,A),
	length(L,Len),
	K is Len - X,
	K >= 3.

headListFirst([_|TSort],A,L):-
	headListFirst(TSort,A,L).

headListSec([Pos|TSort],Pos,L):-
	nth0(X,L,Pos),
	length(L,Len),
	K is Len - X, 
	K >= 2.

headListSec([_|TSort],A,L):-
	headListSec(TSort,A,L).

cutList(L,[],L).
cutList([H|L],[B|Back],L3):-
	cutList(L,Back,L3).

sameList([],[]).
sameList([L1|L2], FL) :- !, 
	sameList(L1, FL1), 
	sameList(L2, FL2), 
	append(FL1, FL2, FL).
sameList(X, [X]).


lexicographic(L,M):-
	msort(L,SortList),
	headListFirst(SortList,P,L),
	timesExist(P,L,N),
	breakHereFirst(P,L,Cut),
	append([L],Cut,NewCut),
	tailList(NewCut,B),
	otherList(L,B,MakeHere),
	rev(MakeHere,Rev),
	sort(Rev,Sor),
	headList(Sor,FirstPart),

	reverse(FirstPart,Back),
	cutList(L,Back,L3),
	msort(L3,Sort2),
	headListSec(Sort2,C,L3),
	timesExist(C,L3,No),
	breakHereSec(C,L3,Cut2),
	append([L],Cut2,NewCut2),
	tailList(NewCut2,B2),
	otherList(L3,B2,MakeHere2),
	rev(MakeHere2,Rev2),
	sort(Rev2,Sor2),
	headList(Sor2,SecondPart),


	sameList(MakeHere2,M1),
	cutList(L3,M1,ThirdPart),
	reverse(ThirdPart,RT),

	append(FirstPart,SecondPart,T),
	append(T,RT,M),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

index([], _, [], _). 
index([X|L],H,[C|Collect],C) :-
	X =:= H,
    NewC is C + 1,
    index(L,H,Collect,NewC).

index([X|L],H,Collect,C) :- 
    NewC is C + 1,
    index(L,H,Collect,NewC).

distance(L,[],[],[]).
distance(L,[H|Socks],[T|Total],[Collect|C]):-
	index(L, H, Collect, 0),
	max_list(Collect,MaxC),
	min_list(Collect,MinC),
	T is MaxC - MinC - 1,
	distance(L,Socks,Total,C).

createCurrent(_,MaxC,MaxC,[]).
createCurrent(L,MinC,MaxC,[V|Subs]):-
	nth0(MinC,L,V),
	Nc is MinC + 1,
	createCurrent(L,Nc,MaxC,Subs).

chain(_,[],[],[]).
chain(L,[C|Collect],[Subs|W],[T1|Tail]):-
	length(L,N),
	max_list(C,MaxC),
	min_list(C,MinC),
	NewMax is MaxC + 1,
	createCurrent(L,MinC,NewMax,Subs),
	Max2 is N + 1,
	append(L,[a],T),
	createCurrent(T,NewMax,Max2,T1),
	chain(L,Collect,W,Tail).

headList([A|TSort],A).

commonElements([],_,[]).
commonElements([H|L1],L2,[1|L3]):-
	member(H,L2),
	commonElements(L1,L2,L3).

commonElements([H|L1],L2,L3):-
	commonElements(L1,L2,L3).


checkIsChain(0,H,T,0).
checkIsChain(Mod,H,T,M):-
	commonElements(H,T,Make),
	sumlist(Make,M).

makeChains([],[],[]).
makeChains([H|W],[T|Tail],[Val|ChainsArray]):-
	length(H,N),
	sort(H,S),
	length(S,N1),
	Mod is mod(N,N1),
	checkIsChain(Mod,H,T,Val),
	makeChains(W,Tail,ChainsArray).

swapsocks(L,M):-
	length(L,N),
	NumSocks is div(N,2),
	sort(L,Socks),
	distance(L,Socks,Total,Collect),
	chain(L,Collect,W,T),
	makeChains(W,T,ChainsArray),
	sumlist(Total,Dis),
	sumlist(ChainsArray,Ch),!,
	M is Dis - Ch.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

deleteElement(L1,[],_,[]).
deleteElement(L1,[H|L],Index,[H|New]):-
	nth0(In,L1,H),
	dif(Index,In),
	deleteElement(L1,L,Index,New).

deleteElement(L1,[H|L],Index,New):-
	deleteElement(L1,L,Index,New).

deletion([_|Tail], Tail,0).
deletion([H|T], [H|New],Counter):-
   	NC is Counter - 1,
    deletion(T, New,NC).

makeLine(L,[],L).
makeLine(L,[H|Tail],List):-
	length(Tail,Len),
	Len = 0,
    deleteElement(L,L,H,List).

makeLine(L, [H|T], List):-
    makeLine(L, T, List1),
    deleteElement(List1,List1,H,List).

loopp([],Rev,[]):-!.
loopp([L|List],Rev,[Fin|Make]):-
	makeLine(L,Rev,Fin),
	loopp(List,Rev,Make).

breakPart([M|Make],M,Make).

elem([],[],_,_,_).
elem([H|T], New,List, L1, Counter):-
    member(H, L1),
   	NC is Counter + 1,
    elem(T, New,List, L1, NC).

elem(_, [Counter|New], List, L1, Counter):-
  	deletion(L1, A, Counter),
    deletion(List, List1, Counter),
    elem(List1,New, List1, A, 0).


mincol([L1,L2,L3],K):-
	Key is 0,
    elem(L1,Ret,L1, L2, Key),
    reverse(Ret, Rev1),

    append([L1],[L2],L),
    append(L,[L3],Fn),
    loopp(Fn,Rev1,Make),

    breakPart(Make,First,Rest),
    breakPart(Rest,Sec,R2),
    breakPart(R2,Third,Tail),

    elem(First,Ret2, First, Third,Key),
    reverse(Ret2, Rev2),

    loopp(Make,Rev2,F),
    breakPart(F,Fir,Res),
    breakPart(Res,Se,R3),
    breakPart(R3,Thir,Tail2),

    elem(Se,Ret3, Se, Thir,Key),
    reverse(Ret3, Rev3),

    makeLine(Fir, Rev3,Final),

    length(L1,Len1),
    length(Final,Len2),!,
    K is Len1-Len2.

 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 sortSuitable([],[]).
sortSuitable([[X1,X2]|Tail],[X1,X2]):-
	length(Tail,N),
	N =:= 0.

sortSuitable(L,[X1|Tail]):-
	length(List,0),
    findfirst(List,L,[X1,A]),
    delElem(X1,L,N),
    sortSuitable(N,Tail).


iscorrect([],[],Key).
iscorrect([Head|Tail],[Head|T],Key):-
    timesExist(Head,[Head|Tail],N),
    N >= Key,
    iscorrect(Tail,T,Key).

iscorrect([_|Tail],T,Key):-
	timesExist(Head,[Head|Tail],N),
    iscorrect(Tail,T,Key).

findfirst(First,[[X1,X2]|L],F):-
	append([X1,X2],[],L2),
    append(First,L,Fin),
    append(First,[L2],New),
    isElem(X1,Fin),
    findfirst(New,L,F).
findfirst(_,[F|_],F).

isElem(A,[[X1,X2]|Tail]):-
	A =:= X2.

isElem(A,[[X1,X2]|Tail]):-
	dif(A,X2),
    isElem(A,Tail).

first([_|Tail],[]):- 
	length(Tail,N),
	N =:=0.

first([Head|Tail],L):-
    sec(Head,Tail,L2),
    append(L2,L1,L),
    first(Tail,L1).

sec(A,[],[]).
sec(A,[Head|Tail],[L|F]):-
	sec(A,Tail,F),
	append([A],[Head],L).

subList([],[]).
subList([Head|T],A):-
    first(Head,Fn),
    append(Fn,L1,A),
    subList(T,L1).

delElem(_,[],[]).
delElem(F,[[X,_]|T], New):-
	F =:= X,
    delElem(F,T,New).

delElem(F,[[X,X1]|T],[[X,X1]|New]):-
	dif(F,X),
    delElem(F,T,New).

phototorture(L,M):-
	Key is 3,
    subList(L,Subs),
    iscorrect(Subs,Rev,Key),
    sort(Rev,L2),
    sortSuitable(L2,M),!.
