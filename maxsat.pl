:-lib(ic).
:-lib(branch_and_bound).

create_formula(NVars, NClauses, Density, Formula) :-
   formula(NVars, 1, NClauses, Density, Formula).

formula(_, C, NClauses, _, []) :-
   C > NClauses.
formula(NVars, C, NClauses, Density, [Clause|Formula]) :-
   C =< NClauses,
   one_clause(1, NVars, Density, Clause),
   C1 is C + 1,
   formula(NVars, C1, NClauses, Density, Formula).

one_clause(V, NVars, _, []) :-
   V > NVars.
one_clause(V, NVars, Density, Clause) :-
   V =< NVars,
   rand(1, 100, Rand1),
   (Rand1 < Density ->
      (rand(1, 100, Rand2),
       (Rand2 < 50 ->
        Literal is V ;
        Literal is -V),
       Clause = [Literal|NewClause]) ;
      Clause = NewClause),
   V1 is V + 1,
   one_clause(V1, NVars, Density, NewClause).

rand(N1, N2, R) :-
   random(R1),
   R is R1 mod (N2 - N1 + 1) + N1.

makeCostList(Helper,C):-    % το κοστος ειναι 1 η 0 αναλογα με το αν το αθροισμα ειναι θετικο η οχι ( θετικο = τουλαχιστον 1 ασσος)
  C #= (sum(Helper) #> 0).

same(H,AbH,Val,HH):- % αν ειναι ομοσημα  κραταω ιδια τα προσημα
  H =:= AbH, 
  HH #= (Val #= 1).

same(H,AbH,Val,HH):- % αν  δεν ειναι ομοσημα πρεπει να αλλαζω τα προσημα
  H =\= AbH,
  HH #= (Val #=0).

createHelper([],S,[]).
createHelper([H|Rest],S,[HH|Helper]):-
  H1 is abs(H),
  element(H1,S,Val),
  same(H,H1,Val,HH),
  % HH #= ((H1 #= H and Val#=1) or (H1 #\= H and Val #=0 )) ,
  createHelper(Rest,S,Helper).

externalLoop([],S,[]) .
externalLoop([H|F],S,[C|CostList]):-

  length(H,LengthH),
  length(Helper,LengthH),
  createHelper(H,S,Helper), %φτιαχνω τη λιστα με τις αντισοιχες αναθεσεις  απο την S

  makeCostList(Helper,C), %φτιαχνω το κοστος για καθε προταση του τυπου
  externalLoop(F,S,CostList).

maxsat(NV,NC,D,F,S,M):-
  create_formula(NV,NC,D,F),
  length(S,NV),
  S #:: [0,1], 
  externalLoop(F,S,CostList),
  M #= sum(CostList),
  MM #= -sum(CostList),
  bb_min(search(S, 0, first_fail, indomain_middle, complete, []),MM,bb_options{strategy:continue}).