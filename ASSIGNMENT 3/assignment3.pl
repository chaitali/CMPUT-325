%CMPUT 325 B1
%Assignment 3



/*
---------------------------------------------------------

Question 1

xreverse(+L, +R)

Given a list L or R, the reverse ordered list is generated. 

test cases: 

xreverse([7,3,4],[4,3,7]) should return true, 
xreverse([7,3,4],[4,3,5]) should return false, 
xreverse([7,3,4], R) should return R = [4,3,7]. 

--------------------------------------------------------- */

xreverse(L, R) :- xreverse(L, [], R). 
xreverse([], R, R).
xreverse([LF|LR], SoFar, R) :- xreverse(LR, [LF|SoFar], R).


/*
---------------------------------------------------------

Question 2

xunique(+L, +Lu)

Given a list, L, of atoms the result Lu is a copy of L
where all the duplicates are removed. 

test cases: 

xunique([a,c,a,d], L) should return L = [a,c,d], 
xunique([a,c,a,d], [a,c,d]) should return yes, 
xunique([a,c,a,d], [c,a,d]) should return no (because of wrong order), 
xunique([a,a,a,a,a,b,b,b,b,b,c,c,c,c,b,a], L) should return L = [a,b,c], 
xunique([], L) should return L = [].

--------------------------------------------------------- */
 
xunique(L, Lu) :- xunique(L, [], Lu).
xunique([LF|LR], Lu, R) :-
       notMember(LF, Lu), 
       append(Lu, [LF], SoFar),
       xunique(LR, SoFar, R).
xunique([LF|LR], Lu, R) :-
       member(LF, Lu),
       xunique(LR, Lu, R).
xunique([],R,R).

/*
---------------------------------------------------------

notMember(+_, ?L)

Given an atom and a list, checks to see if the atom is not
a member of list L. 

notMember(3, [1, 2, 3]) should return false, 
notMember(3, [1, 2]) should return true. 

--------------------------------------------------------- */

notMember(_,[]).
notMember(A,[B|R]) :- 
   A \== B,
   notMember(A,R).


/*
---------------------------------------------------------

Question 3

xunion(+L1, +L2, +L)

Given a list of atoms L1 and L2, the result returns a list L
where L contains the unique elements that are contained in both
L1 and L2, in the same order with no redundancy. 

xunion([a,c,a,d], [b,a,c], L) should return L = [a,c,d,b], 
xunion([a,c,d], [b,a,c], [a,c,d,b]) should return true, 
xunion([a,c,d], [b,a,c], [a,c,d,b,a]) should return false.



--------------------------------------------------------- */


xunion(L1, L2, L) :-  
      append(L1, L2, X),
      xunique(X, Lu),
      append(Lu, [], L).


/*
---------------------------------------------------------

Question 4

removeLast(+L, ?L1, ?Last)

Given a non-empty list L, L1 is the result of removing the 
last element from L, and Last is the last element of the list. 
L1 and Last can be given values or variables. 


removeLast([a,c,a,d], L1, Last) should return L1 = [a,c,a], Last = d, 
removeLast([a,c,a,d], L1, d) should return L1 = [a,c,a], 
removeLast([a,c,a,d], L1, [d]) should return false,
removeLast([a], L1, Last) should return L1 = [], Last = a, 
removeLast([[a,b,c]], L1, Last) should return L1 = [], Last = [a,b,c].

--------------------------------------------------------- */


removeLast([A],[],A).
removeLast([LF|L],[LF|L1],Last) :- removeLast(L,L1,Last).

/*
---------------------------------------------------------

Question 5.1

allConnected(+L)

Given a list L, the function tests if each in L is connected 
to each other node in L. A node A is connected to another 
node B if either edge(A,B) or edge(B,A) is true.

allConnected(L) is true for an empty list, L= []


--------------------------------------------------------- */

allConnected([A|L]) :-
      connect(A,L),
      allConnected(L).
allConnected([]).

/*
---------------------------------------------------------

connect(+N, +L)

connect takes in a node N, and the list L, and checks 
to see if there is an edge between the two nodes. 
--------------------------------------------------------- */

connect(A, [LF|LR]) :-
      edge(LF,A),
      connect(A, LR).
connect(A, [LF|LR]) :-
      edge(A,LF),
      connect(A,LR).
connect(_,[]).



/*
---------------------------------------------------------

Question 5.2

maxclique(+N, +Cliques)

Given a list of Cliques and a size N, maxclique returns
any maximal cliques of size N. A clique is maximal if 
there is no larger clique that contains it. 

maxclique(2,Cliques) returns Cliques = [[a,d],[a,e]] 
maxclique(3,Cliques) returns Cliques = [[a,b,c]] 
maxclique(1,Cliques) returns Cliques = [] 
maxclique(0,Cliques) returns Cliques = []

--------------------------------------------------------- */
maxclique(N, Cliques) :- 
         findall(X, clique(X), L),
         getSize(L, N, NList),
         M is N + 1,
         getSize(L, M, MList),
         findMaxCliques(NList, MList, Cliques).

/*
---------------------------------------------------------

getSize(+L, +N, +Cliques)

Given a list of cliques L and size N, getSize returns 
a list of Cliques all with the size N. 

eg. 

getSize([[1,2],[1]], 2, Cliques) returns Cliques = [[1,2]]


--------------------------------------------------------- */

getSize(L, N, Cliques) :- getSize(L, N, [], Cliques).
getSize([LF|LR], N, C, A) :- 
       length(LF, X),
       X == N, 
       append(C, [LF], B),
       getSize(LR, N, B, A).
getSize([LF|LR], N, C, A) :-
       length(LF, X),
       X \== N,
       getSize(LR, N, C, A).
getSize([], _, A, A).


/*
---------------------------------------------------------

findMaxCliques(+NL,+ ML, ?C)

Given a list of N-sized cliques and N+1 or M sizes cliques
findMaxCliques checks if the N sized cliques are a 
subset of the larger sized cliques. 

--------------------------------------------------------- */

findMaxCliques(NL, ML, C) :- findMaxCliques(NL, ML, [], C).
findMaxCliques([LF|LR], ML, L, C) :-
       length(ML, X),
       checkNotSubset(LF, ML, 0, X),
       append(L, [LF], A),
       findMaxCliques(LR, ML, A, C). 
findMaxCliques([LF|LR], ML, L, C) :-
       checkSubset(LF, ML),
       findMaxCliques(LR, ML, L, C). 
findMaxCliques([], _, C, C).

/*
---------------------------------------------------------

checkNotSubset(+C, +ML,-A, -B)

Given a N-sized clique C, and ML, a list of cliques one 
size larger than N, the function returns C if C is a maximal 
clique. The variables A and B are used to count the number 
of elements in L, where B is the length of ML. So when A is
equal to B, all the subsets in ML have been checked. 

test cases:

checkNotSubset([a,b], [[a,b,c]], 0, 1) returns false
checkNotSubset([a,b], [[a,c, d, [a, d, e]], 0, 2) returns true
--------------------------------------------------------- */

checkNotSubset(C, [LF|LR], A, B) :-
         subset(C, LF),
         checkNotSubset(C, LR, A, B).
checkNotSubset(C, [LF|LR], A, B) :-
         notSubset(C, LF),
         X is A+1,
         checkNotSubset(C, LR, X, B).
checkNotSubset(_, [], B, B).


/*
---------------------------------------------------------

checkSubset(+C, +L)

Given a N-sized clique C, and a list of M sized cliques , 
returns true if C is a subset of any M-sized cliques. 

test cases:

checkSubset([a, b], [[a, b, c]]) returns true. 
checkSubset([a,b], [[a, c, d]]) returns false.
--------------------------------------------------------- */
checkSubset(C, [LF|LR]) :-
     subset(C, LF),
     checkSubset(C, [], 1).
checkSubset(C, [LF|LR]) :-
    notSubset(C, LF),
    checkSubset(C, LR).
checkSubset(_, [], 1).

/*
---------------------------------------------------------

notSubset(+C, +L)

Given a N-sized clique C, and a list of M sized cliques , 
returns false if C is a subset of any M-sized cliques. 

test cases:

notSubset([a, b], [[a, b, c]]) returns false 
notSubset([a,b], [[a, c, d]]) returns true.
--------------------------------------------------------- */
notSubset([LF|_], L) :-
	notMember(LF, L),
	notSubset([], _, 1).
notSubset([LF|LR], L) :-
	member(LF, L),
	notSubset(LR, L).
notSubset([], _, 1).
        
/* --------------------------------------------------------------

clique(+L)

Finds all cliques from nodes and edges in the graph. 
-------------------------------------------------------------- */
    
       

clique(L) :- findall(X,node(X),Nodes),
             xsubset(L,Nodes), allConnected(L).

/* --------------------------------------------------------------

xsubset(?X, +Set)

Subset function that finds all subsets within the set
given. 

-------------------------------------------------------------- */

xsubset([], _).
xsubset([X|Xs], Set) :-
  append(_, [X|Set1], Set),
  xsubset(Xs, Set1).
