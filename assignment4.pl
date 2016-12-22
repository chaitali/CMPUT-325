%CMPUT 325 B1
%Assignment 4


:- use_module(library(clpfd)).

/*
---------------------------------------------------------

Question 1

fourSquares(+N, [-S1, -S2, -S3, -S4])

Given any positive integer N greater than 0, it returns a 
list of integers [S1,S2,S3,S4] such that

N = S1*S1 + S2*S2 + S3*S3 + S4*S4.

test cases: 

fourSquares(20, Var). should return
Var = [0, 0, 2, 4] ;
Var = [1, 1, 3, 3] ;
false.

--------------------------------------------------------- */

fourSquares(N, [S1, S2, S3, S4]) :-
    Vars = [S1, S2, S3, S4],
    Vars ins 0..N,
    S1 #=< S2,
    S2 #=< S3,
    S3 #=< S4,
    N #= S1*S1 + S2*S2 + S3*S3 + S4*S4,
    label(Vars).
    

/*
---------------------------------------------------------

Question 2

disarm(+Adivisions, +Bdivisions,-Solution)

Each month one of the countries can choose to dismantle 
one military division while the other can dismantle two. 
Each division has a certain strength, and both sides want 
to make sure that the total military strength remains 
equal at each point during the disarmament process. 

For example, suppose the strenghs of the country's divisions are:

Country A: 1, 3, 3, 4, 6, 10, 12
Country B: 3, 4, 7, 9, 16

One solution is:

Month 1: A dismantles 1 and 3, B dismantles 4
Month 2: A dismantles 3 and 4, B dismantles 7
Month 3: A dismantles 12, B dismantles 3 and 9
Month 4: A dismantles 6 and 10, B dismantles 16

test cases: 

disarm([1,3,3,4,6,10,12],[3,4,7,9,16],S).
S = [[[1, 3], [4]], [[3, 6], [9]], [[10], [3, 7]], [[4, 12], [16]]].
-------------------------------------------------------- */
 

disarm(Adivisions, Bdivisions, Solutions) :- disarm(Adivisions, Bdivisions, Solutions, 0).

disarm(Adivisions, Bdivisions, Solutions, X) :-
 	Vars = [A,B,C],
 	select(A, Adivisions, A1),
 	select(B, A1, B1),
 	select(C, Bdivisions, C1),
 	A #=< B,
 	A+B #= C,
 	X #=< C,
 	disarm(B1, C1, S1, C),
 	append([[[A,B],[C]]], S1, Solutions),
 	label(Vars).	

disarm(Adivisions, Bdivisions, Solutions, X) :-
 	Vars = [A,B,C],
 	select(A, Adivisions, A1),
 	select(B, Bdivisions, B1),
 	select(C, B1, C1),
 	B #=< C,
 	B+C #= A,
 	X #=< A,
 	disarm(A1, C1, S1, A),
 	append([[[A],[B,C]]], S1, Solutions),
 	label(Vars).	

disarm([],[],S,_) :- S=[].   





