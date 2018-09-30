/*
    Assignment 3

    Authors: Kai Arne S. Myklebust, Silvan Adrian

*/

g1([person(kara, [barry, clark]), 
 person(bruce, [clark, oliver]),
 person(barry, [kara, oliver]),
 person(clark, [oliver, kara]),
 person(oliver, [kara])]).


likes(G,X,Y) :- 
	isMember(person(X, Friends), G), 
	isMember(Y, Friends).

/* Checks if an elem is member of list */
isMember(X, [X|_]).
isMember(X, [_|T]) :- isMember(X, T).