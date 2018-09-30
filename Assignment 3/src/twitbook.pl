/*
    Assignment 3

    Authors: Kai Arne S. Myklebust, Silvan Adrian

*/

g1([person(kara, [barry, clark]), 
 person(bruce, [clark, oliver]),
 person(barry, [kara, oliver]),
 person(clark, [oliver, kara]),
 person(oliver, [kara])]).

% a
likes(G,X,Y) :- 
    isMember(person(X, Friends), G),
    isMember(Y, Friends).

/* Checks if an elem is member of list */
isMember(Head,[Head|_]).
isMember(Head,[_|Tail]) :- isMember(Head,Tail).

% b - using not which may not be allowed
% different also not in use
dislikes(G, X, Y) :-
    % different(G, X, Y),
    likes(G, Y, X),
    not(likes(G, X, Y)).
    
% different(G, X, Y) :-