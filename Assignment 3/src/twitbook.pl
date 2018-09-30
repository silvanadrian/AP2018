/*
    Assignment 3

    Authors: Kai Arne S. Myklebust, Silvan Adrian

*/

g1([person(kara, [barry, clark]), 
 person(bruce, [clark, oliver]),
 person(barry, [kara, oliver]),
 person(clark, [oliver, kara]),
 person(oliver, [kara])]).

/* */
likes(G,X,Y) :- 
    isMember(person(X, Friends), G),
    isMember(Y, Friends).

/* Checks if an elem is member of list */
isMember(Head,[Head|_]).
isMember(Head,[_|Tail]) :- isMember(Head,Tail).

/* b - using not which may not be allowed */
dislikes(G, X, Y) :-
    different(G, X, Y),
    likes(G, Y, X),
    notLikes(G, X, Y).

/* different succeeds whenever X and Y are different members of the network G */
different(G, X, Y) :-
    selectList(person(X, _), G, G2),
    selectList(person(Y, _), G2, _).

selectList(X, [X|Tail], Tail).
selectList(Elem, [Head|Tail], [Head|Rest]) :-
    select(Elem, Tail, Rest).


notLikes(G, X, Y) :- getFriends(G, X, Xfriends),
        isNotFriend(G, Y, Xfriends).

isNotFriend(_, _, []).
    isNotFriend(G, X, [Friend|RestFriends]) :- different(G, X, Friend),
        isNotFriend(G, X, RestFriends).               

getFriends([person(X, Friends)|_], X, Friends).
getFriends([_|T], X, Friends) :- getFriends(T, X, Friends).            
