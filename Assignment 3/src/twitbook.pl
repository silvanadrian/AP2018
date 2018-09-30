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
    not(likes(G, X, Y)).

/* different succeeds whenever X and Y are different members of the network G */
different(G, X, Y) :-
    select(person(X, _), G, G2),
    select(person(Y, _), G2, _).

select(X, [X|Tail], Tail).
select(Elem, [Head|Tail], [Head|Rest]) :-
    select(Elem, Tail, Rest).


notLikes(G, X, Y) :- 
            getFriendList(G, X, Xfriends),
	        isNotElemInFriendList(G, Y, Xfriends).


isNotElemInFriendList(_, _, []).
isNotElemInFriendList(G, X, [Friend|RestFriends]) :- 
            different(G, X, Friend),
            isNotElemInFriendList(G, X, RestFriends).               

getFriendList([person(X, FriendList)|_], X, FriendList).
getFriendList([_|T], X, FriendList) :- getFriendList(T, X, FriendList).            

