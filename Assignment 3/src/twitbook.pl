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
isNotFriend(G, X, [Friend|RestFriends]) :- 
    different(G, X, Friend),
    isNotFriend(G, X, RestFriends).               

getFriends([person(X, Friends)|_], X, Friends).
getFriends([_|T], X, Friends) :- getFriends(T, X, Friends).            


% Level 1 - Task c
popular(G, X) :-
    isMember(person(X, Friends), G),
    allLikingX(G, X, Friends).

allLikingX(_, _, []).
allLikingX(G, X, [Head | Tail]) :-
    likes(G, Head, X),
    allLikingX(G, X, Tail).

% Task d
outcast(G, X) :-
    isMember(person(X, Friends), G),
    allDislikeX(G, X, Friends).

allDislikeX(_, _, []).
allDislikeX(G, X, [Head | Tail]) :-
    dislikes(G, Head, X),
    allDislikeX(G, X, Tail).

% Task e
friendly(G, X) :-
    isMember(person(X, _), G),
    checkFriendliness(G, G, X).

checkFriendliness([], _, _).
checkFriendliness([person(Name, _)|Tail], G, Name) :-
    checkFriendliness(Tail, G, Name).
checkFriendliness([person(Name, _)|Tail], G, X) :-
    likes(G, Name, X),
    likes(G, X, Name),
    checkFriendliness(Tail, G, X).
checkFriendliness([person(_, Friends)|Tail], G, X) :-
    isNotFriend(G, X, Friends),
    checkFriendliness(Tail, G, X).

% Task e
hostile(G, X) :-
    isMember(person(X, _), G),
    checkHostileness(G, G, X).

checkHostileness([], _, _).
checkHostileness([person(Name, _)|Tail], G, Name) :-
    checkHostileness(Tail, G, Name).
checkHostileness([person(Name, _)|Tail], G, X) :-
    likes(G, Name, X),
    dislikes(G, X, Name),
    checkHostileness(Tail, G, X).
checkHostileness([person(_, Friends)|Tail], G, X) :-
    isNotFriend(G, X, Friends),
    checkHostileness(Tail, G, X).