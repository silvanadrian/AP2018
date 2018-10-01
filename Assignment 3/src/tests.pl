:-include(twitbook).
% Tests
:- begin_tests(twitbook).

% Imported to run tests
:- use_module(library(plunit)).

% Graphs for testing
g1([person(kara, [barry, clark]),
    person(bruce, [clark, oliver]),
    person(barry, [kara, oliver]),
    person(clark, [oliver, kara]),
    person(oliver, [kara])]).

g2([person(batman, [green_arrow, superman]),
    person(green_arrow, [supergirl]),
    person(supergirl, [flash, superman]),
    person(flash, [green_arrow, supergirl]),
    person(superman, [green_arrow, supergirl])]).

test_likes :-
    g1(G), g2(H),
    likes(G, kara, clark),
    likes(G, barry, kara),
    \+ likes(G, clark, bruce),
    likes(H, batman, green_arrow),
    \+ likes(H, supergirl, batman),
    write('Likes tests'), nl.

test_dislikes :-
    g1(G), g2(H),
    dislikes(G, kara, oliver),
    dislikes(G, oliver, barry),
    \+ dislikes(G, clark, kara),
    dislikes(H, green_arrow, superman),
    \+ dislikes(H, supergirl, superman),
    write('Dislikes tests'), nl.

test_popular :-
    g1(G), g2(H),
    popular(G, kara),
    \+ popular(G, clark),
    \+ popular(G, bruce),
    popular(H, supergirl),
    \+ popular(H, batman),
    \+ popular(H, superman),
    write('Popular tests'), nl.


test_all :-
    write('Start with tests'), nl,
    test_likes,
    test_dislikes,
    test_popular,
    write('All tests have been run'), nl.

:- end_tests(twitbook).