:-include(twitbook).

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

test_outcast :-
    g1(G), g2(H),
    outcast(G, bruce),
    \+ outcast(G, kara),
    outcast(H, batman),
    \+ outcast(H, supergirl),
    write('Outcast tests'), nl.

test_admires :-
    g1(G), g2(H),
    admires(G, bruce, kara),
    admires(G, bruce, barry),
    admires(G, bruce, clark),
    \+ admires(G, barry, bruce),
    \+ admires(G, kara, bruce),
    admires(H, batman, supergirl),
    admires(H, batman, flash),
    admires(H, batman, superman),
    \+ admires(H, flash, batman),
    \+ admires(H, supergirl, batman),
    write('Admires tests'), nl.

test_indifferent :-
    g1(G), g2(H),
    indifferent(G, kara, bruce),
    indifferent(G, clark, bruce),
    \+ indifferent(G, bruce, kara),
    indifferent(H, green_arrow, batman),
    indifferent(H, flash, batman),
    \+ indifferent(H, batman, superman),
    write('Indifferent tests'), nl.

test_same_world1 :-
    g1(G), g2(H),
    same_world(G,H,A),
    member((kara,supergirl), A),
    write('Same world tests 1'), nl.

test_same_world2 :-
    g1(G), g2(H),
    same_world(H,G,A),
    member((batman, bruce), A),
    write('Same world tests 2'), nl.

test_same_world3 :-
    g1(G),
    same_world(G,G,A),
    member((clark, clark), A),
    write('Same world tests 3'), nl.

test_all :-
    write('Start with tests'), nl,
    test_likes,
    test_dislikes,
    test_popular,
    test_outcast,
    test_admires,
    test_indifferent,
    test_same_world1,
    test_same_world2,
    test_same_world3,
    write('All tests have been run'), nl.
