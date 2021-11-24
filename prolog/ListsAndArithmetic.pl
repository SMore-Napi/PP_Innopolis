% Lab 11
% Roman Soldatov B19-SD-01
% r.soldatov@innopolis.university

% Exercise 11.1
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).
% member(3, [X, 3, Y]).
% (X = _G1, Y = _G2) member(3, [_G1, 3, _G2]). ->
% 	| -> member(3, [_G1|_]). -> (_G1 = 3) -> (X = 3)
% 	| -> member(3, [_|[3, _G2]]) -> member(3, [3, _G2]). ->
% 		|-> member(3, [3|_]). -> true.
%		|-> member(3, [_|_G2]) -> member(3, _G2) -> (_G2 = 3) -> (Y = 3)

% Exercise 11.2
prefix([], _).
prefix([X|Xs], [X|Ys]) :- prefix(Xs, Ys).

% ?- prefix([1, 2], [1, 2, 3, 4]).
% ?- prefix(X, [1, 2, 3]).

% Exercise 11.3
interleave([], Y, Y).
interleave([X|Xs], [Y|Ys], [X, Y | R]) :- interleave(Xs, Ys, R).

% ?- interleave([1, 2], [a, b], X).
% ?- interleave([1, 2], [a, b, c], X).
% ?- interleave(X, Y, [1, 2, 3, 4]).

% Exercise 11.4
squares([], []).
squares([X|Xs], [Y|R]) :- Y is X*X, squares(Xs, R).

% ?- squares([1, 2, 3, 4], X).

% Exercise 11.5
select([H|R], H, R).
select([H|T], E, [H|R]) :- select(T, E, R).

% ?- select([6, 3, 8, 5], H, R).

% Exercise 11.6
anagram([], []).
anagram([H|T], List2) :- length([H|T], L), length(List2, L),
   select(List2, H, RestList2), anagram(T, RestList2).

% ?- anagram([d,o,r,m,i,t,o,r,y], [d,i,r,t,y, r,o,o,m]).
% ?- anagram([1, 2], X).

% Exercise 11.7
sublist([], _ ).
sublist([X|T], [X|R]) :- sublist(T, R).
sublist([X|T], [_|R]) :- sublist([X|T], R).

zebra(N) :-
    % Representation for the houses and the street
    Street = [_H1,_H2,_H3],
    member(house(red, _, _), Street),
    member(house(blue, _, _), Street),
    member(house(green, _, _), Street),
    
    % 4 facts about people

    % The Englishman lives in the red house.
	member(house(red, english, _), Street),
    % The jaguar is the pet of the Spanish family.
    member(house(_, spanish, jaguar), Street),
    % The Japanese lives to the right of the snail keeper.
    sublist([house(_, _, snail), house(_, japanese, _)], Street),
    % The snail keeper lives to the left of the blue house.
    sublist([house(_, _, snail), house(blue, _, _)], Street),
    
    % Solution itself
    member(house(_, N, zebra), Street).

% zebra(X).

% Exercise 11.8
sorted([]).
sorted([X, Y]) :- X =< Y.
sorted([X, Y|T]) :- X =< Y, sorted([Y|T]).

% ?- sorted([6, 3, 8, 5]).
% ?- sorted([3, 5, 6, 8]).

% Exercise 11.9
sort_([], []).
sort_(X, R) :- anagram(X, R), sorted(R).

% ?- sort_([6, 3, 8, 5], X).

% Exercise 11.10
% shuffled/2 is not working in one direction 
% because there is no any point to stop.
% So, it inifinitly will be trying to find soultions 
% increasing the unknown list's length.
sameLength([], []).
sameLength([_|X], [_|Y])
	:- sameLength(X, Y).

shuffled([], []).
shuffled(X, [H|Y]) :-
    sameLength(X, [H|Y]),
    select(X, H, T),
    shuffled(T, Y).
