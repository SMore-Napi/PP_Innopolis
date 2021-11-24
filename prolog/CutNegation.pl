% Lab 12
% Roman Soldatov B19-SD-01
% r.soldatov@innopolis.university

% Exercise 12.1
nonunifiable1(X, Y) :- \+ (X = Y).

neg(Goal) :- Goal, !, fail.
neg(_Goal).
nonunifiable2(X, Y) :- neg(X = Y).

nonunifiable3(X, X) :- !, fail.
nonunifiable3(_X, _Y).

% ?- nonunifiable(test, test).

% Exercise 12.2
partition1(_, [], [], []).
partition1(P, [X|H], [X|L], G) :- X < P, partition1(P, H, L, G).
partition1(P, [X|H], L, [X|G]) :- X >= P, partition1(P, H, L, G).

partition2(_, [], [], []).
partition2(P, [X|H], [X|L], G) :- X < P, partition2(P, H, L, G), !.
partition2(P, [X|H], L, [X|G]) :- X >= P, partition2(P, H, L, G).

partition3(_, [], [], []).
partition3(P, [X|H], [X|L], G) :- X < P, !, partition3(P, H, L, G).
partition3(P, [X|H], L, [X|G]) :- partition3(P, H, L, G).

% ?- partition(3, [1, 5, 2, 6, 3, 0], L, G).

% Exercise 12.3
parent(mike, jack).
parent(mike, bob).

sibling(X, Y) :- parent(P, X), parent(P, Y), neg(X = Y).

% ?- sibling(jack, jack).

% Exercise 12.4
connected(1,2).
connected(2,4).
connected(2,5).
connected(5,3).
connected(5,6).
connected(6,3).
connected(4,3).

path(X, Y, [X, Y]) :- connected(X, Y), !.
path(X, Y, [X|R]) :- connected(X, Z), path(Z, Y, R), !.

% ?- path(1, 3, Path).

% Exercise 12.5
filterUnifiable([], _, []).
filterUnifiable([X|H], T, Y) :- \+ (X = T), filterUnifiable(H, T, Y), !.
filterUnifiable([X|H], T, [X|Y]) :- filterUnifiable(H, T, Y).

% ?- filterUnifiable([X, b, t(Y)], t(a), List).

% Exercise 12.6
attacks((X, _Y1), (X, _Y2)).
attacks((_X1, Y), (_X2, Y)).
attacks((X1, Y1), (X2, Y2)) :- D1 is abs(X1-X2), D2 is abs(Y1-Y2), D1 = D2.

attacksAny(X, [H|_]) :- attacks(X, H).
attacksAny(X, [H|T]) :- \+ attacks(X, H), attacksAny(X, T).

noAttacks([]).
noAttacks([H|T]) :- \+ attacksAny(H, T), noAttacks(T).

addQueenAt(Q, Queens) :- 
    Positions = [1, 2, 3, 4, 5, 6, 7, 8],
    Q = (X, Y),
    member(X, Positions),
    member(Y, Positions),
    noAttacks([Q|Queens]).
    
eightQueens(Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8) :- 
    Q1 = (1, _),
    Q2 = (2, _),
    Q3 = (3, _),
    Q4 = (4, _),
    Q5 = (5, _),
    Q6 = (6, _),
    Q7 = (7, _),
    Q8 = (8, _),
    
    addQueenAt(Q1, []),
    addQueenAt(Q2, [Q1]),
    addQueenAt(Q3, [Q1, Q2]),
    addQueenAt(Q4, [Q1, Q2, Q3]),
    addQueenAt(Q5, [Q1, Q2, Q3, Q4]),
    addQueenAt(Q6, [Q1, Q2, Q3, Q4, Q5]),
    addQueenAt(Q7, [Q1, Q2, Q3, Q4, Q5, Q6]),
    addQueenAt(Q8, [Q1, Q2, Q3, Q4, Q5, Q6, Q7]).
    