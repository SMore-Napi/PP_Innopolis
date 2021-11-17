% Lab 10
% Roman Soldatov B19-SD-01
% r.soldatov@innopolis.university

% Exercise 10.1.
house_elf(dobby).
witch(hermione).
witch('McGonagall').
witch(rita_skeeter).

magic(X) :- house_elf(X).
% magic(X) :- wizard(X).
magic(X) :- witch(X).

% magic(X) -> X = _G1 -> magic(_G1) | -> house_elf(_G1) -> (_G1 = dobby) house_elf(dobby) -> X = dobby
%                                   | -> wizard(_G1). -> error
%                                   | -> witch(_G1). | -> (_G1 = hermione) witch(hermione) -> X = hermione
%                                                    | -> (_G1 = 'McGonagall') witch('McGonagall') -> X = 'McGonagall' 
%                                                    | -> (_G1 = rita_skeeter) witch(rita_skeeter) -> X = rita_skeeter


% Exercise 10.2.

word(astante,  a,s,t,a,n,t,e).
word(astoria,  a,s,t,o,r,i,a).
word(baratto,  b,a,r,a,t,t,o).
word(cobalto,  c,o,b,a,l,t,o).
word(pistola,  p,i,s,t,o,l,a).
word(statale,  s,t,a,t,a,l,e).

crossword(V1, V2, V3, H1, H2, H3) :-
 word(V1, _, L1, _, L4, _, L7, _),
 word(V2, _, L2, _, L5, _, L8, _),
 word(V3, _, L3, _, L6, _, L9, _),
 word(H1, _, L1, _, L2, _, L3, _),
 word(H2, _, L4, _, L5, _, L6, _),
 word(H3, _, L7, _, L8, _, L9, _),
 V1 \== H1, V2 \== H2, V3 \== H3.

% Exercise 10.3.
directTrain(saarbruecken,dudweiler).
directTrain(forbach,saarbruecken).
directTrain(freyming,forbach).
directTrain(stAvold,freyming).
directTrain(fahlquemont,stAvold).
directTrain(metz,fahlquemont).
directTrain(nancy,metz).
 
travelFromTo(X, Y) :- directTrain(X, Y).
travelFromTo(X, Y) :- directTrain(X, Z), travelFromTo(Z, Y).

% Exercise 10.4.
connected(1,2).
connected(3,4).
connected(6,3).
connected(5,6).
connected(7,8).
connected(9,10).
connected(12,13).
connected(13,14).
connected(15,16).
connected(17,18).
connected(19,20).
connected(4,1).
connected(4,7).
connected(6,11).
connected(14,9).
connected(11,15).
connected(16,12).
connected(14,17).
connected(16,19).

% connectedBiDirectonal(X, Y) :- connected(X, Y); connected(Y, X).

path(X, Y) :- connected(X, Y).
path(X, Y) :- connected(X, Z), path(Z, Y).

% Exercise 10.5.
byCar(auckland,hamilton).
byCar(hamilton,raglan).
byCar(valmont,saarbruecken).
byCar(valmont,metz).

byTrain(metz,frankfurt).
byTrain(saarbruecken,frankfurt).
byTrain(metz,paris).
byTrain(saarbruecken,paris).

byPlane(frankfurt,bangkok).
byPlane(frankfurt,singapore).
byPlane(paris,losAngeles).
byPlane(bangkok,auckland).
byPlane(singapore,auckland).
byPlane(losAngeles,auckland).

travel(X, Y) :- byCar(X, Y); byTrain(X, Y); byPlane(X, Y).
travel(X, Y) :- byCar(X, Z), travel(Z, Y).
travel(X, Y) :- byTrain(X, Z), travel(Z, Y).
travel(X, Y) :- byPlane(X, Z), travel(Z, Y).
 
% Exercise 10.6.
travel(X, Y, goByCar(X, Y)) :- byCar(X, Y).
travel(X, Y, goByTrain(X, Y)) :- byTrain(X, Y).
travel(X, Y, goByPlane(X, Y)) :- byPlane(X, Y).

travel(X, Y, goByCar(X, Z, R)) :- byCar(X, Z), travel(Z, Y, R).
travel(X, Y, goByTrain(X, Z, R)) :- byTrain(X, Z), travel(Z, Y, R).
travel(X, Y, goByPlane(X, Z, R)) :- byPlane(X, Z), travel(Z, Y, R).
