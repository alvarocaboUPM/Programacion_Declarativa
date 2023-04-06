:- module(E2, [author_data/4, natural/1, suma/3, par/1, impar/1, pares_lista/2, suma_a_lista/3, extrae_elemento/4], [iso_strict]).
author_data('Cabo', 'Ciudad', 'Alvaro', '200172').

% EJERCICIO 1 %

natural(0).
natural(s(X)):-natural(X).

suma(0, Y, Y).
suma(s(X), Y, Z) :- natural(X), suma(X, s(Y), Z).

par(0).
par(s(s(X))) :- par(X).

impar(X) :-  \+par(X).

% EJERCICIO 2 %
suma_a_lista([], natural(N), []).
suma_a_lista([X|Xs], N, [Y|Ys]) :-
    suma(X, N, Y),
    suma_a_lista(Xs, N, Ys).

% Implementación tipo include
% pares_lista(L, Ps) :- include(par, L, Ps).

pares_lista([],[]).
pares_lista([X|Xs],[X|S]) :- call(par,X),!,pares_lista(Xs,S).
pares_lista([_|Xs],S) :- pares_lista(Xs,S).

% EJERCICIO 3 
% NL -> L - L[I] <-> L - E
%

extrae_elemento(natural(I),[],_,[]).
extrae_elemento(natural(I),[],_,[]).