:- module(PR1, [author_data/4, color/1, rule/7, evolve/3, cells/3]).
author_data('Cabo', 'Ciudad', 'Alvaro', '200172').

% Métodos auxiliares dados %

color(o).
color(x).

rule(o,o,o,_,o). % regla nula
rule(x,o,o,r(A,_,_,_,_,_,_),A) :- color(A).
rule(o,x,o,r(_,B,_,_,_,_,_),B) :- color(B).
rule(o,o,x,r(_,_,C,_,_,_,_),C) :- color(C).
rule(x,o,x,r(_,_,_,D,_,_,_),D) :- color(D).
rule(x,x,o,r(_,_,_,_,E,_,_),E) :- color(E).
rule(o,x,x,r(_,_,_,_,_,F,_),F) :- color(F).
rule(x,x,x,r(_,_,_,_,_,_,G),G) :- color(G).

% Ejercicio 1: ¿Es una célula válida?

cells(InitialState, Rules, FinalState) :-
    % Verificar que el estado inicial y el estado resultante cumplen las especificaciones
    (InitialState = [o|_], last(InitialState, o),
     evolve(InitialState, Rules, Result), Result = [o|_], last(Result, o)) ->
        FinalState = Result ; % Si se cumplen las especificaciones, devolver el estado resultante
        fail. % Si no se cumplen las especificaciones, fallar

cells(InitialState, Rules, FinalState) :-
    % Si hay tres células consecutivas que no pueden evolucionar, fallar
    \+ (append([_,_,_], Rest, InitialState), append(NotEvolved, [_,_,_], Rest),
        evolve([o|NotEvolved], Rules, [o,o|_])),
    % Verificar que el estado inicial y el estado resultante cumplen las especificaciones
    (InitialState = [o|_], last(InitialState, o),
     evolve(InitialState, Rules, Result), Result = [o|_], last(Result, o)) ->
        FinalState = Result ; % Si se cumplen las especificaciones, devolver el estado resultante
        fail. % Si no se cumplen las especificaciones, fallar


% Ejercicio 2: Evolución de la cinta

evolve([_,_], _, [ ]).
evolve([X,Y,Z|Rest], Rules, [NewY|NewRest]) :-
    rule(X,Y,Z,Rules,NewY),
    evolve([Y,Z|Rest], Rules, NewRest).



