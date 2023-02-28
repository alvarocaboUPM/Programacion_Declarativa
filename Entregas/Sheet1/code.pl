:- module(E1, [author_data/4, elimina/2, hermano/2, padre/2, hijo/2, abuela/2, tio/2, cunyado/2, madre/2]).
author_data('Cabo', 'Ciudad', 'Alvaro', '200172').

% EJERCICIO 1 %

% Hechos
controla(corleone, manhattan).
controla(corleone, brooklyn).
controla(solozzo, drogas).
controla(corleone, apuestas).

% Reglas
elimina(corleone, solozzo) :- controla(corleone, manhattan), controla(corleone, brooklyn).
elimina(solozzo, corleone) :- controla(solozzo, drogas), apoya(roth, solozzo).
elimina(corleone, solozzo) :- apoya(roth, corleone), controla(corleone, manhattan), controla(corleone, brooklyn).
elimina(solozzo, _) :- apoya(roth, solozzo), controla(solozzo, bronx), controla(solozzo, harlem).
elimina(_, roth) :- apoya(roth, _).
controla_policia(X) :- controla(X, apuestas).
apoya(X, Y) :- garantiza_impunidad(X), controla_policia(Y).
garantiza_impunidad(_). % Se asume que todos pueden garantizar la impunidad.

% Ejemplo de consulta:
% ¿Quién va a eliminar a quién?
% ?- elimina(X, Y).

% EJERCICIO 2 %

% Hechos
%padres e hijos
padre(homer, pugsley).
padre(homer, wednesday).
madre(morticia, pugsley).
madre(morticia, wednesday).
% abuela
madre(abuela_addams, tio_fester).
madre(abuela_addams, tio_cosa).
madre(abuela_addams, homer).
%extra
casado(homer, morticia).

% Reglas
casado(X, Y) :- casado(Y, X).
hermano(X, Y) :- padre(Z, X), padre(Z, Y), X \= Y; madre(Z, X), madre(Z, Y), X \= Y.
hijo(X, Y) :- padre(Y, X); madre(Y, X).
abuela(X, Y) :- madre(Z, Y), madre(X, Z); padre(Z, Y), madre(X, Z).
tio(X, Y) :- hermano(X, Z), padre(Z, Y); hermano(X, Z), madre(Z, Y).
sobrino(X, Y) :- tio(Y, X).
cunyado(X, Y) :- casado(Y, Z), hermano(X, Z). 