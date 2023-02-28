:- module(E1, [elimina/2]).
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