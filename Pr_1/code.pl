:- module(PR1, [author_data/4, color/1, rule/5, cells/3, evol/3], [assertions]).
:- use_module(library(unittest)).

author_data('Cabo', 'Ciudad', 'Alvaro', '200172').

:- doc(title, "Practica 1 - Programacion logica pura").

:- doc(author, "ALVARO CABO CIUDAD").
:- doc(module, "Modelado de un autómata celular 1D en Ciao-Prolog

@section{Predicados auxiliares dados}
@subsection{Color}
Establece los valores validos del color de la célula con color/1:

@subsection{Reglas}
Establece el conjunto de reglas posibles para crear el automata:
@begin{verbatim}
Example usage:
  ?- R = r(x,o,x,x,x,x,o), rule(o,x,o,R,Y).
  R = r(x,o,x,x,x,x,o),
  Y = o.
@end{verbatim}

@section{Tests}
Se incluyen aserciones que empiezan por @tt{:- test} al final del documento.

@subsection{basic_building(X) tests}
@begin{verbatim}
:- test basic_building(X) : (X = [[1,1],[s(0)]] ) 
@end{verbatim}
@begin{verbatim}
:- test basic_building(X) : (X = [[s(0),1],[s(0)]] )
@end{verbatim}
@begin{verbatim}
:- test basic_building(X) : (X = [[],[s(0)]])
@end{verbatim}
@begin{verbatim}
:- test basic_building(X) : (X = [])
@end{verbatim}
").

:- pred color(X)
   #"Binary representation where @var{X} is either x or o. @includedef{color/1}".
color(o).
color(x).

:- pred rule(+Cell1, +Cell2, +Cell3, +Rules, -ResultCell)
    #" This predicate is used to consult a specific rule given by the @var{Rules} list
    and the pattern of @var{Cell1}, @var{Cell2}, and @var{Cell3} cells. It returns the
    @var{ResultCell} that corresponds to the pattern of cells based on the rules in
    the @var{Rules} list. @includedef{rule/5}".
rule(o,o,o,_,o). % regla nula
rule(x,o,o,r(A,_,_,_,_,_,_),A) :- color(A).
rule(o,x,o,r(_,B,_,_,_,_,_),B) :- color(B).
rule(o,o,x,r(_,_,C,_,_,_,_),C) :- color(C).
rule(x,o,x,r(_,_,_,D,_,_,_),D) :- color(D).
rule(x,x,o,r(_,_,_,_,E,_,_),E) :- color(E).
rule(o,x,x,r(_,_,_,_,_,F,_),F) :- color(F).
rule(x,x,x,r(_,_,_,_,_,_,G),G) :- color(G).

% Ejercicio 1: Evolucionador de células
:- pred cells(InitialState::in, Rules::in, FinalState::out)
#"Verifies whether @var{InitialState} is a valid list of cells that can be evolved according to the given @var{Rules}. 
If so, the predicate binds @var{FinalState} to the resulting evolved state.".

cells([o, X|Rest], Rules, [o,S|FinalState]) :-
    rule(o,o,X, Rules, S), 
    evolve([o,X|Rest], Rules, FinalState).

% Predicado auxiliar para evolucionar células
evolve([X,o], Rules, [S,o]):- % Caso base [fin de lista]
    rule(X,o,o,Rules,S).

evolve([X,Y,Z|Rest], Rules, [S|NewRest]) :-
    rule(X,Y,Z,Rules,S),
    evolve([Y,Z|Rest], Rules, NewRest).

% Ejercicio 2: N-Evolución de la cinta
:- pred evol(N, Rules, Evolution)
#"Aplies @var{N} steps of the evolution starting at @tt{[o,x,o]}".

evol(0,_,[o,x,o]). %Caso base
evol(s(N), Rules, Cells):-
    N \= s(0),
    cells(Evolution, Rules, Cells),
    evol(N, Rules, Evolution).


% ######################## TESTING ######################## %
