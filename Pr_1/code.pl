:- module(PR1, [author_data/4, color/1, rule/5, cells/3, evol/3, steps/2, ruleset/2], [assertions]).
:- use_module(library(unittest)).

author_data('Cabo', 'Ciudad', 'Alvaro', '200172').

:- doc(title, "Practica 1 - Programacion logica pura").

:- doc(author, "ALVARO CABO CIUDAD").
:- doc(module, "1D cellular Automata modeling in Ciao Prolog

@section{Aux predicates}
@subsection{Color}
Stablishes valid colors/states for the cells
@includedef{color/1}

@subsection{Rules}
Defines valid rules and returns the result of the iteration
@begin{verbatim}
Example usage:
  ?- R = r(x,o,x,x,x,x,o), rule(o,x,o,R,Y).
  R = r(x,o,x,x,x,x,o),
  Y = o.
@end{verbatim}

@section{Testing}
Included at the end of the document all the @tt{:- test} assertions.\n
@includedef{test/1}
@bf{Note: } Every unit test uses @tt{r(x,x,o,x,o,x,o)} as its ruleset, just as Deliverit

@subsection{Running the tests}
Testing can be run using the ciao console or using the integrated Ciao
debugger on Emacs
@begin{verbatim}
    Example of usage:
    ?-  use_module(library(unittest)).
        yes
    ?- run_tests_in_module('/home/varo/UPM/3ero/ProDec/Pr_1/code.pl').

    PASSED: (lns 123-126) cells/3.
    PASSED: (lns 127-128) cells/3.
    PASSED: (lns 129-129) cells/3.
    PASSED: (lns 130-130) cells/3.
    PASSED: (lns 131-135) evol/3.
    PASSED: (lns 136-136) evol/3.
    FAILED: (lns 137-139) steps/2. 
    (lns 137-139) steps(_1,_) run-time check failure
    Requires in *success*: 
        _1=[_,_,_,_,_].
    But instead:
        _1=[_,o,o,o,o,o]
        _=_2
        _=_1
        _=_3
        _=_4
        _=_5
    FAILED: (lns 140-140) steps/2. 
    (lns 140-140) steps(_1,_) run-time check failure.
    Requires in *success*: 
        _1=[_,_,_,_,_,_,_].
    But instead:
        _1=[_,o,o,o,o,o,o,o]
        _=_2
        _=_1
        _=_3
        _=_4
        _=_5
        _=_6
        _=_7

Note: {Total:
Passed: 6 (75.00'%') Failed: 2 (25.00'%') Precond Failed: 0 (0.00'%') Aborted: 0 (0.00'%') Timeouts: 0 (0.00'%') Total: 8 Run-Time Errors: 2
}

yes
?- 
@end{verbatim}

@subsection{cells/3 tests}
@bf{Basic Test}
@begin{verbatim}
:- test cells(I,R,F) : (I = [o,x,o], R=r(x,x,o,x,o,x,o)) => (C=[o,o,x,x,o])
@end{verbatim}
@bf{Long Test}
@begin{verbatim}
:- test cells(I,R,F) : (I = [o,x,x,x,o,o,o,x,o,o,x,x,o,x,x,x,x,o,x,x,o], 
                        R=r(x,x,o,x,o,x,o)) 
                        => (C=[o,o,x,o,o,x,o,o,x,x,o,x,o,x,x,o,o,o,x,x,o,x,o])
@end{verbatim}
@bf{Reverse-order Test}
@begin{verbatim}
:- test cells(I,R,F) : (C = [o,o,x,x,o,o,o,x,o,o,x,o,x,x,x,x,o], R=r(x,x,o,x,o,x,o))
@end{verbatim}
@bf{Ruleset inference Test}
@begin{verbatim}
:- test cells(I,R,F) : (I = [o,x,o,o,o,o,x,x,x,o,o,x,o,x,o],
    F= [o,o,x,x,o,o,o,x,o,o,x,o,x,x,x,x,o]) => R=r(x,x,o,x,o,x,o) 
@end{verbatim}

@subsection{evol/3 tests}
@bf{Basic Test}
@begin{verbatim}
:- test evol(N,R,C) : (N = 0, R=r(x,x,o,x,o,x,o)) => (C=[o,x,o])
@end{verbatim}
@bf{Advanced Test}
@begin{verbatim}
:- test evol(N,R,C) : (N = s(0), R=r(x,x,o,x,o,x,o))
@end{verbatim}

@subsection{steps/2 tests}
@bf{Basic Test}
@begin{verbatim}
:- test steps(C,N) : (N = s(0))    => (C=[_,_,_,_,_])
@end{verbatim}
@bf{Advanced Test}
@begin{verbatim}
:- test steps(C,N) : (N = s(s(0))) => (C=[_,_,_,_,_,_,_])
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
    the @var{Rules} list.".
rule(o,o,o,_,o). % regla nula
rule(x,o,o,r(A,_,_,_,_,_,_),A) :- color(A).
rule(o,x,o,r(_,B,_,_,_,_,_),B) :- color(B).
rule(o,o,x,r(_,_,C,_,_,_,_),C) :- color(C).
rule(x,o,x,r(_,_,_,D,_,_,_),D) :- color(D).
rule(x,x,o,r(_,_,_,_,E,_,_),E) :- color(E).
rule(o,x,x,r(_,_,_,_,_,F,_),F) :- color(F).
rule(x,x,x,r(_,_,_,_,_,_,G),G) :- color(G).

% Ejercicio 1: Evolucionador de células
:- pred cells(+InitialState, +Rules, -FinalState)
#"Verifies whether @var{InitialState} is a valid list of cells that can be evolved according to the given @var{Rules}. 
If so, the predicate binds @var{FinalState} to the resulting evolved state. @includedef{cells/3}".

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
:- pred evol(+N, +Rules, -Cells)
#"Aplies @var{N} steps of the evolution starting at @tt{[o,x,o]} @includedef{evol/3}".

evol(0,_,[o,x,o]). % Caso base
evol(s(N), Rules, Cells) :-
    evol(N, Rules, Evolution),
    cells(Evolution, Rules, Cells). 

% Ejercicio 3: Descrubir autómatas
:- pred steps(+Cells, -N)
#"Returns the @var{N} steps necessary to get from the intial state @tt{[o,x,o]} to @var{Cells} @includedef{steps/3}".

steps([_,_,_], 0).
steps([_|T], s(N)):-
    evol(s(N),_,T).

:- pred ruleset(RuleSet, Cells)
#"Returns valid @var{Cells} using @var{RuleSet} starting at the intial state @tt{[o,x,o]} to".

ruleset(_, [o,x,o]).
% ######################## TESTING ######################## %

% CELLS
:- test cells(I,R,F) : (I = [o,x,o], R=r(x,x,o,x,o,x,o)) => (C=[o,o,x,x,o]) #"@includedef{test/1}".
:- test cells(I,R,F) : (I = [o,x,x,x,o,o,o,x,o,o,x,x,o,x,x,x,x,o,x,x,o], R=r(x,x,o,x,o,x,o)) 
                            => (C=[o,o,x,o,o,x,o,o,x,x,o,x,o,x,x,o,o,o,x,x,o,x,o]) #"@includedef{test/1}".
:- test cells(I,R,F) : (C = [o,o,x,x,o,o,o,x,o,o,x,o,x,x,x,x,o], R=r(x,x,o,x,o,x,o)) #"@includedef{test/1}".
:- test cells(I,R,F) : (I = [o,x,o,o,o,o,x,x,x,o,o,x,o,x,o],F= [o,o,x,x,o,o,o,x,o,o,x,o,x,x,x,x,o]) => (R=r(x,x,o,x,o,x,o)) #"@includedef{test/1}".


% EVOL

:- test evol(N,R,C) : (N = 0, R=r(x,x,o,x,o,x,o)) => (C=[o,x,o]) #"@includedef{test/1}".
:- test evol(N,R,C) : (N = s(0), R=r(x,x,o,x,o,x,o)) #"@includedef{test/1}".

% STEPS
:- test steps(C,N) : (N = s(0))    => (C=[_,_,_,_,_]) #"@includedef{test/1}".
:- test steps(C,N) : (N = s(s(0))) => (C=[_,_,_,_,_,_,_]) #"@includedef{test/1}".