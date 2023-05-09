:- module(PR1, [author_data/4, generar_recorrido/6, ], [assertions]).
:- use_module(library(unittest)).

author_data('Cabo', 'Ciudad', 'Alvaro', '200172').

:- doc(title, "Practica 2 - Programacion ISO-Prolog").

:- doc(author, "ALVARO CABO CIUDAD").
:- doc(module, "Minimun value path for mathematical operations

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
