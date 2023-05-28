:- module(PR2,_,[assertions]).

author_data('Cabo', 'Ciudad', 'Alvaro', '200172').

:- doc(title, "Game Board Operations Module").

:- doc(author, "Alvaro Cabo Ciudad").
:- doc(module, "Module for Game Board Operations in Prolog

@section{Predicates}
@subsection{Board definition}
Defines a fixed 4x4 game board with pre-set operations.

@subsection{Game logic predicates}
Defines predicates for checking valid movements, selecting cells and directions, applying operations to cells and generating a game path.

@def{efectuar_movimiento/3} Predicate that calculates the resulting position `Pos2` when moving from the given position `Pos` in the specified direction `Dir`.
@includedef{efectuar_movimiento/3}

@def{movimiento_valido/3} Predicate that checks if a move is valid. A move is valid if the resulting position from moving from `Pos` in direction `Dir` results in a valid position on an `N x N` board.
@includedef{movimiento_valido/3}

@def{select_cell/4} Predicate that extracts a cell with position `IPos` from the `Board` list to obtain a `NewBoard` list (without the extracted cell). The operation associated with the selected cell is unified with `Op`.
@includedef{select_cell/4}

@def{select_dir/3} Predicate that extracts a direction `Dir` from the list of alLed directions `Dirs`, obtaining in `NewDirs` the list of remaining alLed directions. `NewDirs` may be the same list as `Dirs` but with the number of applications of the selected direction decreased by one, or, if this was the last alLed application, without that element.
@includedef{select_dir/3}

@def{aplicar_op/3} Predicate that applies the operation indicated by `Op` to `Valor` to obtain `Valor2`. Given `Op` as `op(Operator,Operand)`, it applies the operation on `Valor`.
@includedef{aplicar_op/3}

@def{generar_recorrido/6} This predicate generates a game path. Its exact functionality needs to be defined in the context of your program.
@includedef{generar_recorrido/6}

@def{generar_recorridos/5} This predicate generates multiple game paths. Its exact functionality needs to be defined in the context of your program.
@includedef{generar_recorridos/5}

@def{tablero/5} This predicate relates to the game board. 
Its exact functionality needs to be defined in the context of your program.
@includedef{tablero/5}

@section(Testing)
Included at the end of the document all the @tt{:- test} assertions.\n
@subsection{Running the tests}
Testing can be run using the ciao console or using the integrated Ciao
debugger on Emacs
@begin{verbatim}

   ?- use_module(library(unittest)).

   yes
   ?- run_tests_in_module('/home/varo/UPM/3ero/ProDec/Pr_2/code.pl').
   {Reading /home/varo/UPM/3ero/ProDec/Pr_2/code.pl
   WARNING: (lns 323-326) [DireccionesPermitidas,N,NumeroDeRutasConValorMinimo,Tablero,ValorMinimo] - singleton variables in tablero/5
   }
   {Reading /home/varo/UPM/3ero/ProDec/Pr_2/code.pl
   WARNING: (lns 323-326) [DireccionesPermitidas,N,NumeroDeRutasConValorMinimo,Tablero,ValorMinimo] - singleton variables in tablero/5
   }
   {In /home/varo/UPM/3ero/ProDec/Pr_2/code.pl
   PASSED: (lns 327-334) efectuar_movimiento/3.
   PASSED: (lns 335-337) efectuar_movimiento/3.
   FAILED: (lns 338-341) distance/3.
   (lns 338-341) distance(_,_,_1) run-time check failure.
   Requires in *success*: 
      _1=3.
   But instead:
      _1=1
   PASSED: (lns 342-344) distance/3.
   PASSED: (lns 345-348) select_aux/3.
   PASSED: (lns 349-352) movimiento_valido/3.
   WARNING: (lns 353-356) select_cell/4. Goal tested failed, but test does not specify failure behavior.
   PASSED: (lns 357-360) select_dir/3.
   PASSED: (lns 361-364) aplicar_op/3.
   PASSED: (lns 365-367) aplicar_op/3.
   PASSED: (lns 368-370) generar_recorrido/6.
   WARNING: (lns 371-374) generar_recorrido/6. Goal tested failed, but test does not specify failure behavior.
   WARNING: (lns 375-378) generar_recorridos/5. Goal tested failed, but test does not specify failure behavior.
   }

   Note: {Total:
   Passed: 12 (92.31%) Failed: 1 (7.69%) Precond Failed: 0 (0.00%) Aborted: 0 (0.00%) Timeouts: 0 (0.00%) Total: 13 Run-Time Errors: 1
   }


   yes
   ?- 

@end{verbatim}
@subsection{efectuar_movimiento/3 tests}
   @bf{Basic Test 1}
   @begin{verbatim}
   :- test efectuar_movimiento(Pos, Dir, NewPos) : 
      (Pos = pos(1,1), Dir = n) => (NewPos = pos(0,1)).
   @end{verbatim}
   @bf{Basic Test 2}
   @begin{verbatim}
   :- test efectuar_movimiento(Pos, Dir, NewPos) : 
      (Pos = pos(1,1), Dir = e) => (NewPos = pos(1,2)).@end{verbatim}

@subsection{distance/3 tests}
   @bf{Basic Test}
   @begin{verbatim}
   :- test distance(L, H, Val) : 
   (L = 1, H = 5) => (Val = 3).
   @end{verbatim}
   @bf{Advanced Test}
   @begin{verbatim}
   :- test distance(L, H, Val) : 
   (L = 0, H = 0) => (Val = 0).
   @end{verbatim}

@subsection{select_aux/3 tests}
   @bf{Basic Test}
   @begin{verbatim}
   :- test select_aux(X, L, NewL) : 
      (X = a, L = [a,b,c]) => (NewL = [b,c]).
   @end{verbatim}

@subsection{movimiento_valido/3 tests}
   @bf{Basic Test}
   @begin{verbatim}
   :- test movimiento_valido(N, Pos, _) : 
   (N = 3, Pos = pos(2,2)) => true.
   @end{verbatim}

@subsection{select_cell/4 tests}
   @bf{Basic Test}
   @begin{verbatim}
   :- test select_cell(IPos, Op, Board, NewBoard) : 
   (IPos = pos(1,1), Op = op(*,-3), Board = board1(Board)) => (NewBoard is _).

   @end{verbatim}

@subsection{select_dir/4 tests}
   @bf{Basic Test}
   @begin{verbatim}
   :- test select_dir(Dir, Dirs, NewDirs) : 
   (Dir = n, Dirs = [dir(n, 2), dir(s, 1)], NewDirs = [dir(n, 1), dir(s, 1)]).

   @end{verbatim}

@subsection{aplicar_op/3 tests}
   @bf{Basic Test}
   @begin{verbatim}
   :- test aplicar_op(Op, Valor, Valor2) : 
   (Op = op(*,3), Valor = 2) => (Valor2 = 6).
   
   @end{verbatim}

   @bf{Basic Test 2}
   @begin{verbatim}
   :- test aplicar_op(Op, Valor, Valor2) : 
   (Op = op(-,1), Valor = 5) => (Valor2 = 4).

   @end{verbatim}


").

% Defining the board
:- pred board1(-Board)
#"Given the @var{Board} returns the initial state of the game board.".

board1([cell(pos(1,1),op(*,-3)),

cell(pos(1,2),op(-,1)),

cell(pos(1,3),op(-,4)),

cell(pos(1,4),op(-,555)),

cell(pos(2,1),op(-,3)),

cell(pos(2,2),op(+,2000)),

cell(pos(2,3),op(*,133)),

cell(pos(2,4),op(-,444)),

cell(pos(3,1),op(*,0)),

cell(pos(3,2),op(*,155)),

cell(pos(3,3),op(//,2)),

cell(pos(3,4),op(+,20)),

cell(pos(4,1),op(-,2)),

cell(pos(4,2),op(-,1000)),

cell(pos(4,3),op(-,9)),

cell(pos(4,4),op(*,4))]).

% Predicate to perform the movement
:- pred efectuar_movimiento(+Pos, +Dir, -Pos2)
#"Calculates the resulting position @var{Pos2} when moving from the given position @var{Pos} in the specified direction @var{Dir}.".

efectuar_movimiento(pos(Row, Col), n, pos(NewRow, Col)) :- NewRow is Row - 1.

efectuar_movimiento(pos(Row, Col), s, pos(NewRow, Col)) :- NewRow is Row + 1.

efectuar_movimiento(pos(Row, Col), e, pos(Row, NewCol)) :- NewCol is Col + 1.

efectuar_movimiento(pos(Row, Col), o, pos(Row, NewCol)) :- NewCol is Col - 1.

efectuar_movimiento(pos(Row, Col), ne, pos(NewRow, NewCol)) :- NewRow is Row - 1, NewCol is Col + 1.

efectuar_movimiento(pos(Row, Col), no, pos(NewRow, NewCol)) :- NewRow is Row - 1, NewCol is Col - 1.

efectuar_movimiento(pos(Row, Col), se, pos(NewRow, NewCol)) :- NewRow is Row + 1, NewCol is Col + 1.

efectuar_movimiento(pos(Row, Col), so, pos(NewRow, NewCol)) :- NewRow is Row + 1, NewCol is Col - 1.

% Cálculo de espacio entre casillas

distance(L, _, L).

% Recursive case: the number is in the range
distance(L, H, Val) :-
    L < H,
    NL is L + 1,
    distance(NL, H, Val).

% Predicate to check if the movement is valid
:- pred movimiento_valido(+N, +Pos, +Dir)
#"Checks if a move is valid. A move is valid if the resulting position from moving from @var{Pos} in direction @var{Dir} results in a valid position on an @var{N} x @var{N} board.".

movimiento_valido(N, pos(Row, Col), _) :-

    distance(1, N, Row),

    distance(1, N, Col).

% Predicate to select a cell
:- pred select_cell(+IPos, -Op, +Board, -NewBoard)
#"Extracts a cell with position @var{IPos} from the @var{Board} list to obtain a @var{NewBoard} list (without the extracted cell). The operation associated with the selected cell is unified with @var{Op}.".

% Predicado para seleccionar una celda

select_cell(IPos, Op, Board, NewBoard) :-

   select_aux(cell(IPos, Op), Board, NewBoard).


% Predicate to select a direction
:- pred select_dir(+Dir, +Dirs, -NewDirs)
#"Extracts a direction @var{Dir} from the list of alLed directions @var{Dirs}, obtaining in @var{NewDirs} the list of remaining alLed directions. @var{NewDirs} may be the same list as @var{Dirs} but with the number of applications of the selected direction decreased by one, or, if this was the last alLed application, without that element.".


% Predicado para seleccionar una dirección

select_dir(Dir, Dirs, NewDirs) :-

    select_aux(dir(Dir, Count), Dirs, TempDirs),

    NewCount is Count - 1,

    (NewCount > 0 -> NewDirs = [dir(Dir, NewCount)|TempDirs] ; NewDirs = TempDirs).


% Aux functions for selections

% Base case:
select_aux(X, [X|T], T).

% Recursive case:
select_aux(X, [H|T], [H|NewT]) :-
    select_aux(X, T, NewT).

% Predicate to apply an operation
:- pred aplicar_op(+Op, +Valor, -Valor2)
#"Applies the operation indicated by @var{Op} to @var{Valor} to obtain @var{Valor2}. Given @var{Op} as `op(Operator,Operand)`, it applies the operation on @var{Valor}.".

aplicar_op(op(Op, Val), Valor, Valor2) :-

    Expr =.. [Op, Valor, Val],

    Valor2 is Expr.


:- pred generar_recorrido(+Ipos, +N, +Board, +Dirs, -Recorrido, -FinalValue)
#"Generates a game path @var{Recorrido} with its final value @var{FinalValue}, starting from initial position @var{Ipos} in a @var{N} x @var{N} @var{Board} with possible directions @var{Dirs}.".

generar_recorrido(Ipos, N, Board, Dirs, Recorrido, FinalValue) :-

   generar_recorrido_aux(Ipos, N, Board, Dirs, Recorrido, 0, FinalValue).

% Base case

generar_recorrido_aux(_, _, [], _, [], AccValue, AccValue).

% Recursive case:

generar_recorrido_aux(Ipos, N, Board, Dirs, [(Ipos, Op, Valor, NewAccValue)|Rest], AccValue, FinalValue) :-

    % Selecciona la celda actual y la operación asociada a ella

    select_cell(Ipos, Op, Board, NewBoard),

    % Aplica la operación a la celda actual

    aplicar_op(Op, AccValue, NewAccValue),

    Valor = NewAccValue,

    % Selecciona una dirección válida para moverse

    select_dir(Dir, Dirs, NewDirs),

    % Calcula la nueva posición

    efectuar_movimiento(Ipos, Dir, NewPos),

    % Verifica que la nueva posición es válida

    movimiento_valido(N, NewPos, _),

    % Continúa el recorrido

    generar_recorrido_aux(NewPos, N, NewBoard, NewDirs, Rest, NewAccValue, FinalValue).


% All possible paths generation
generar_recorridos(N, Board, DireccionesPermitidas, Recorrido, Valor) :-

   member(cell(Ipos, _), Board),
   generar_recorrido(Ipos, N, Board, DireccionesPermitidas, Recorrido, Valor).

% Unimplemented predicates

tablero(N, Tablero, DireccionesPermitidas, ValorMinimo, NumeroDeRutasConValorMinimo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%% TESTING %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% EFECTUAR_MOVIMIENTO
:- test efectuar_movimiento(Pos, Dir, NewPos) : 
   (Pos = pos(1,1), Dir = n) => (NewPos = pos(0,1)).

:- test efectuar_movimiento(Pos, Dir, NewPos) : 
   (Pos = pos(1,1), Dir = e) => (NewPos = pos(1,2)).

% distance
:- test distance(Low, High, Val) : 
   (Low = 1, High = 5) => (Val = 3).

:- test distance(Low, High, Val) : 
   (Low = 0, High = 0) => (Val = 0).

% select_aux
:- test select_aux(X, L, NewL) : 
   (X = a, L = [a,b,c]) => (NewL = [b,c]).

% MOVIMIENTO_VALIDO
:- test movimiento_valido(N, Pos, _) : 
   (N = 3, Pos = pos(2,2)) => true.

% SELECT_CELL
:- test select_cell(IPos, Op, Board, NewBoard) : 
   (IPos = pos(1,1), Op = op(*,-3), Board = board1(Board)) => (NewBoard is _).

% SELECT_DIR
:- test select_dir(Dir, Dirs, NewDirs) : 
   (Dir = n, Dirs = [dir(n, 2), dir(s, 1)], NewDirs = [dir(n, 1), dir(s, 1)]).

% APLICAR_OP
:- test aplicar_op(Op, Valor, Valor2) : 
   (Op = op(*,3), Valor = 2) => (Valor2 = 6).

:- test aplicar_op(Op, Valor, Valor2) : 
   (Op = op(-,1), Valor = 5) => (Valor2 = 4).

% GENERAR RECORRIDO
:- test  generar_recorrido(1, 3, [], [], [], 0) => (Recorrido = [], FinalValue = 0).


:- test generar_recorrido(1, 3, [(1, 'Op', 5, [])], ['Op'], [(1, 'Op', 5, 5)], 5) =>
   (Recorrido = [(1, 'Op', 5, 5)], FinalValue = 5).

% GENERAR RECORRIDOS
:- test   generar_recorridos(3, [(1, 'Op', 5, []), (2, 'Op2', 10, [])], ['Op', 'Op2'], [(1, 'Op', 5, 5)], 5) =>
   (Recorrido = [(1, 'Op', 5, 5)], Valor = 5).
