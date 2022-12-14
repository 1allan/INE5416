board = [
    ['++', '++', '.-', '+-', '++', '.-', '++', '+-', '.-'],
    ['--', '-+', '.-', '++', '-+', '.+', '++', '+-', '.-'],
    ['+.', '-.', '..', '+.', '-.', '..', '-.', '-.', '..'],
    ['++', '--', '.-', '+-', '--', '.+', '++', '+-', '.+'],
    ['+-', '--', '.-', '-+', '++', '.-', '-+', '++', '.-'],
    ['-.', '+.', '..', '-.', '+.', '..', '+.', '-.', '..'],
    ['--', '-+', '.+', '--', '+-', '.-', '++', '--', '.+'],
    ['--', '+-', '.-', '--', '++', '.+', '-+', '++', '.+'],
    ['-.', '+.', '..', '+.', '+.', '..', '-.', '-.', '..']
].

operation(op_greater).
operation(op_less).
operation(op_noop).

% Conta os elementos em uma lista
count(_, [], 0).
count(X, [X | T], N) :-
    !, count(X, T, N1),
    N is N1 + 1.
count(X, [_ | T], N) :-
    count(X, T, N).

% Verifica se o elemento está presente na lista
not_a_member(_, []).
not_a_member(El, [El|_]) :- !.
not_a_member(El, [_|T]) :- !, not_a_member(El, T).

% Predicados de acesso de dados das células
value(V, [V|_]).
right_op(Op, [_|Op]).
bottom_op(Op, Cell) :- nth0(2, Cell, Op).

% Cria uma nova célula
create_new_cell(Value, RightOp, BottomOp, Cell) :-
    Cell = [Value, RightOp, BottomOp].


% Cria uma nova célula vazia
create_empty_cell(Cell) :-
    Cell = [0, opNoop, opNoop].

% Inverte a operação: "maior que" vira "menor que", vice-versa. op_noop caso
% contrário (não há operação)
inverse_of(Op, Out) :-
    (Op = op_noop -> Out = op_noop;
        (Op = op_less -> Out = op_greater; Out = op_less)).

% Aplica a operação Op com os números N1 e N2
operate(N1, N2, Op) :-
    (Op = op_noop -> N1 = N1
    ; (Op = op_greater -> N1 > N2
      ; N1 < N2
      )
    ).

% Transforma uma matriz em lista
list_to_board([], _, []).
list_to_board(Board, Size, [List|Rest]):-
    list_to_board_aux(Board, Size, List, Tail),
    list_to_board(Tail, Size, Rest).

list_to_board_aux(Tail, 0, [], Tail).
list_to_board_aux([Item|Board], Size, [Item|List], Tail):-
    NSize is Size - 1,
    list_to_board_aux(Board, NSize, List, Tail).

% Transforma uma lista em uma matriz
board_to_list([], _, []).
board_to_list(List, T, [Start|Rest]) :-
    append(Start, Remainder, List),
    length(Start, T),
    board_to_list(Remainder, T, Rest).

% Transforma um índice em um ponto
itop(I, Point) :-
    X is I rem 9,
    Y is I // 9,
    Point = [X ,Y].

% Retorna a linha de um tabuleiro em um dado índice.
row_at(Board, I, Row) :- nth0(I, Board, Row).

% Retorna a coluna do tabuleiro em um dado índice transpondo-o e utilizando a 
% `row_at`
column_at(Board, I, Column) :-
    transpose(Board, T),
    row_at(T, I, Column).

% Retorna a região do tabuleiro a qual um dado ponto (x, y) pertence
region_at(Board, [X|Y], Region) :-
    Y_ is Y // 3 * 3,
    X_ is X // 3 * 3,
    drop(X_, Board, B),
    take(3, B, Rows),
    board_to_list(Region, 3, Rows).

% Retorna uma célula do tabuleiro
cell_at(Board, [X|Y], Cell) :-
    nth0(Y, Board, Row),
    nth0(X, Row, Cell).

replace(List, Element, I, NewList) :-
    take(I, List, FirstPart),
    drop(I + 1, List, SecondPart),
    append([FirstPart, [Element], SecondPart], NewList).

possibilities(Board, I, List) :-
    itop(I, Coord),
    list_to_board(Board, 9, B),
    cell_at(B, Coord, CurrentCell),
    (Y > 0 -> cell_at(B, [X, Y - 1], UpperCell); create_empty_cell(UpperCell)),
    (X > 0 -> cell_at(B, [X - 1, Y], LeftCell); create_empty_cell(LeftCell)),
    row_at(Board, I, Row),
    column_at(Board, I, Column),
    region_at(Board, I, Region),
    Shared = [Row, Column, Region],
    maplist(value, Shared, UsedValues),

    right_op(LeftCell, RightOp),
    inverse_of(RightOp, InvRightOpLeftCell),
    bottomt_op(UpperCell, BottomtOp),
    inverse_of(BottomtOp, InvBottomtOpUpperCell),
    right_op(CurrentCell, RightOpCurr),
    bottom_op(CurrentCell, BottomOpCurr),
    Operations = [InvRightOpLeftCell, InvBottomtOpUpperCell, RightOpCurr, BottomOpCurr],
    count('+', Operations, GreaterQuantity),
    count('-', Operations, LessQuantity),
    MinValue is 1 + GreaterQuantity,
    MaxValue is 9 - LessQuantity,

    not_a_member(X, Possibilities), X >= MinValue, X <= MaxValue,
    maplist(Possibilities, List).

solve(Board, Solved) :-
    board_to_list(Board, 9, B),
    possibilities(B, PossMatrix)
    go(B, PossMatrix, 0, yes, Solved).

go(Board, PossMatrix, I, Forward, Result) :- !.
    (I = -1; I = 81 -> list_to_board(Board, Solved), Result = Solved
    ;
        (Forward = yes -> possibilities(Board, I, Poss);  nth0(I, PossMatrix, Poss))
    ),
    length(Poss, PossQuantity),
    (PossQuantity > 0 -> 
        nth0(I, Board, CurrCell),
        create_empty_cell(EmptyCell),
        replace(Board, EmptyCell, I, ReplacedBoard),
        go(ReplacedBoard, PossMatrix, I - 1, no)
    ;
        take(1, Poss, X),
        drop(1, Poss, XS),
        nth0(I, Board, CurrCell),
        right(CurrCell, RightOp),
        bottom(CurrCell, BottomOp),
        create_new_cell(X, RightOp, BottomOp, NewCell),
        replace(Board, NewCell, I, ReplacedBoard),
        replace(PossMatrix, XS, I, ReplacedPossMatrix),
        go(ReplacedBoard, ReplacedPossMatrix, I + 1, yes)
    ).
