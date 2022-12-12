board)[
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

% Predicados de acesso de dados das células
value(V, [V|_]).
right_op(Op, [_|Op]).
bottom_op(Op, Cell) :- nth0(2, Cell, Op).

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

possibilities(Board, I, List) :- !.
solve(Board, Solved) :- !.
