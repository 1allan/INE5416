Cell(value , right , bottom).

Line([Cell, Cell, Cell, Cell, Cell, Cell, Cell, Cell, Cell]).
Board([Line, Line, Line, Line, Line, Line, Line, Line, Line]).

char('+').
char('-').
inverseOf(char).
'-' :- inverseOf('+').
'+' :- inverseOf(char('-')).

partialOp(Char,Int).
'>' val :- partialOp('+',Int).
'<' val :- partialOp('-',Int).
_ True :- partialOp(_, _).

rightOp(Cell).
partialOp(right, value) :- rightOp(Cell(value , right , bottom)).

bottomOp(Cell).
partialOp(bottom, value) :- bottomOp(Cell(value , right , bottom)).

#converte o tabuleiro para formato de lista
boardToList(Board).
append(append(append(append(append(append(append(append(Line1, Line2, Result2), Line3, Result3), Line4, Result4), Line5, Result5), Line6, Result6), Line7, Result7), Line8, Result8), Line9, Result9) :- 
boardToList(Board([Line1, Line2, Line3, Line4, Line5, Line6, Line7, Line8, Line9])).

slice(Board, From, To, Return):-
    length(LFrom, From),
    length([_|LTo], To),
    append(LTo, _, L),
    append(LFrom, Return, LTo).

# converte a lista para formato de tabuleiro
listToBoard(Lista).
    Board(slice(Lista,0, 8, Return1), slice(Lista,9, 17, Return2),
     slice(Lista,18, 27, Return3), slice(Lista,28, 36, Return4), slice(Lista,37,45, Return5), slice(Lista,46,54, Return6),
      slice(Lista,55,63, Return7), slice(Lista,64,72, Return8), slice(Lista,73,81, Return9)).

itop(Int).
(i mod 9)(floor(i)):- itop(i).


cellAt(board, coord).
(board, (x,y)) :- ((board, (x, y)), (x \= y) ).

# retorna uma célula presente em dada linha
rowAt(Board, Int).
(fail) :- rowAt(board, i),((i < 0);(:-(i>=(length(board))))).
(board[i]):- rowAt(board, i).  

# retorna uma célula presente em dada coluna
columnAt(Board, Int).
(fail) :- columnAt((fail) :- column(board, i),((i < 0);(:-(i>=(length(board))))).).
(board[i]):- column(board, i). 

# retorna uma célula presente em dada região
regionAt(Board, coord).
(x, floor(first coord) :- (x `div` 3 * 3)
(y, floor(first coord) :- (y `div` 3 * 3)

replace(l, e)
drop (i + 1) :- ((i >= 0);(:-(i < length(l))))

# gera a lista de possibilidades
possibilities(b)
listToBoard(b2, b)
cellAt(x,y - 1) :- (y > 0)
leftCell(x - 1, y) :- (x > 0)
(filter (=='+') operations, minValue)
(filter (=='+') operations, maxValue)
