Cell(value , right , bottom).

% Tipos 'Line' para conter uma linha de células e 'Board' que contém todas as linhas do tabuleiro. 
Line([Cell, Cell, Cell, Cell, Cell, Cell, Cell, Cell, Cell]).
Board([Line, Line, Line, Line, Line, Line, Line, Line, Line]).

char('+').
char('-').
% Inverte o char de uma operação, utilizada para computar as operações de células acima e à esquerda
inverseOf(char).
'-' :- inverseOf('+').
'+' :- inverseOf(char('-')).

% Converte um char para a função equivalente e retorna uma função parcialmente aplicada com o int fornecido
partialOp(Char,Int).
'>' val :- partialOp('+',Int).
'<' val :- partialOp('-',Int).
_ True :- partialOp(_, _).

% Recebe uma célula e invoca partialOp
rightOp(Cell).
partialOp(right, value) :- rightOp(Cell(value , right , bottom)).

bottomOp(Cell).
partialOp(bottom, value) :- bottomOp(Cell(value , right , bottom)).

% As funcões boardToList e listToBoard são usadas para podermos tratar o tabuleiro como matriz 9x9 ou como lista de 81 células, de acordo com o que for conveniente.
boardToList(Board).
append(append(append(append(append(append(append(append(Line1, Line2, Result2), Line3, Result3), Line4, Result4), Line5, Result5), Line6, Result6), Line7, Result7), Line8, Result8), Line9, Result9) :- 
boardToList(Board([Line1, Line2, Line3, Line4, Line5, Line6, Line7, Line8, Line9])).

slice(Board, From, To, Return):-
    length(LFrom, From),
    length([_|LTo], To),
    append(LTo, _, L),
    append(LFrom, Return, LTo).

% converte a lista para formato de tabuleiro
listToBoard(Lista).
    Board(slice(Lista,0, 8, Return1), slice(Lista,9, 17, Return2),
     slice(Lista,18, 27, Return3), slice(Lista,28, 36, Return4), slice(Lista,37,45, Return5), slice(Lista,46,54, Return6),
      slice(Lista,55,63, Return7), slice(Lista,64,72, Return8), slice(Lista,73,81, Return9)).

itop(Int).
(i mod 9)(floor(i)):- itop(i).

% Funcões responsáveis por retornar uma celula ou uma lista de célula do tabuleiro ao passar uma coordenada.
cellAt(board, coord).
(board, (x,y)) :- ((board, (x, y)), (x \= y) ).

% retorna uma célula presente em dada linha
rowAt(Board, Int).
(fail) :- rowAt(board, i),((i < 0);(:-(i>=(length(board))))).
(board[i]):- rowAt(board, i).  

% retorna uma célula presente em dada coluna
columnAt(Board, Int).
(fail) :- columnAt((fail) :- column(board, i),((i < 0);(:-(i>=(length(board))))).).
(board[i]):- column(board, i). 

% retorna uma célula presente em dada região
regionAt(Board, coord).
(x, floor(first coord) :- (x `div` 3 * 3).
(y, floor(first coord) :- (y `div` 3 * 3).

% Retorna uma cópia da trocando o valor fornecido no índice especificado
replace(l, e).
drop (i + 1) :- ((i >= 0);(:-(i < length(l)))).

% Determina a lista de possibilidades de uma célula.
possibilities(b).
listToBoard(b2, b).
cellAt(x,y - 1) :- (y > 0).
% Determina a celula atual, a celula acima dela e a celula à esquerda dela
leftCell(x - 1, y) :- (x > 0).
% Reduz o intervalo de possibilidades
(filter (=='+') operations, minValue).
(filter (=='+') operations, maxValue).


% ### Ideia do algoritmo de solução:
% Percorre o tabuleiro e determina os numeros que devem preencher cada celula,
% usando a funcão possibilities para determinar os valores possiveis em cada iteração.
% Os passos são:
% 0. O índice inicia em 0 e a matriz de possibilidades inicia vazia
% 1. Verifica o valor do índice
%     1.1 O valor é -1 ou 81
%         1.1.1 Passo 4
% 2. O algoritmo está incrementando ou decrementando? 
%     2.1 Incrementando
%         2.1.1 Calcula as possibilidades da célula atual
%         2.1.2 Passo 3
%     2.2 Decrementando
%         2.2.1 Busca as possiblidades restantes disponível na matriz para a célula atual
% 3. Verifica as possibilidades
%     3.1. Há possibilidades
%         3.1.1. Substitui o valor da célula no tabuleiro pela primeira possibilidade calculada
%         3.1.2. Guarda o resto das possibilidades na matriz de possibilidades
%         3.1.3. Vai para a próxima célula
%         3.1.4. Passo 1
%     3.2. Não há possibilidades
%         3.2.1 Zera o valor da célula atual no tabuleiro
%         3.2.2 Retorna à célula anterior
%         3.2.3 Passo 1
% 4. Fim do algoritmo.

solve(Board).
