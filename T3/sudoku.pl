Cell(value , right , bottom).

Line([Cell, Cell, Cell, Cell, Cell, Cell, Cell, Cell, Cell]).
Board([Line, Line, Line, Line, Line, Line, Line, Line, Line]).

# inverseOf :: Char -> Char
# inverseOf '+' = '-'
# inverseOf '-' = '+'
# inverseOf c = c

char('+').
char('-').
inverseOf(char).
'-' :- inverseOf('+').
'+' :- inverseOf(char('-')).









# partialOp :: Char -> Int -> (Int -> Bool)
# partialOp '+' val = (>) val
# partialOp '-' val = (<) val
# partialOp _ _ = const True

partialOp(Char,Int).
'>' val :- partialOp('+',Int).
'<' val :- partialOp('-',Int).
_ True :- partialOp(_, _).






# rightOp :: Cell -> (Int -> Bool)
# rightOp (Cell v r _) = partialOp r v
# rightOp (Cell v r _) = partialOp x v


rightOp(Cell).
partialOp(right, value) :- rightOp(Cell(value , right , bottom)).



# bottomOp :: Cell -> (Int -> Bool)
# bottomOp (Cell v _ b) = partialOp b v
bottomOp(Cell).
partialOp(bottom, value) :- bottomOp(Cell(value , right , bottom)).




# boardToList :: Board -> [Cell]
# boardToList = concat
# retorna o Result9 que é o concat
boardToList(Board).
append(append(append(append(append(append(append(append(Line1, Line2, Result2), Line3, Result3), Line4, Result4), Line5, Result5), Line6, Result6), Line7, Result7), Line8, Result8), Line9, Result9) :- 
boardToList(Board([Line1, Line2, Line3, Line4, Line5, Line6, Line7, Line8, Line9])).






# listToBoard :: [[Cell, Cell, Cell, Cell, Cell, Cell, Cell, Cell, Cell]] -> Board
# listToBoard [] = []
# listToBoard b = take 9 b : listToBoard (drop 9 b)
# LISP
#
# (defun board-to-list (b)
#     (defun rec (acc b)
#         (if (= (length b) 0)
#             acc
#             (rec (nconc acc (copy-list (car b))) (cdr b))))
#     (rec nil b))

# funcao auxiliar
slice(Board, From, To, Return):-
    length(LFrom, From),
    length([_|LTo], To),
    append(LTo, _, L),
    append(LFrom, Return, LTo).


# executa e retorna:
# ?- slice([a,b,c,d,e], 1, 4, Return).
# Return = [b, c].

# retornta um Board de uma lista de lines
listToBoard(Lista).
    Board(slice(Lista,0, 8, Return1), slice(Lista,9, 17, Return2),
     slice(Lista,18, 27, Return3), slice(Lista,28, 36, Return4), slice(Lista,37,45, Return5), slice(Lista,46,54, Return6),
      slice(Lista,55,63, Return7), slice(Lista,64,72, Return8), slice(Lista,73,81, Return9)).



# itop :: Int -> (Int, Int)
# itop i = swap (i `divMod` 9)
# LISP:
#(defun itop (i)
# (list (mod i 9) (floor i 9)))

# LISP
# (defun itop (i)
#     (list (mod i 9) (floor i 9)))

itop(Int).
(i mod 9)(floor(i)):- itop(i).






# cellAt :: Board -> (Int, Int) -> Cell
# cellAt b (x, y) = (b !! y) !! x
#LISP
# (defun cell-at (board coord)
#     (nth (first coord) (nth (second coord) board)))

cellAt(board, coord).
(board, (x,y)) :- ((board, (x, y)), (x \= y) ).




#rowAt :: Board -> Int -> Line
#rowAt m i
#    | i < 0 || i >= length m = []
#    | otherwise = m !! i

# LISP
# (defun row-at (board i)
#     (if (or (< i 0) (>= i (length board)))
#         nil
#         (nth i board)))

rowAt(Board, Int).
(fail) :- rowAt(board, i),((i < 0);(:-(i>=(length(board))))).
(board[i]):- rowAt(board, i).    # vai rodar o rowAt, se nao retornar Fail entao retorna sla kk





############ falta traduzir:  



# columnAt :: Board -> Int -> Line
# columnAt [] _ = []
# columnAt (x:xs) i
#     | i < 0 || i >= length x = []
#     | otherwise = x !! i : columnAt xs i
# LISP
# (defun column-at (board i)char
#     (defun reccc (acc b)
#         (if (<= (length b) 0)
#             acc
#             (reccc (push (nth i (car b)) acc) (cdr b))))
#     (reverse (reccc nil (copy-list board))))

columnAt(Board, Int).
() :- columnAt().






# regionAt :: Board -> (Int, Int) -> Line
# regionAt b (x, y) = let
#         y' = (y `div` 3 * 3)
#         x' = (x `div` 3 * 3)
#         rows = take 3 (drop y' b)
#     in
#     concatMap (take 3 . drop x') rows
# LISP
# (defun region-at (board coord)
#     (let (b x y rows)
#         (setf b (copy-list board))
#         (setf x (* (floor (first coord) 3) 3))
#         (setf y (* (floor (second coord) 3) 3))
#         (setf rows (subseq (subseq b y) 0 3))
#         (map 'list (lambda (r) (subseq (subseq r x) 0 3)) rows)))

regionAt(Board, coord).

 



# (defun replace_ (l e i)
#     ...


# (defun possibilities (b_ index)
#     ...

# (defun solve (board)
#     ...