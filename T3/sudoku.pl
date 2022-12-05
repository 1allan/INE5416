Cell(value , right , bottom).

Line([Cell, Cell, Cell, Cell, Cell, Cell, Cell, Cell, Cell])
Board([Line, Line, Line, Line, Line, Line, Line, Line, Line])

# inverseOf :: Char -> Char
# inverseOf '+' = '-'
# inverseOf '-' = '+'
# inverseOf c = c

char('+').
char('-').
inverseOf(char).
'-' :- inverseOf('+')
'+' :- inverseOf(char('-'))









# partialOp :: Char -> Int -> (Int -> Bool)
# partialOp '+' val = (>) val
# partialOp '-' val = (<) val
# partialOp _ _ = const True

partialOp(Char,Int)
'>' val :- partialOp('+',Int)
'<' val :- partialOp('-',Int)
_ True :- partialOp(_, _)






# rightOp :: Cell -> (Int -> Bool)
# rightOp (Cell v r _) = partialOp r v
# rightOp (Cell v r _) = partialOp x v


rightOp(Cell)
partialOp(right, value) :- rightOp(Cell(value , right , bottom))



# bottomOp :: Cell -> (Int -> Bool)
# bottomOp (Cell v _ b) = partialOp b v
bottomOp(Cell)
partialOp(bottom, value) :- bottomOp(Cell(value , right , bottom))




# boardToList :: Board -> [Cell]
# boardToList = concat
# retorna o Result9 que é o concat
boardToList(Board)
append(append(append(append(append(append(append(append(Line1, Line2, Result2), Line3, Result3), Line4, Result4), Line5, Result5), Line6, Result6), Line7, Result7), Line8, Result8), Line9, Result9) :- 
boardToList(Board([Line1, Line2, Line3, Line4, Line5, Line6, Line7, Line8, Line9]))






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
listToBoard(Lista)
    Board(slice(Lista,0, 8, Return1), slice(Lista,9, 17, Return2),
     slice(Lista,18, 27, Return3), slice(Lista,28, 36, Return4), slice(Lista,37,45, Return5), slice(Lista,46,54, Return6),
      slice(Lista,55,63, Return7), slice(Lista,64,72, Return8), slice(Lista,73,81, Return9)) 



# itop :: Int -> (Int, Int)
# itop i = swap (i `divMod` 9)
# LISP:
#(defun itop (i)
# (list (mod i 9) (floor i 9)))

# LISP
# (defun itop (i)
#     (list (mod i 9) (floor i 9)))

itop()






# cellAt :: Board -> (Int, Int) -> Cell
# cellAt b (x, y) = (b !! y) !! x

cellAt(Board, (a, b))





#rowAt :: Board -> Int -> Line
#rowAt m i
#    | i < 0 || i >= length m = []
#    | otherwise = m !! i