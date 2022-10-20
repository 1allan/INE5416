module Sudoku where
import Data.Tuple (swap)

data Cell = Cell {value :: Int, right :: Char, bottom :: Char}

-- Tipos 'Line' para conter uma linha de células e 'Board' que contém todas as linhas do tabuleiro. 
type Line = [Cell]
type Board = [Line]

-- Inverte o char de uma operação, utilizada para computar as operações de células acima e à esquerda
inverseOf :: Char -> Char
inverseOf '+' = '-'
inverseOf '-' = '+'
inverseOf c = c

-- Converte um char para a função equivalente e retorna uma função parcialmente aplicada com o int fornecido
partialOp :: Char -> Int -> (Int -> Bool)
partialOp '+' val = (>) val
partialOp '-' val = (<) val
partialOp _ _  = const True

-- Recebe uma célula e invoca partialOp
rightOp :: Cell -> (Int -> Bool)
rightOp (Cell v r _) = partialOp r v

bottomOp :: Cell -> (Int -> Bool)
bottomOp (Cell v _ b) = partialOp b v


{- As funcões boardToList e listToBoard são usadas para podermos tratar o tabuleiro
como matriz 9x9 ou como lista de 81 células, de acordo com o que for conveniente. -}
boardToList :: Board -> [Cell]
boardToList = concat

listToBoard :: [Cell] -> Board
listToBoard [] = []
listToBoard b = take 9 b : listToBoard (drop 9 b)

itop :: Int -> (Int, Int)
itop i = swap (i `divMod` 9)


{- Funcões responsáveis por retornar uma celula ou uma lista de célula do tabuleiro 
ao passar uma coordenada. -}
cellAt :: Board -> (Int, Int) -> Cell
cellAt b (x, y) = (b !! y) !! x

rowAt :: Board -> Int -> Line
rowAt m i
    | i < 0 || i >= length m = []
    | otherwise = m !! i

columnAt :: Board -> Int -> Line
columnAt [] _ = []
columnAt (x:xs) i
    | i < 0 || i >= length x = []
    | otherwise = x !! i : columnAt xs i

regionAt :: Board -> (Int, Int) -> Line
regionAt b (x, y) = let
        y' = (y `div` 3 * 3)
        x' = (x `div` 3 * 3)
        rows = take 3 (drop y' b)
    in
    concatMap (take 3 . drop x') rows



-- Retorna uma cópia da trocando o valor fornecido no índice especificado
replace :: [a] -> a -> Int -> [a]
replace l e i
    | i >= 0 && i < length l = take i l ++ [e] ++ drop (i + 1) l
    | otherwise = l


-- Determina a lista de possibilidades de uma célula.
possibilities :: [Cell] -> Int -> [Int]
possibilities b' index =
    let
        coord@(x, y) = itop index

        b = listToBoard b'
        -- Determina a celula atual, a celula acima dela e a celula à esquerda dela
        currentCell = cellAt b coord
        upperCell = if y > 0 then cellAt b (x, y - 1) else Cell 0 '.' '.'
        leftCell = if x > 0 then cellAt b (x - 1, y) else Cell 0 '.' '.'

        -- Função de validação de células adjacentes de acordo com a operação
        assert v = all (\cond -> cond v) [rightOp leftCell, bottomOp upperCell]

        usedValues = map value (rowAt b y ++ columnAt b x ++ regionAt b coord)

        {- Reduz o intervalo de possibilidades com base nas 4 operações que a célula está relacionada
        seguindo o critério: [1 + quantidade de >, 9 - quantidade de -] -}
        operations = [inverseOf (right leftCell), inverseOf (bottom upperCell), right currentCell, bottom currentCell]
        minValue = 1 + length (filter (=='+') operations)
        maxValue = 9 - length (filter (=='-') operations)

        possibilities = filter (`notElem` usedValues) [minValue .. maxValue]
    in
    filter assert possibilities



{-
Percorre o tabuleiro e determina os numeros que devem preencher cada celula,
usando a funcão possibilities para determinar os valores possiveis em cada iteração.
Os passos são:
0. O índice inicia em 0 e a matriz de possibilidades inicia vazia
1. Verifica o valor do índice
    1.1 O valor é -1 ou 81
        1.1.1 Passo 4
2. O algoritmo está incrementando ou decrementando? 
    2.1 Incrementando
        2.1.1 Calcula as possibilidades da célula atual
        2.1.2 Passo 3
    2.2 Decrementando
        2.2.1 Busca as possiblidades restantes disponível na matriz para a célula atual
3. Verifica as possibilidades
    3.1. Há possibilidades
        3.1.1. Substitui o valor da célula no tabuleiro pela primeira possibilidade calculada
        3.1.2. Guarda o resto das possibilidades na matriz de possibilidades
        3.1.3. Vai para a próxima célula
        3.1.4. Passo 1
    3.2. Não há possibilidades
        3.2.1 Zera o valor da célula atual no tabuleiro
        3.2.2 Retorna à célula anterior
        3.2.3 Passo 1
4. Fim do algoritmo.
-}
solve :: Board -> Board
solve b = let
        board = boardToList b
        -- Cria uma matriz de possibilidades para cada célula do tabuleiro
        possMatrix =  map (const []) board
    in
    go board possMatrix 0 True where
        go b _ (-1) _ = listToBoard b
        go b _ 81 _ = listToBoard b
        go b b' i forward = let
                poss = if forward then possibilities b i else b' !! i
            in
            case poss of
                [] -> let
                        currCell = b !! i
                        emptyCell = Cell 0 (right currCell) (bottom currCell)
                    in
                    go (replace b emptyCell i) b' (i - 1) False
                _  -> let
                        (x:xs) = poss
                        currCell = b !! i
                        newCell = Cell x (right currCell) (bottom currCell)
                    in
                    go (replace b newCell i) (replace b' xs i) (i + 1) True

