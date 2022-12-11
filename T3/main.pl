:- include('sudoku.pl').
:- compile('sudoku.pl').

# Inicializacao do programa
main :-
    write('Board id: '),
    read(BoardName)
    append("./boards/", BoardName, makeBoardName).
    append(makeBoardName, ".txt", FullBoardName).
    open(FullBoardName, read, Str),
    read_file(Str,Lines),

    write('Solving... '),

    close(Str),
    #write(Lines), nl.

    solve(Lines) 



# Carregar o tabuleiro, em formato de .txt, passado e retorna a estrutura de dados de um tabuleiro.
read_file(Stream,[]) :-
    at_end_of_stream(Stream).

read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read(Stream,X),
    read_file(Stream,L).