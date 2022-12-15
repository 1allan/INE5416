:- include('sudoku.pl').
:- compile('sudoku.pl').

main :-
    write('Board id: '),
    read(BoardName)
    append("./boards/", BoardName, makeBoardName).
    append(makeBoardName, ".txt", FullBoardName).
    open(FullBoardName, read, Str),
    read_file(Str,Lines),

    write('Solving... '),

    close(Str),

    solve(Lines) 

read_file(Stream,[]) :-
    at_end_of_stream(Stream).

read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read(Stream,X),
    read_file(Stream,L).