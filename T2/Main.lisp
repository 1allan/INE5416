(load "./Matrix.lisp")
(load "./OperatorMatrix.lisp")
(load "./Validator.lisp")
(load "./PuzzleSolver.lisp")

(defun main()
    (setq example  (list 00 01 02  03 04 05  06 07 08
                         09 10 11  12 13 14  15 16 17
                         18 19 20  21 22 23  24 25 26

                         27 28 29  30 31 32  33 34 35
                         36 37 38  39 40 41  42 43 44
                         45 46 47  48 49 50  51 52 53

                         54 55 56  57 58 59  60 61 62
                         63 64 65  66 67 68  69 70 71
                         72 73 74  75 76 77  78 79 80))

    (setq example6 (list 00 01  02 03  04 05
                         06 07  08 09  10 11
                         12 13  14 15  16 17
                         18 19  20 21  22 23
                         24 25  26 27  28 29
                         30 31  32 33  34 35))

    (setq example4 (list 1 2 3 4
                         2 0 0 0
                         0 0 0 0
                         0 0 0 0))

    (setq operatorMatrix9 (list "<" ">" "|" "<" "<" "|" ">" ">" "|"
                                "v" "^" "^" "v" "v" "v" "^" "v" "^"
                                "<" "<" "|" "<" ">" "|" ">" "<" "|"
                                "^" "^" "^" "^" "v" "v" "v" "v" "v"
                                "<" "<" "|" "<" ">" "|" ">" "<" "|"

                                "|" "|" "|" "|" "|" "|" "|" "|" "|"

                                ">" ">" "|" ">" "<" "|" "<" ">" "|"
                                "v" "^" "^" "v" "v" "v" "^" "v" "^"
                                ">" "<" "|" ">" "<" "|" ">" ">" "|"
                                "v" "^" "^" "^" "^" "v" "v" "v" "v"
                                "<" ">" "|" "<" ">" "|" ">" "<" "|"

                                "|" "|" "|" "|" "|" "|" "|" "|" "|"

                                "<" ">" "|" ">" ">" "|" "<" ">" "|"
                                "v" "v" "v" "^" "v" "v" "^" "^" "^"
                                ">" "<" "|" ">" "<" "|" "<" "<" "|"
                                "^" "^" "v" "v" "v" "^" "v" "^" "v"
                                "<" ">" "|" "<" "<" "|" "<" ">" "|"))

    (setq operatorMatrix6 (list "<" "|" ">" "|" ">" "|"
                                "^" "v" "^" "^" "^" "v"
                                ">" "|" ">" "|" ">" "|"
                                "v" "v" "v" "^" "^" "^"
                                "<" "|" "<" "|" "<" "|"
                                "|" "|" "|" "|" "|" "|"
                                "<" "|" "<" "|" "<" "|"
                                "^" "^" "v" "v" "^" "v"
                                ">" "|" "<" "|" "<" "|"
                                "v" "v" "^" "^" "^" "v"
                                ">" "|" "<" "|" ">" "|"))

    (setq operatorMatrix4 (list "<" "|" "<" "|"
                                "v" "^" "^" "v"
                                "<" "|" ">" "|"

                                "|" "|" "|" "|"
                                
                                ">" "|" "<" "|"
                                "v" "^" "^" "v"
                                ">" "|" ">" "|"))
    
    (setq matrixI5 (list 2 3 1 4
                         1 4 0 0
                         0 0 0 0
                         0 0 0 0))

    (printMatrix (solveMatrix operatorMatrix4))
    (printMatrix (solveMatrix operatorMatrix6))
    (printMatrix (solveMatrix operatorMatrix9))

    ; (write-line (write-to-string (validateRow matrixI5 5)))
    ; (write-line (write-to-string (validateColumn matrixI5 5)))

    ; (write-line (write-to-string (validateAdjacents matrixI5 operatorMatrix4 5)))

    ; (write-line (write-to-string (validateElement matrixI5 operatorMatrix4 5)))

    ; (write-line (write-to-string (solveElement operatorMatrix4 matrixI5 4 1 5)))
    ; (write-line (write-to-string (validateArrayWithX '(1 2 3 4) 5)))
    ; (write-line (write-to-string (validateArrayWithX '(1 2 3 4) 3)))
    ; (write-line (write-to-string (validateArray '(1 0 0 0))))
    ; (write-line (write-to-string (validateArray '(1 2 2 4))))
    ; (write-line (write-to-string (validateRow example6 02)))
    ; (write-line (write-to-string (validateRow example6 14)))
    ; (write-line (write-to-string (validateColumn example6 01)))
    ; (write-line (write-to-string (validateColumn example6 04)))
    ; (write-line (write-to-string (validateOperation "<" 2 3)))
    ; (write-line (write-to-string (validateOperation ">" 4 6)))
    ;(write-line (write-to-string (validateOperation "v" 1 2)))
    ; (write-line (write-to-string (validateOperation "|" 1 5)))
    ; (write-line (write-to-string (validateRight example operatorMatrix9 0)))
    ; (write-line (write-to-string (validateLeft example operatorMatrix9 01)))
    ; (write-line (write-to-string (validateDown example operatorMatrix9 01)))
    ; (write-line (write-to-string (validateUp example operatorMatrix9 10)))
    ; (write-line (write-to-string (validateAdjacents example operatorMatrix9 10)))
    ; (write-line (write-to-string (validateRight example operatorMatrix9 10)))
    ; (write-line (write-to-string (validateLeft example operatorMatrix9 10)))
    ; (write-line (write-to-string (validateDown example operatorMatrix9 10)))
    ; (write-line (write-to-string (validateUp example4 operatorMatrix4 4)))
    ; (write-line (write-to-string (validateElement example operatorMatrix9 10)))
    ; (write-line (write-to-string (getElement (list (- 1)) 0 )))


    ; (printMatrix example)
    ; (write-line (write-to-string(checkright operatorMatrix9 40)))
    ; (write-line (write-to-string(checkleft  operatorMatrix9 40)))
    ; (write-line (write-to-string(checkup    operatorMatrix9 40)))
    ; (write-line (write-to-string(checkdown  operatorMatrix9 40)))
    ; (setq example2 (generateMatrix 9 0))
    ; (printMatrix example2)

    ; (write-line (write-to-string(getElement example 56)))
    ; (setq example3 (setElement example 56 66))
    ; (write-line (write-to-string(getElement example3 56)))
    ; (print (getElement example3 56))
    ; (printMatrix example3)
    ; (write-line (write-to-string(getSize example3)))
    ; (write-line (write-to-string(getOrder example3)))
    ; (write-line "-- Column")
    ; (printColumn (getColumn example 2))

    ; (write-line "-- Example 9x9")
    ; (write-line "-- Region 0")
    ; (printRegion (getRegion example 0))
    ; (write-line "-- Region 1")
    ; (printRegion (getRegion example 1))
     ;(write-line "-- Region 2")
    ; (printRegion (getRegion example 2))
    ; (write-line "-- Region 3")
    ; (printRegion (getRegion example 3))
    ; (write-line "-- Region 4")
    ; (printRegion (getRegion example 4))
    ; (write-line "-- Region 5")
    ; (printRegion (getRegion example 5))
    ; (write-line "-- Region 6")
    ; (printRegion (getRegion example 6))
    ; (write-line "-- Region 7")
    ; (printRegion (getRegion example 7))
    ; (write-line "-- Region 8")
    ; (printRegion (getRegion example 8))


    ; (write-line "-- Example 6x6")
    ; (printMatrix example6)

    ; (write-line "-- Region 0")
    ; (printRegion (getRegion example6 0))
    ; (write-line "-- Region 1")     
    ; (printRegion (getRegion example6 1))
    ; (write-line "-- Region 2")     
    ; (printRegion (getRegion example6 2))
    ; (write-line "-- Region 3")     
    ; (printRegion (getRegion example6 3))
    ; (write-line "-- Region 4")     
    ; (printRegion (getRegion example6 4))
    ; (write-line "-- Region 5")     
    ; (printRegion (getRegion example6 5))
)

(main)