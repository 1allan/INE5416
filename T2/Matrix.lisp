; Retorna a ordem da matriz
(defun getOrder (matrix)
    (sqrt (getSize matrix))
)

; Retorna o tamanho da matriz
(defun getSize (matrix)
    (if (null matrix)
        0
        (+ 1 (getSize (cdr matrix)))
    )
)

; Percorre a matriz filtrando os elementos pelo índice para retornar a região
(defun getRegionAux (matrix region rows columns initialRow initialColumn order index remaining)
    (setq row    (floor index order))
    (setq column (mod   index order))
    (if (= remaining 0)
        ()
        (if (and (<= initialRow    row    (+ initialRow rows -1))
                 (<= initialColumn column (+ initialColumn columns -1)))
            (cons (car matrix) 
            (getRegionAux (cdr matrix) region rows columns initialRow initialColumn order (+ index 1) (- remaining 1))
            )
            (getRegionAux (cdr matrix) region rows columns initialRow initialColumn order (+ index 1) remaining)
        )
    )
)

; Retorna a região da matriz quadrada passada
(defun getRegion (matrix region)
    (setq order         (getOrder matrix))
    (setq rows          (ceiling (sqrt order)))             ; Quantidade de linhas  por região e quantidade de regiões que compartilham determinada linha
    (setq columns       (floor (sqrt order)))               ; Quantidade de colunas por região e quantidade de regiões que compartilham determinada coluna
    (setq initialRow    (- region  (mod region rows)))      ; (* rows (floor region rows))
    (setq initialColumn (* columns (mod region rows)))
    (getRegionAux matrix region rows columns initialRow initialColumn order 0 order)
)
; Retorna o elemento da matriz passada
(defun getElement (matrix index)
    (if (= index 0)
        (car matrix)
        (getElement (cdr matrix) (- index 1))
    )
)

; Retorna "order" elementos a partir da posição "begin"
(defun getRowAux (matrix begin order)
    (if (= begin 0)
        (if (= order 0)
            ()
            (cons (car matrix) (getRowAux (cdr matrix) 0 (- order 1)))
        )
        (getRowAux (cdr matrix) (- begin 1) order)
    )
)
 
; Retorna a linha da matriz quadrada passada
(defun getRow (matrix row)
    (setq order (getOrder matrix))
    (getRowAux matrix (* row order) order)
)

; Retorna a coluna da matriz quadrada passada sem calcular order
(defun getColumnAux (matrix column order index)
    (if (null matrix)
        ()
        (if (= column (mod index order))
            (cons (car matrix) (getColumnAux (cdr matrix) column order (+ index 1)))
            (getColumnAux (cdr matrix) column order (+ index 1))
        )
    )
)

; Retorna a coluna da matriz quadrada passada
(defun getColumn (matrix column)
    (setq order (getOrder matrix))
    (getColumnAux matrix column order 0)
)

; Altera o elemento na posição do indíce com o valor informado
(defun setElement (matrix index value)
    (if (= index 0)
        (cons value (cdr matrix))
        (cons (car matrix) (setElement (cdr matrix) (- index 1) value))
    )
)

; Cria uma matrix de tamanho e valor informado
(defun generateMatrixAux (value size)
    (if (= size 0)
        ()
        (cons value (generateMatrixAux value (- size 1)))
    )
)

; Cria uma matriz com o valor informado
(defun generateMatrix (order value)
    (setq size (expt order 2))
    (generateMatrixAux value size)
)

; Imprime uma região a partir da linha informada
(defun printRegionFromRow (region row rows columns)
    (write-line (write-to-string (getRowAux region (* row columns) columns)))
    (if (= row (- rows 1))
        (values)
        (printRegionFromRow region (+ row 1) rows columns)
    )
)

; Imprime uma região isolada de uma matriz quadrada
(defun printRegion (region)
    (setq order (getSize region))
    (setq rows    (ceiling (sqrt order))) ; Quantidade de linhas  por região
    (setq columns (floor (sqrt order)))   ; Quantidade de colunas por região
    (printRegionFromRow region 0 rows columns)
)

; Imprime um vetor verticalmente
(defun printColumn (column)
    (if (null column)
        (values)
        (progn
            (write-line (write-to-string(car column)))
            (printColumn (cdr column))
        )
    )
)

; Imprime uma matriz quadrada a partir da linha informada
(defun printMatrixFromRow (matrix row order)
    (write-line (write-to-string (getRow matrix row)))
    (if (= row (- order 1))
        (values)
        (printMatrixFromRow matrix (+ row 1) order)
    )
)

; Imprime uma matriz quadrada
(defun printMatrix (matrix)
    (setq order (getOrder matrix))
    (printMatrixFromRow matrix 0 order)
)