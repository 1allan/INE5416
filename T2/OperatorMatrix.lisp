; Retorna o operador a direita baseado no índice da matriz de números
(defun checkright (operatorMatrix index)
  (setq order   (getOrderOperatorMatrix operatorMatrix))
  (setq row     (floor index order))
  (setq column  (mod index order))
  (getElement operatorMatrix (+ column          (* order row 2)))
)

; Retorna o operador a esquerda baseado no índice da matriz de números
(defun checkleft (operatorMatrix index)
  (setq order   (getOrderOperatorMatrix operatorMatrix))
  (setq row     (floor index order))
  (setq column  (mod index order))
  (if (= column 0)
    "|"
    (getElement operatorMatrix (+ (- column 1) (* order row 2)))
  )
)

; Retorna o operador debaixo baseado no índice da matriz de números
(defun checkdown (operatorMatrix index)
  (setq order   (getOrderOperatorMatrix operatorMatrix))
  (setq row     (floor index order))
  (setq column  (mod index order))
  (if (= row (- order 1))
    "|"
    (getElement operatorMatrix (+ column        (* order (+ (* row 2) 1))))
  )
)

; Retorna o operador decima baseado no índice da matriz de números
(defun checkup (operatorMatrix index)
  (setq order   (getOrderOperatorMatrix operatorMatrix))
  (setq row     (floor index order))
  (setq column  (mod index order))
  (if (= row 0)
    "|"
    (getElement operatorMatrix (+ column     (* order (- (* row 2) 1))))
  )
)

; Retorna qual a linha do índice passado
(defun getRowIndexOperatorMatrix (operatorMatrix index)
  (floor index (getOrderOperatorMatrix operatorMatrix))
)

; Retorna a Ordem da matriz de operadores passada
(defun getOrderOperatorMatrix (operatorMatrix)
  (* (/ 1 4) (+ 1 (sqrt (+ 1 (* 8 (getSize operatorMatrix))))))
)