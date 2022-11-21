; Validar se o número passado não está no array
(defun validateArrayWithX (array value)
   (if (= (getSize array) 0)
      T
      (and (/= (car array) value) (validateArrayWithX (cdr array) value))
   )
)

; Validar linha/coluna passada
(defun validateArray (array)
   (if (= (getSize array) 0)
      T
      (and (validateArray (cdr array))
           (or (= (car array) 0) (validateArrayWithX (cdr array) (car array))))
   )
)

; Checar se linha tem elementos repetidos
(defun validateRow (numberMatrix index)
   (setq order    (getOrder numberMatrix))
   (setq row      (floor index order))
   (validateArray (getRow numberMatrix row))
)

; Checar se coluna tem elementos repetidos
(defun validateColumn (numberMatrix index)
   (setq order    (getOrder numberMatrix))
   (setq column   (mod index order))
   (validateArray (getColumn numberMatrix column))
)

#| Validar coerência entre dois elementos
         (sempre considerando da esquerda para
         a direita ou de cima para baixo) |#
(defun validateOperation (operator element1 element2)
   (if (or (string= operator "|") (= element1 0) (= element2 0))
      T
      (cond
         ((string= operator ">") (> element1 element2))
         ((string= operator "<") (< element1 element2))
         ((string= operator "v") (> element1 element2))
         ((string= operator "^") (< element1 element2))
      )
   )
)

; Validar a coerência entre o elemento passado e aquele à sua direita
(defun validateRight (numberMatrix operatorMatrix index)
   (setq order   (getOrder numberMatrix))
   (setq column  (mod index order))
   (or (= column order) (validateOperation 
      (checkright operatorMatrix index)
      (getElement numberMatrix index)      
      (getElement numberMatrix (+ index 1)))
   )
)

; Validar a coerência entre o elemento passado e aquele à sua esquerda
(defun validateLeft (numberMatrix operatorMatrix index)
   (setq order  (getOrder numberMatrix))
   (setq column (mod index order))
   (or (= column 0) (validateOperation
      (checkleft operatorMatrix index)
      (getElement numberMatrix (- index 1))
      (getElement numberMatrix index))
   )
)

; Validar a coerência entre o elemento passado e aquele embaixo
(defun validateDown (numberMatrix operatorMatrix index)
   (setq order (getOrder numberMatrix))
   (setq row   (floor index order))
   (or (= row (- order 1)) (validateOperation
      (checkdown operatorMatrix index)
      (getElement numberMatrix index)
      (getElement numberMatrix (+ index order)))
   )
)

; Validar a coerência entre o elemento passado e aquele acima
(defun validateUp (numberMatrix operatorMatrix index)
   (setq order (getOrder numberMatrix))
   (setq row   (floor index order))
   (or (= row 0) (validateOperation
      (checkup operatorMatrix index)
      (getElement numberMatrix (- index order))
      (getElement numberMatrix index))
   )
)

; Validar a coerência entre o elemento passado e todos seus adjacentes
(defun validateAdjacents (numberMatrix operatorMatrix index)
   (and
      (validateRight numberMatrix operatorMatrix index)
      (validateLeft numberMatrix operatorMatrix index)
      (validateDown numberMatrix operatorMatrix index)
      (validateUp numberMatrix operatorMatrix index)
   )
)

; Validar coerência de um elemento no puzzle
(defun validateElement (numberMatrix operatorMatrix index)
   (and
      (validateRow numberMatrix index)
      (validateColumn numberMatrix index)
      (validateAdjacents numberMatrix operatorMatrix index)
   )
)