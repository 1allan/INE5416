;(setq cell)

(defun inverseOf (n)
    (cond
        ((= n "+") "-")
        ((= n "-") "+")
        ;((= n n) "Caso 3") ;inverseOf c = c
    )

)

; (defun partialOp (char val)
;     (cond
;         ((and (= char "+") val) ">" val)
;     )

; )
; partialOp :: Char -> Int -> (Int -> Bool)
; partialOp '+' val = (>) val
; partialOp '-' val = (<) val
; partialOp _ _  = const True