(defun join (str-list)
    (format nil "~{~A~^ ~}" str-list))

(defun format-line (line)
    (join (map 'list (lambda (c)
        (concatenate 'string
            (list (digit-char (cell-value c))
            (cell-right c)
            (cell-bottom c)))) line))
)

(defun print-board (board)
    (terpri)
    (map 'list (lambda (l) (format t "~a ~%" (format-line l))) board))
