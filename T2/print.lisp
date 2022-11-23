(defun splice-in-3 (some-list separator)
    (append
        (subseq some-list 0 3)
        (list separator)
        (subseq some-list 3 6)
        (list separator)
        (subseq some-list 6 9)))

(defun join (str-list)
    (format nil "~{~A~^ ~}" (splice-in-3 str-list " ")))

(defun format-line (line)
    (join (map 'list (lambda (c)
        (concatenate 'string
            (list (digit-char (cell-value c))
            (cell-right c)
            (cell-bottom c)))) line)))

(defun print-board (board)
    (map 'list (lambda (l)
        (if (null l)
            (terpri)
            (format t "~a ~%" (format-line l))))
        (splice-in-3 board nil)))
