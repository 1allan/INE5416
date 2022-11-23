(load "./sudoku.lisp")
(load "./print.lisp")

(defun split-string (str separator)
    (loop for i = 0 then (1+ j)
        as j = (position separator str :start i)
        collect (subseq str i j)
        while j))

(defun parse (rows)
    (let (new-cell map-row)
        (setq new-cell (lambda (tuple)
            (make-cell
                :value 0
                :right (char tuple 0)
                :bottom (char tuple 1))))
        (setq map-row (lambda (row) (map 'list new-cell (split-string row #\Space))))
        (map 'list map-row rows)))

(defun main ()
    (write-string "Board id: ")
    (setq board-id (read))
    (setq file-name (concatenate `string "./boards/" (write-to-string board-id) ".txt"))
    (setq list_ '())
    (with-open-file (in file-name :if-does-not-exist nil)
        (when in
            (loop for line = (read-line in nil)
                while line do (push line list_))))
    (terpri)
    (print-board (solve (parse (reverse list_)))))

(main)
