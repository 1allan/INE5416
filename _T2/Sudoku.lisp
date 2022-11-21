(defstruct cell value right bottom)

(defun inverse-of (op)
    (cond
        ((string= op "+") "-")
        ((string= op "-") "+")
        (T op)
    )
)

(defun partial-op (op val)
    (cond
        ((string= op "+") (lambda (x) (> val x)))
        ((string= op "-") (lambda (x) (< val x)))
        (T (lambda (x) T))
    )
)

(defun right-op (c)
    (partial-op (cell-right c) (cell-value c))
)

(defun bottom-op (c)
    (partial-op (cell-bottom c) (cell-value c))
)

(defun board-to-list (b)
    (defun rec (acc b)
        (if (= (length b) 0)
            acc
            (rec (nconc acc (copy-list (car b))) (cdr b))
        )
    )
    (rec nil b)
)

(defun list-to-board (l)
    (defun rec (acc l)
        (if (= (length l) 0)
            acc
            (rec (push (subseq l 0 9) acc) (subseq l 9))
        )
    )
    (reverse (rec nil l))
)

(defun itop (i)
    (cons (mod i 9) (floor i 9)))

(defun cell-at (board coord)
    (nth (first coord) (nth (second coord) board))
)

(defun row-at (board i)
    (if (or (< i 0) (>= i (length board)))
        nil
        (nth i board)
    )
)

(defun column-at (board i)
    (defun rec (acc b)
        (if (<= (length b) 0)
            acc
            (rec (push (nth i (car b)) acc) (cdr b))
        )
    )
    (reverse (rec nil (copy-list board)))
)

(defun region-at (board coord)
    (let (b x y rows)
        (setf b (copy-list board))
        (setf x (* (/ (first coord) 3) 3))
        (setf y (* (/ (second coord) 3) 3))
        (setf rows (subseq (subseq b y) 0 3))
        (map 'list (lambda (r) (subseq (subseq r x) 0 3)) rows)
    )
)

(defun possibilities (b_ index)
    (let (coord x y )
        (setf coord (itop index))
        (setf x (first coord))
        (setf y (second coorsd))

        (setf b (list-to-board b_)
        (setf current-cell (cell-at b coord)))
        (setf upper-cell
            (if (> y 0)
                (cell-at b (x . (- y 1)))
                (make-cell :value 0 :right "." :bottom ".")))
        (setf left-cell
            (if (> x 0)
                (cell-at b ((- x 1) . y))
                (make-cell :value 0 :right "." :bottom ".")))

        (setf assert_ (lambda (value)
            (every
                (lambda (cond_) (funcall cond_ value))
                '((right-op left-cell) (bottom-op upper-cell)))))

        (setf used-values (
            map 'list
            cell-value
            '((row-at b y) (column-at b x) (region-at b coord))))

        (setf operations '(
            (inverse-of (cell-right left-cell))
            (inverse-of (cell-bottom upper-cell))
            (cell-right current-cell)
            (cell-bottom current-cell)))
        (setf min-value (+ 1 (count "+" operations)))
        (setf max-value (- 9 (count "-" operations)))

        (setf possibilities
            (remove-if-not
                (lambda (v) (= (count v used-values) 0))
                (loop :from min-value :below max-value :for n :collect n)))

        (remove-if-not (lambda (p) (funcall assert_ p)) possibilities)
    )
)
