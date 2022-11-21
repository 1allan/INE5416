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
        (T (constantly T))
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
    (list (mod i 9) (floor i 9)))

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
        (setf x (* (floor (first coord) 3) 3))
        (setf y (* (floor (second coord) 3) 3))
        (setf rows (subseq (subseq b y) 0 3))
        (map 'list (lambda (r) (subseq (subseq r x) 0 3)) rows)
    )
)

(defun replace_ (l e i)
    (if (and (>= i 0) (< i (length l)))
        (concatenate 'list (subseq l 0 i) (list e) (subseq l (+ i 1)))
        l
    )
)

(defun possibilities (b_ index)
    (let (
            coord x y b current-cell upper-cell left-cell assert_ used_values
            operations min-value max-value possibilities
        )
        (setf coord (itop index))
        (setf x (first coord))
        (setf y (second coord))

        (setf b (list-to-board b_))
        (setf current-cell (cell-at b coord))
        (setf upper-cell
            (if (> y 0)
                (cell-at b (list x (- y 1)))
                (make-cell :value 0 :right "." :bottom ".")))
        (setf left-cell
            (if (> x 0)
                (cell-at b (list (- x 1) y))
                (make-cell :value 0 :right "." :bottom ".")))

        (setf assert_ (lambda (value)
            (every
                (lambda (cond_) (funcall cond_ value))
                (concatenate 'list (list (right-op left-cell)) (list (bottom-op upper-cell))))))

        (setf used-values (
            map 'list
            (lambda (c) (cell-value c))
            (concatenate 'list (row-at b y) (column-at b x) (board-to-list (region-at b coord))))) ; using `board-to-list` to flatten the 3x3 matrix

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
                (loop :for n :from min-value :below max-value :collect n)))

        (remove-if-not (lambda (p) (funcall assert_ p)) possibilities)
    )
)

(defun solve (b)
    (defun rec- (b b_ i forward)
        (cond
            ((= i -1) (list-to-board b))
            ((= i 81) (list-to-board b))
            (t (progn
                (setq poss (if (not (null forward)) (possibilities b i) (nth i b_)))
                (if (= (length poss) 0)
                    (progn
                        (setq curr-cell (nth i b))
                        (setq empty-cell (make-cell
                            :value 0
                            :right (cell-right curr-cell)
                            :bottom (cell-bottom curr-cell)))
                        (rec- (replace_ b empty-cell i) b_ (- i 1) nil)
                    )
                    (progn
                        (setq curr-cell (nth i b))
                        (setq new-cell (make-cell
                            :value (first poss)
                            :right (cell-right curr-cell)
                            :bottom (cell-bottom curr-cell)))
                        (rec- (replace_ b new-cell i) (replace_ b_ (rest poss) i) (+ i 1) t)
                    )
                )
            ))
        )
    )
    (rec- (board-to-list b) (map 'list (constantly nil) (board-to-list b)) 0 t)
)
