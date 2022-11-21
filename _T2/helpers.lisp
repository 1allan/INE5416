(defun split-string (str separator)
    (loop for i = 0 then (1+ j)
        as j = (position separator str :start i)
        collect (subseq str i j)
        while j
    )
)
