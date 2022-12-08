(defconstant +input+ "input.txt")

(let*
    ((lines (with-open-file (stream +input+) (loop for line = (read-line stream nil) while (not (zerop (length line))) collecting line)))
     (lc (length lines))
     (rc (length (first lines)))
     (trees (make-array (list (+ 2 lc) (+ 2 rc)) :initial-element -1))
     (visibility (make-array (list (+ 2 lc) (+ 2 rc)))))
  ;; init visibility
  (loop for l from 0 upto (1+ lc) do (loop for r from 0 upto (1+ rc) do (setf (aref visibility l r) (make-array 4 :initial-element -1))))
  ;; load trees
  (loop for l from 1 upto lc for line in lines
    do (loop for r from 1 upto rc do (setf (aref trees l r) (digit-char-p (char line (1- r))))))

  ;; traversing top to bottom left to right
  (loop for l from 1 upto lc
        do (loop for r from 1 upto rc
                 do (progn
                      ;; from top
                      (setf (aref (aref visibility l r) 0)
                            (max
                             (aref trees (1- l) r)
                             (aref (aref visibility (1- l) r) 0)))
                      ;; from left
                      (setf (aref (aref visibility l r) 3)
                            (max
                             (aref trees l (1- r))
                             (aref (aref visibility l (1- r)) 3))))))

  ;; traversinf from bottom to top, right to left
  (loop for l from lc downto 1
        do (loop for r from rc downto 1
                 do (progn
                      ;; from bottom
                      (setf (aref (aref visibility l r) 2)
                            (max
                             (aref trees (1+ l) r)
                             (aref (aref visibility (1+ l) r) 2)))
                      ;; from right
                      (setf (aref (aref visibility l r) 1)
                            (max
                             (aref trees l (1+ r))
                             (aref (aref visibility l (1+ r)) 1))))
                 ))
  (loop for l from 1 upto lc summing
                             (loop for r from 1 upto rc counting (find-if (lambda (x) (< x (aref trees l r)))
                                                                          (aref visibility l r)))))
