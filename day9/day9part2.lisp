(defparameter *input* "sample")

(defun read-input ()
  (with-open-file (stream *input*)
    (loop for line = (read-line stream nil) while (not (zerop (length line)))
          collecting (cons (intern (subseq line 0 1) "KEYWORD")
                           (read-from-string (subseq line 2))
                           ))))

(defvar *knots* nil)
(defvar *visited* nil)



(defun move-u (knot) (setf (aref *knots* knot) (cons (car (aref *knots* knot)) (1+ (cdr (aref *knots* knot))))) (when (= knot 9) (mark-visited)))
(defun move-d (knot) (setf (aref *knots* knot) (cons (car (aref *knots* knot)) (1- (cdr (aref *knots* knot))))) (when (= knot 9) (mark-visited)))
(defun move-l (knot) (setf (aref *knots* knot) (cons (1- (car (aref *knots* knot))) (cdr (aref *knots* knot)))) (when (= knot 9) (mark-visited)))
(defun move-r (knot) (setf (aref *knots* knot) (cons (1+ (car (aref *knots* knot))) (cdr (aref *knots* knot)))) (when (= knot 9) (mark-visited)))
(defun move-ul (knot) (setf (aref *knots* knot) (cons (1- (car (aref *knots* knot))) (1+ (cdr (aref *knots* knot))))) (when (= knot 9) (mark-visited)))
(defun move-ur (knot) (setf (aref *knots* knot) (cons (1+ (car (aref *knots* knot))) (1+ (cdr (aref *knots* knot))))) (when (= knot 9) (mark-visited)))
(defun move-dl (knot) (setf (aref *knots* knot) (cons (1- (car (aref *knots* knot))) (1- (cdr (aref *knots* knot))))) (when (= knot 9) (mark-visited)))
(defun move-dr (knot) (setf (aref *knots* knot) (cons (1+ (car (aref *knots* knot))) (1- (cdr (aref *knots* knot))))) (when (= knot 9) (mark-visited)))

(defun mark-visited () (setf (gethash  (aref *knots* 9) *visited*) t))

(defun reset-world ()
  (setf *knots* (make-array 10 :initial-element '(0 . 0)))
  (setf *visited* (make-hash-table :test #'equal))
  (mark-visited))


(progn
  (setf *input* "input.txt")
  (reset-world)
  (loop for step in (read-input)
        do (dotimes (i (cdr step))
             (ecase (car step)
               (:U (move-u 0))
               (:D (move-d 0))
               (:L (move-l 0))
               (:R (move-r 0)))
             (loop
               for head from 0 to 8
               for tail from 1 to 9
               do (let ((hp (aref *knots* head))
                        (tp (aref *knots* tail)))
                    (unless (and (<= (abs (- (car hp) (car tp))) 1)
                                 (<= (abs (- (cdr hp) (cdr tp))) 1))
                      (cond ((= (car hp) (car tp)) ;; vertically aligned
                             (if (< (cdr hp) (cdr tp))
                                 (move-d tail)
                                 (move-u tail)))
                            ((= (cdr hp) (cdr tp))
                             (if (< (car hp) (car tp))
                                 (move-l tail)
                                 (move-r tail)))
                            ((and (> (car hp) (car tp)) (< (cdr hp) (cdr tp)))
                             (move-dr tail))
                            ((and (> (car hp) (car tp)) (> (cdr hp) (cdr tp)))
                             (move-ur tail))
                            ((and (< (car hp) (car tp)) (> (cdr hp) (cdr tp)))
                             (move-ul tail))
                            ((and (< (car hp) (car tp)) (< (cdr hp) (cdr tp)))
                             (move-dl tail))))))
             (print "----")))
  (hash-table-count *visited*))
