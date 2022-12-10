(defparameter *input* "sample")

(defun read-input ()
  (with-open-file (stream *input*)
    (loop for line = (read-line stream nil) while (not (zerop (length line)))
          collecting (cons (intern (subseq line 0 1) "KEYWORD")
                           (read-from-string (subseq line 2))
                           ))))

(defvar *hp* '(0 . 0))
(defvar *tp* '(0 . 0))
(defvar *visited* nil)

(defun move-h-u () (setf *hp* (cons (car *hp*) (1+ (cdr *hp*)))))
(defun move-t-u () (setf *tp* (cons (car *tp*) (1+ (cdr *tp*)))) (mark-visited))

(defun move-h-d () (setf *hp* (cons (car *hp*) (1- (cdr *hp*)))))
(defun move-t-d () (setf *tp* (cons (car *tp*) (1- (cdr *tp*)))) (mark-visited))

(defun move-h-l () (setf *hp* (cons (1- (car *hp*)) (cdr *hp*))))
(defun move-t-l () (setf *tp* (cons (1- (car *tp*)) (cdr *tp*))) (mark-visited))

(defun move-h-r () (setf *hp* (cons (1+ (car *hp*)) (cdr *hp*))))
(defun move-t-r () (setf *tp* (cons (1+ (car *tp*)) (cdr *tp*))) (mark-visited))

(defun move-t-ul () (setf *tp* (cons (1- (car *tp*)) (1+ (cdr *tp*)))) (mark-visited))
(defun move-t-ur () (setf *tp* (cons (1+ (car *tp*)) (1+ (cdr *tp*)))) (mark-visited))
(defun move-t-dl () (setf *tp* (cons (1- (car *tp*)) (1- (cdr *tp*)))) (mark-visited))
(defun move-t-dr () (setf *tp* (cons (1+ (car *tp*)) (1- (cdr *tp*)))) (mark-visited))

(defun mark-visited ()
  (setf (gethash  *tp* *visited*) t)
  )

(defun reset-world ()
  (setf *hp* '(0 . 0))
  (setf *tp* '(0 . 0))
  (setf *visited* (make-hash-table :test #'equal))
  (mark-visited))

(progn
  (setf *input* "input.txt")
  (reset-world)
  (loop
        for step in (read-input)
        do (dotimes (i (cdr step))
             (ecase (car step)
               (:U (move-h-u))
               (:D (move-h-d))
               (:L (move-h-l))
               (:R (move-h-r)))
             (unless (and (<= (abs (- (car *hp*) (car *tp*))) 1)
                          (<= (abs (- (cdr *hp*) (cdr *tp*))) 1))
               (cond ((= (car *hp*) (car *tp*)) ;; vertically aligned
                      (if (< (cdr *hp*) (cdr *tp*))
                          (move-t-d)
                          (move-t-u)))
                     ((= (cdr *hp*) (cdr *tp*))
                      (if (< (car *hp*) (car *tp*))
                          (move-t-l)
                          (move-t-r)))
                     ((and (> (car *hp*) (car *tp*)) (< (cdr *hp*) (cdr *tp*)))
                      (move-t-dr))
                     ((and (> (car *hp*) (car *tp*)) (> (cdr *hp*) (cdr *tp*)))
                      (move-t-ur))
                     ((and (< (car *hp*) (car *tp*)) (> (cdr *hp*) (cdr *tp*)))
                      (move-t-ul))
                     ((and (< (car *hp*) (car *tp*)) (< (cdr *hp*) (cdr *tp*)))
                      (move-t-dl))))
             (print "----")
             (print *hp*)
             (print *tp*)

             )

        )

  (hash-table-count *visited*))
