(ql:quickload "cl-ppcre")
(defvar *cave*)
(defvar *maxrow*)

(defun load-input ()
  (setf *maxrow* 0)
  (setf *cave* (make-hash-table :test #'equal))
  (with-open-file (stream "input.txt")
  (loop
    with max-row = 0
    for line = (read-line stream nil) while (not (zerop (length line)))
    do (let ((parts (cl-ppcre::split " " line)))
         (loop for p1 in parts by #'cddr
               for p2 in (cddr parts) by #'cddr
               do (let* ((p1ps (mapcar #'read-from-string (cl-ppcre::split "," p1)))
                         (p2ps (mapcar #'read-from-string (cl-ppcre::split "," p2)))
                         (x1 (first p1ps))
                         (y1 (second p1ps))
                         (x2 (first p2ps))
                         (y2 (second p2ps)))
                    (setf *maxrow* (max y1 y2 *maxrow*))
                    (cond ((= x1 x2)
                            (loop for i from (min y1 y2) to (max y1 y2)
                                  do (setf (gethash (cons x1 i) *cave*) :rock) )
                            )
                           ((= y1 y2)
                            (loop for i from (min x1 x2) to (max x1 x2)
                                  do (setf (gethash (cons i y1) *cave*) :rock) )))))))))


;; returns:
;; - nil if x,y already occupied
;; - (x' . y') containing the final position of a sand that is put in position x,y
(defun trickle-down (x y)
  (when (not (gethash (cons x y) *cave*))
    (if (> y *maxrow*)
        (cons x y)
        (or (trickle-down x (1+ y))
            (trickle-down (1- x) (1+ y))
            (trickle-down (1+ x) (1+ y))
            (cons x y)))))


;; part1
(load-input)
(loop
  for sand = (trickle-down 500 0) while (<= (cdr sand) *maxrow*)
  do (progn
       (setf (gethash sand *cave*) :sand))
  counting t)

;; part 2
(load-input)
(1+ (loop
      for sand = (trickle-down 500 0) while (not (equal sand (cons 500 0)))
      do (progn
           (setf (gethash sand *cave*) :sand))
      counting t))
