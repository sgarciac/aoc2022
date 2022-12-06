;;;; day5.lisp

(in-package #:day5)

;; part 1
(with-open-file (stream "/Users/sergiogarcia/dev/aoc2022/day5/input.txt")
  (let ((stacks (loop
                  with stacks = (make-array 9 :initial-element '())
                  for line = (read-line stream nil) while (and (not (zerop (length line)))
                                                               (not (cl-ppcre:scan "1" line)))
                  do (loop for i from 1 upto 33 by 4
                           for j from 0 to 9
                           do (unless (char= (aref line i) #\ )
                                (setf  (aref stacks j) (nconc (aref stacks j) (list (intern (string (aref line i))))))))
                  finally (return stacks))))

    (read-line stream nil)
    (loop
      for line = (read-line stream nil) while (not (zerop (length line)))
      for parts = (cl-ppcre:split " " line) then (cl-ppcre:split " " line)
      do (let ((qty (read-from-string (second parts)))
               (from (1- (read-from-string (fourth parts))))
               (to (1- (read-from-string (sixth parts)))))
           (dotimes (i qty)
             (let ((element (car (aref stacks from))))
               (setf (aref stacks from) (cdr (aref stacks from)))
               (setf (aref stacks to) (cons element (aref stacks to)))))
           ))
    (loop for i from 0 upto 8
          do (print (car (aref stacks i))))))


;; part 2
(with-open-file (stream "/Users/sergiogarcia/dev/aoc2022/day5/input.txt")
  (let ((stacks (loop
                  with stacks = (make-array 9 :initial-element '())
                  for line = (read-line stream nil) while (and (not (zerop (length line)))
                                                               (not (cl-ppcre:scan "1" line)))
                  do (loop for i from 1 upto 33 by 4
                           for j from 0 to 9
                           do (unless (char= (aref line i) #\ )
                                (setf  (aref stacks j) (nconc (aref stacks j) (list (intern (string (aref line i))))))))
                  finally (return stacks))))

    (read-line stream nil)
    (loop
      for line = (read-line stream nil) while (not (zerop (length line)))
      for parts = (cl-ppcre:split " " line) then (cl-ppcre:split " " line)
      do (let* ((qty (read-from-string (second parts)))
                (from (1- (read-from-string (fourth parts))))
                (to (1- (read-from-string (sixth parts))))
                (elements (subseq (aref stacks from) 0 qty)))

           (setf (aref stacks to) (nconc elements (aref stacks to)))
           (setf (aref stacks from) (subseq  (aref stacks from) qty))
           ))
    (loop for i from 0 upto 8
          do (print (car (aref stacks i))))))
