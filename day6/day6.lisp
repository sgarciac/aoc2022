;;;; day6.lisp

(in-package #:day6)


(defun all-different (str start end)
  (loop
    with bag = '()
    for i from start to end
    do (progn
         (when (find (char str i) bag :test #'char=)
           (return-from all-different nil))
         (setf bag (adjoin (char str i) bag :test #'char=))))
  t)

(defun find-mark (str size)
  (loop
    for i from 0 to (- (length str) size)
    for j from (+ i size) to (length str)
    do (when (all-different str i j)
         (return-from find-mark j))))

;; part1
(with-open-file (stream "/Users/sergiogarcia/dev/aoc2022/day6/input.txt")
  (let ((line (read-line stream)))
    (find-mark line 4)))

;; part2
(with-open-file (stream "/Users/sergiogarcia/dev/aoc2022/day6/input.txt")
  (let ((line (read-line stream)))
    (find-mark line 14)))
