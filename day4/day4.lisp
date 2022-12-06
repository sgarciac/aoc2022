;;;; day4.lisp

(in-package #:day4)

(defstruct range start end)

(defun parse-line(line)
  (let ((parts (cl-ppcre:split "-|," line)))
    (cons
     (make-range :start (read-from-string (first parts))
                 :end (read-from-string (second parts)))
     (make-range :start (read-from-string (third parts))
                 :end (read-from-string (fourth parts))))))

;; true if either one contains the other
(defun contain-p (range1 range2)
  (or (and (<= (range-start range1) (range-start range2))
           (>= (range-end range1) (range-end range2))
           )
      (and (<= (range-start range2) (range-start range1))
           (>= (range-end range2) (range-end range1)))))

;; true if any overlap
(defun overlap-p (range1 range2)
  (or
   (contain-p range1 range2)
   (and (<= (range-start range1) (range-end range2))
           (>= (range-end range1) (range-end range2)))
      (and (<= (range-start range1) (range-start range2))
           (>= (range-end range1) (range-start range2)))))

;; part1
(with-open-file (stream "/Users/sergiogarcia/dev/aoc2022/day4/input.txt")
  (loop
    for line = (read-line stream nil) while (not (zerop (length line)))
    counting (let ((ranges (parse-line line)))
               (contain-p (car ranges) (cdr ranges)))))

(with-open-file (stream "/Users/sergiogarcia/dev/aoc2022/day4/input.txt")
  (loop
    for line = (read-line stream nil) while (not (zerop (length line)))
    counting (let ((ranges (parse-line line)))
               (format t "~A ~A~%" line (overlap-p (car ranges) (cdr ranges)))
               (overlap-p (car ranges) (cdr ranges)))))
