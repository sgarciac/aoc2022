(defparameter *input* "sample")
(setf *input* "/Users/sergiogarcia/dev/aoc2022/day10/input.txt")


(defun read-input ()
  (with-open-file (stream *input*)
    (loop for line = (read-line stream nil) while (not (zerop (length line)))
          collecting (let ((op (intern (string-upcase (subseq line 0 4)) "KEYWORD") ))
                       (print op)
                       (if (eq op :noop)
                           (cons op nil)
                           (cons op (read-from-string (subseq line 5))))))))

; part1
(loop
  with significant = '(20 60 100 140 180 220)
  with signal = 0
  with x = 1
  with cycle = 1
  for inst in (read-input)
  do (progn
       (when (find cycle significant)
         (setf signal (+ signal (* cycle x))))
       (case (car inst)
         (:NOOP
          (incf cycle)
          )
         (:ADDX
          (incf cycle)
          (when (find cycle significant)
            (setf signal (+ signal (* cycle x))))
          (incf cycle)
          (setf x (+ x (cdr inst))))))
  finally (return signal))

; part2
(loop
  with screen = (make-string 240 :initial-element #\.)
  with pixel = 0
  with x = 1
  for inst in (read-input)
  do (progn
       (when (and (>= (mod pixel 40) (1- x)) (<= (mod pixel 40) (1+ x)))
         (setf (char screen pixel) #\#))
       (case (car inst)
         (:NOOP
          (incf pixel)
          )
         (:ADDX
          (incf pixel)
          (when (and (>= (mod pixel 40) (1- x)) (<= (mod pixel 40) (1+ x)))
            (setf (char screen pixel) #\#))
          (incf pixel)
          (setf x (+ x (cdr inst))))))
  finally (progn
            (print (subseq screen 0 40))
            (print (subseq screen 40 80))
            (print (subseq screen 80 120))
            (print (subseq screen 120 160))
            (print (subseq screen 160 200))
            (print (subseq screen 200 240))
            ))
