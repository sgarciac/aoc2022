(defstruct monkey
  items
  op
  test
  true-monkey
  inspected
  inspected-list
  false-monkey)

(defparameter *monkeys* '())

(defun reset-monkeys ()
  (setf *monkeys* (make-array 8 :initial-contents
                              (list
                               (make-monkey :items (list 93 98)
                                            :op (cons #'* 17)
                                            :test 19
                                            :inspected 0
                                            :inspected-list '()
                                            :true-monkey 5
                                            :false-monkey 3)
                               (make-monkey :items (list 95 72 98 82 86)
                                            :op (cons #'+ 5)
                                            :test 13
                                            :inspected 0
                                            :inspected-list '()
                                            :true-monkey 7
                                            :false-monkey 6)
                               (make-monkey :items (list 85 62 82 86 70 65 83 76)
                                            :op (cons #'+ 8)
                                            :test 5
                                            :inspected 0
                                            :inspected-list '()
                                            :true-monkey 3
                                            :false-monkey 0)
                               (make-monkey :items (list 86 70 71 56)
                                            :op (cons #'+ 1)
                                            :test 7
                                            :inspected 0
                                            :inspected-list '()
                                            :true-monkey 4
                                            :false-monkey 5)
                               (make-monkey :items (list 77 71 86 52 81 67)
                                            :op (cons #'+ 4)
                                            :test 17
                                            :inspected 0
                                            :inspected-list '()
                                            :true-monkey 1
                                            :false-monkey 6)
                               (make-monkey :items (list 89 87 60 78 54 77 98)
                                            :op (cons #'* 7)
                                            :test 2
                                            :inspected 0
                                            :inspected-list '()
                                            :true-monkey 1
                                            :false-monkey 4)
                               (make-monkey :items (list 69 65 63)
                                            :op (cons #'+ 6)
                                            :test 3
                                            :inspected 0
                                            :inspected-list '()
                                            :true-monkey 7
                                            :false-monkey 2)
                               (make-monkey :items (list 89)
                                            :op (cons #'* nil)
                                            :test 11
                                            :inspected 0
                                            :inspected-list '()
                                            :true-monkey 0
                                            :false-monkey 2)))))

(defconstant +factor+ (* 19 13 5 7 17 2 3 11))

(defun process-monkey (monkey)
  (loop
    for item in (monkey-items monkey) do
      (let ((new-item
              (floor (/ (funcall (car (monkey-op monkey)) item
                                 (or (cdr (monkey-op monkey))
                                     item)
                                 ) 3))))
        (if (zerop (mod new-item (monkey-test monkey)))
            (setf (monkey-items (aref *monkeys* (monkey-true-monkey monkey)))
                  (append (monkey-items (aref *monkeys* (monkey-true-monkey monkey)))
                          (list new-item)))
            (setf (monkey-items (aref *monkeys* (monkey-false-monkey monkey)))
                  (append (monkey-items (aref *monkeys* (monkey-false-monkey monkey)))
                          (list new-item))))))
  (incf (monkey-inspected monkey) (length (monkey-items monkey)))
  (setf (monkey-inspected-list monkey) (cons (length (monkey-items monkey)) (monkey-inspected-list monkey)))
  (setf (monkey-items monkey) '()))

(reset-monkeys)
(loop for i from 1 to 20
      do (loop for monkey across *monkeys*
               do (process-monkey monkey)
                  finally (print i)
               ))
*monkeys*1
(* 282 279)


;; part 2
(defun process-monkey (monkey)
  (loop
    for item in (monkey-items monkey) do
      (let ((new-item (funcall (car (monkey-op monkey)) item
                               (or (cdr (monkey-op monkey))
                                   item))))
        (if (zerop (mod new-item (monkey-test monkey)))
            (setf (monkey-items (aref *monkeys* (monkey-true-monkey monkey)))
                  (append (monkey-items (aref *monkeys* (monkey-true-monkey monkey)))
                          (list (mod new-item +factor+))))
            (setf (monkey-items (aref *monkeys* (monkey-false-monkey monkey)))
                  (append (monkey-items (aref *monkeys* (monkey-false-monkey monkey)))
                          (list (mod new-item +factor+)))))))
  (incf (monkey-inspected monkey) (length (monkey-items monkey)))
  (setf (monkey-inspected-list monkey) (cons (length (monkey-items monkey)) (monkey-inspected-list monkey)))
  (setf (monkey-items monkey) '()))


(reset-monkeys)
(loop for i from 1 to 10000
      do (loop for monkey across *monkeys*
               do (process-monkey monkey)
               finally (print i)))

(setf solution (sort (loop for monkey across *monkeys*
                          collecting (monkey-inspected monkey)) #'> ))
(* (car solution) (second solution))
