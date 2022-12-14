(ql:quickload "cl-ppcre")

(defvar *input*)

(defun rplc (o s n)
  (cl-ppcre:regex-replace-all o s n))

(defun parse-list (line)
  (read-from-string (rplc "\\]" (rplc "\\[" (rplc "," line " ") "(") ")")))

(defun right-order (l r)
  (cond
    ((and (numberp l) (numberp r) (= l r)) :equal)
    ((and (endp l) (not (endp r)))  :less)
    ((and (endp r) (not (endp l)))  :greater)
    ((and (endp l) (endp r))  :equal)
    ((and (numberp (car l)) (numberp (car r)) (< (car l) (car r)))  :less)
    ((and (numberp (car l)) (numberp (car r)) (> (car l) (car r)))  :greater)
    ((and (numberp (car l)) (listp (car r)))  (right-order (cons (list (car l)) (cdr l)) r))
    ((and (numberp (car r)) (listp (car l)))  (right-order l (cons (list (car r)) (cdr r))))
    (t  (let ((result (right-order (car l) (car r))))
                     (if (eq result :equal)
                         (right-order (cdr l) (cdr r))
                         result)))))

;; part 1
(with-open-file (stream "input.txt")
  (loop
    for lineleft = (read-line stream nil) while (not (zerop (length lineleft)))
    for lineright = (read-line stream nil) while (not (zerop (length lineright)))
    for emptyline = (read-line stream nil)
    for index = 1 then (1+ index)
    when (eq :less (right-order (parse-list lineleft) (parse-list lineright))) summing index))

;; part 2

(let ((sorted (sort
               (with-open-file (stream "input.txt")
                 (append '(((2)) ((6)))
                         (loop
                           for lineleft = (read-line stream nil) while (not (zerop (length lineleft)))
                           for lineright = (read-line stream nil) while (not (zerop (length lineright)))
                           for emptyline = (read-line stream nil)
                           appending (list (parse-list lineleft) (parse-list lineright)))))
               (lambda (l r) (eq :less (right-order l r))))))
  (* (1+ (position '((2)) sorted :test #'equal)) (1+ (position '((6)) sorted :test #'equal))))
