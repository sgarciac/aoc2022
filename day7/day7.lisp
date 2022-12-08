(ql:quickload 'cl-ppcre)
(defstruct dir parent content name)
(defstruct file parent size name)

(defvar *root-dir* )
(defvar *current-dir*)

(defun entry-name (entry)
  (etypecase entry
    (dir (dir-name entry))
    (file (file-name entry))))

(defun print-entry (entry &optional (level 0))
  (etypecase entry
    (dir (print-dir entry level))
    (file (print-file entry level))))

(defun print-dir (dir &optional (level 0))
  (format T "~A/~A~%" (make-string (* 2 level) :initial-element #\ ) (dir-name dir))
  (loop for entry in (dir-content dir) do (print-entry entry (1+ level))))

(defun print-file (file &optional (level 0))
  (format T "~A/~A (~A)~%" (make-string (* 2 level) :initial-element #\ ) (file-name file) (file-size file)))

(defun find-entry (dir name)
  (find name (dir-content dir) :test (lambda (name entry)
                                       (string= (entry-name entry) name))))


(defun change-dir (arg)
  (cond ((string= arg "..") (setf *current-dir* (dir-parent *current-dir*)))
        ((string= arg "/") (setf *current-dir* *root-dir*))
        (t
         (let ((dir (find-entry *current-dir* arg)))
           (if
            dir
            (setf *current-dir* dir)
            (let ((new-dir (make-dir
                            :parent *current-dir*
                            :content '()
                            :name arg)))
              (setf (dir-content *current-dir*) (cons new-dir (dir-content *current-dir*)))
              (setf *current-dir* new-dir)))))))

(defun entry-size (entry)
  (etypecase entry
    (dir (dir-size entry))
    (file (file-size entry))))

(defun dir-size (dir)
  (loop for entry in (dir-content dir)
        summing (entry-size entry)))


(defun read-command (stream)
  (let ((line (read-line stream nil)))
    (when line
      (let* ((parts (cl-ppcre:split " " line))
             (command (second parts)))
        (if (string= "cd" command)
            (change-dir (third parts))
            (loop
              for first-char = (peek-char nil stream nil #\$) while (not (char= first-char #\$))
              for line = (read-line stream nil) while (not (zerop (length line)))
              for parts = (cl-ppcre:split " " line)
              for first-part = (car parts)
              for name = (cadr parts)
              do (let ((entry (find-entry *current-dir* name)))
                       (unless
                           entry
                         (setf (dir-content *current-dir*)
                               (cons
                                (if (string= first-part "dir")
                                    (make-dir
                                     :parent *current-dir*
                                     :content '()
                                     :name name)
                                    (make-file
                                     :parent *current-dir*
                                     :size (read-from-string first-part)
                                     :name name))
                                (dir-content *current-dir*)))))
                     (cond ((string= "dir" first-part)
                            )))))
      t)))



;; part1
(defun part1 (dir limit)
  (let ((size (dir-size dir)))
    (+ (if (<= size limit)
           size
           0)
       (loop for entry in (dir-content dir)
             summing (etypecase entry
                       (dir (part1 entry limit))
                       (file 0))))))

(with-open-file (stream "~/dev/aoc2022/day7/input.txt")
  (let* ((*root-dir* (make-dir :parent nil :content '() :name ""))
         (*current-dir* *root-dir*))
    (loop while (read-command stream))
    (print (dir-size *root-dir*))
    (print  (part1 *root-dir* 100000))
    ))

;; part2
(defun sizes (dir)
  (cons (dir-size dir) (mapcan (lambda (entry)
                                 (etypecase entry
                                   (dir (sizes entry))
                                   (file '()))
                                 ) (dir-content dir))))


(with-open-file (stream "~/dev/aoc2022/day7/input.txt")
  (let* ((*root-dir* (make-dir :parent nil :content '() :name ""))
         (*current-dir* *root-dir*))
    (loop while (read-command stream))
    (print (dir-size *root-dir*))
    (let ((needed-space-to-free
            (- (dir-size *root-dir*) 40000000)))
      (print needed-space-to-free)
      (print (apply #'min (remove-if (lambda (size) (< size needed-space-to-free))
                                 (sizes *root-dir*)))))))
