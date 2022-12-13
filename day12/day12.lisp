(ql:quickload "graph")

(defvar *graph* nil)
(defvar *height* 1)
(defvar *width* 1)
(defvar *start* 0)
(defvar *end* 0)
(defvar *lowest* '())

(defun node-identifier (line col)
  (+ col (* line *width*)))

(defun pos-from-identifier (identifier)
  (cons (floor (/ identifier *width*))
        (mod identifier *width*)))

(defun height-value (ch)
  (let ((is-start (char= ch #\S))
        (is-end (char= ch #\E)))
    (char-code (cond (is-start #\a) (is-end #\z) (t ch)))))



(defun load-input (file)
  (setf *graph* (make-instance 'graph::digraph))
  (setf *lowest* '())
  (with-open-file (stream file)
    (let ((lines (loop for line = (read-line stream nil) while (not (zerop (length line))) collecting line)))
      (setf *width* (length (first lines)))
      (setf *height* (length lines))
      (loop for line in lines
            for l from 0 upto (1- *height*)
            do (loop for col from 0 upto (1- *width*)
                     do (let* ((ch (char line col))
                               (id (node-identifier l col))
                               (is-start (char= ch #\S))
                               (is-end (char= ch #\E))
                               (value (height-value ch)))
                          (when (= value (char-code #\a))
                            (push id *lowest*))
                          (when is-start
                            (setf *start* (node-identifier l col)))
                          (when is-end
                            (setf *end* (node-identifier l col)))
                          (graph::add-node *graph* (node-identifier l col))
                          ;; up
                          (when (and (not (zerop l))
                                     (not (> (height-value (char (nth (1- l) lines) col)) (1+ value))))
                            (graph::add-edge *graph* (list id (node-identifier (1- l) col))))
                          ;; down
                          (when (and (not (= l (1- *height*)))
                                     (not (> (height-value (char (nth (1+ l) lines) col)) (1+ value))))
                            (graph::add-edge *graph* (list id (node-identifier (1+ l) col))))
                          ;; left
                          (when (and (not (zerop col))
                                     (not (> (height-value (char line (1- col))) (1+ value))))
                            (graph::add-edge *graph* (list id (node-identifier l (1- col)))))
                          (when (and (not (= col (1- *width*)))
                                     (not (> (height-value (char line (1+ col))) (1+ value))))
                            (graph::add-edge *graph* (list id (node-identifier l (1+ col)))))))))))

(load-input "input.txt")
(length (graph::shortest-path *graph* *start* *end*))
(apply #'min (remove 0 (mapcar (lambda (p) (length (graph::shortest-path *graph* p *end*))) *lowest*)))
