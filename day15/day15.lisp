(ql:quickload :cl-ppcre)

(defvar *entries*)
(defvar *objects* )

(defun load-input ()
  (setf *objects* '())
  (setf *entries* (with-open-file (stream "input.txt")
                    (loop
                      for line = (read-line stream nil) while (not (zerop (length line)))
                      for sensor = (let ((parts (mapcar #'read-from-string (remove "" (cl-ppcre::split "[a-zA-Z =:]+" line) :test #'string=))))
                                    (cons (cons (first parts) (second parts))
                                          (cons (third parts) (fourth parts))))
                      do (progn
                           (setf *objects* (adjoin (car sensor) *objects* :test #'equal))
                           (setf *objects* (adjoin (cdr sensor) *objects* :test #'equal))
                           )
                      collecting sensor))))

(defun x (p)(car p))
(defun y (p) (cdr p))
(defun s (sensor) (car sensor))
(defun b (sensor) (cdr sensor))

(defun manhattan (p1 p2)
  (+ (abs (- (x p1) (x p2)))
     (abs (- (y p1) (y p2)))))

(defun sensor-range (sensor)
  (manhattan (s sensor) (b sensor)))

(defun h-range-for-sensor-row (sensor row)
  (let ((sr (sensor-range sensor))
        (vd (abs (- row (y (s sensor))))))
    (when (<= vd sr)
      (cons (- (x (s sensor)) (- sr vd))
            (+ (x (s sensor)) (- sr vd))))))

(defun h-ranges-for-sensors-and-row (sensors row)
  (remove nil (mapcar (lambda (sensor) (h-range-for-sensor-row sensor row)) sensors)))

(h-ranges-for-sensors-and-row (load-input) 10)

(defun overlap-p (r1 r2)
  "assumes head and tail and ranges are in order"
  (or
   (and (<= (car r1) (car r2)) (>= (cdr r1) (car r2)))
   (and (<= (car r1) (cdr r2)) (>= (cdr r1) (cdr r2)))
   (and (<= (car r1) (car r2)) (>= (cdr r1) (cdr r2)))
   (and (<= (car r2) (car r1)) (>= (cdr r2) (cdr r1)))))

(defun range-cover-p (r p)
  (and (>= p (car r))
       (<= p (cdr r))))

(defun merge-two-ranges (r1 r2)
       "return a single range, if merged, or nil if they can't be merged"
  (when (overlap-p r1 r2)
    (cons (min (car r1) (car r2))
          (max (cdr r1) (cdr r2)))))

(defun find-first-overlapping (ranges)
  (loop for range in ranges
        for rest = (cdr ranges) then (cdr rest)
        do (let ((ol (find-if (lambda (candidate) (overlap-p range candidate)) rest)))
             (when ol (return-from find-first-overlapping (cons range ol))))))

(defun merge-first-overlapping (ranges)
  (let ((overlapping (find-first-overlapping ranges)))
    (if overlapping
        (cons (merge-two-ranges (car overlapping) (cdr overlapping)) (remove (cdr overlapping) (remove (car overlapping) ranges :test #'equal) :test #'equal))
        ranges)))

(defun merge-all-overlapping (ranges)
  (loop with result = ranges
        do (progn
             (let ((s (length result)))
               (setf result (merge-first-overlapping result))
               (when (= (length result) s)
                 (return-from merge-all-overlapping result))))))

(defun part1 (row)
  (load-input)
  (let* ((ranges (merge-all-overlapping (h-ranges-for-sensors-and-row *entries* row)))
         (occupied (loop for object in (remove-if (lambda (object) (not (= (cdr object) row))) *objects*)
                         when (find-if (lambda (range) (range-cover-p range (car object))) ranges)
                           collect object))
         (covered (loop for range in ranges summing (1+ (- (cdr range) (car range))))))
    (print ranges)
    (print occupied)
    (- covered (length occupied))))

;(part1 2000000)

;; part2
(defun part2 ()
  (load-input)
  (loop for row from 0 upto 4000000
        for ranges = (merge-all-overlapping (h-ranges-for-sensors-and-row *entries* row))
        while (= (length ranges) 1)
        finally (return (cons row ranges))))

(part2)
