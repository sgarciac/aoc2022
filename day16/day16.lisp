(ql:quickload "graph")
(ql:quickload "cl-ppcre")

(defstruct world valves rates paths)
(defstruct situation open-valves minute current-valve)

(defun create-best-known () (make-hash-table))

(defun situation-key (situation)
  "Gets a canonical key to store a situation in a hashmap"
  (let ((sorted-valves (sort (mapcar #'symbol-name (situation-open-valves situation)) #'string>)))
    (intern (format nil "~{~a~}~a~a"
                    sorted-valves
                    (situation-minute situation)
                    (symbol-name (situation-current-valve situation))))))


(defun situation-best (situation best-known) (gethash (situation-key situation) best-known))

(defun theres-equal-or-better (situation released best-known)
  "did we already visited a situation with the same number of open valves, in the same position,
in less or equal time?"
  (loop for i from (situation-minute situation) downto 0
        do (let ((bk (situation-best
                      (make-situation :open-valves (situation-open-valves situation)
                                      :current-valve (situation-current-valve situation)
                                      :minute i) best-known)))
             (when (and bk (>= bk released) ) (return-from theres-equal-or-better t)))))

(defun update-best-known (best-known situation released)
  (let ((current-best (situation-best
                       situation
                       best-known)))
    (when (or (not current-best) (> released current-best))
      (setf (gethash (situation-key situation) best-known) released))))

(defun create-empty-world () (make-world :valves '() :rates (make-hash-table) :paths (make-instance 'graph::graph)))

(defun read-input (filename)
  (with-open-file (stream filename)
    (loop
      with world = (create-empty-world)
      for line = (read-line stream nil) while (not (zerop (length line)))
      for parts = (cl-ppcre::split " " line)
      collecting parts
      do (let* ((valve (read-from-string (nth 1 parts)))
                (rate (read-from-string (cadr (cl-ppcre::split "[;=]" (nth 4 parts)))))
                (destinations (mapcar (lambda (part) (read-from-string (remove #\, part))) (subseq parts 9))))
           (push valve (world-valves world))
           (setf (gethash valve (world-rates world)) rate)
           (loop for destination in destinations do (graph::add-edge (world-paths world) (list valve destination))))
         finally (return world))))

(defun gas-round (world valves)
  (loop for valve in valves summing (gethash valve (world-rates world))))

(defun next-actions (world current-valve open-valves)
  (let ((edges (mapcar (lambda (edge) (if (eq (car edge) current-valve) (cadr edge) (car edge))) (graph::node-edges (world-paths world) current-valve))))
    (if (or (find current-valve open-valves) (zerop (gethash current-valve (world-rates world))))
        edges
        (cons :OPEN edges))))

(defun get-best (world
                 current-valve
                 minutes
                 initially-released
                 open-valves
                 max-actions
                 best-known
                 )
  "Returns the best path and the corresponding released gas for a world,
starting at _current-valve_, previous released gas
_initially_released_, a list of _open-valves_ and a maximum number of actions _max-actions_.
the number of _minutes_ is equal to the length of previous-actions.

_known_best_ contains a global hashtable mapping a (valves, minute) and the greatest released gas known
for that pair. If the current situation has a best greater released gas than previous-released,
the function returns :DEAD_END"

  ;; if time is up, add a new round of gas and return no actions and a round of released gas
  (when (= minutes max-actions)
    ;(print "end")
    (return-from get-best (values nil (gas-round world open-valves))))

  ;; REDUCE OPTIONS SECTION
  ;; If we have already been in this situation is the same or equal time, stop visiting since we
  ;; already know something better.
  (let ((dead-end (theres-equal-or-better (make-situation :current-valve current-valve :open-valves open-valves :minute minutes) initially-released best-known)))
    (update-best-known best-known (make-situation :current-valve current-valve :open-valves open-valves :minute minutes) initially-released)
    (when dead-end
      (return-from get-best :DEAD_END)))

  (let ((next-actions (next-actions world current-valve open-valves)))
    (loop
      with next-best-actions = :DEAD_END
      with next-best-released = -1
      with next-best-action = nil
      for action in next-actions
      do (multiple-value-bind (actions released)
             (get-best
              world
              (if (eq action :OPEN) current-valve action)
              (1+ minutes)
              ;; notive we know already that if :OPEN is in the list, the valve is not yet open
              ;; we don't need to use adjoin.
              (+ initially-released (gas-round world (if (eq action :OPEN) (cons current-valve open-valves) open-valves)))
              (if (eq action :OPEN) (cons current-valve open-valves) open-valves)
              max-actions
              best-known)
           ;(format t "exploring action: from ~A  (~A) ~A ~A ~A~%" current-valve action released next-best-released open-valves)
           (when (and (not (eq actions :DEAD_END)) (> released next-best-released))
             (setf next-best-action action)
             (setf next-best-actions actions)
             (setf next-best-released released)))
      finally (progn
                (return (if (not (eq next-best-actions :DEAD_END))
                                 (values (cons next-best-action next-best-actions)
                                         (+ next-best-released (gas-round
                                                                world
                                                                open-valves)))
                                 :DEAD_END))))))

(let ((world (read-input "input.txt")))
  (get-best world 'AA 0 0 '() 29 (create-best-known)))
