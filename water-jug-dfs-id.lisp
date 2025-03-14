(defconstant max-a 8)
(defconstant max-b 5)

(defun full-a (state)
  (list max-a (second state)))

(defun clear-a (state)
  (list 0 (second state)))

(defun a-to-b (state &aux
                     (a (first state))
                     (b (second state)))
  (list (max (- (+ a b) max-b) 0) (min (+ a b) max-b)))

(defun full-b (state)
  (list (first state) max-b))

(defun clear-b (state)
  (list (first state) 0))

(defun b-to-a (state &aux
                     (a (first state))
                     (b (second state)))
  (list (min (+ a b) max-a) (max (- (+ a b) max-a) 0)))

(defconstant op-list
  (list #'full-a #'clear-a #'a-to-b #'full-b #'clear-b #'b-to-a))

(defstruct queue (front nil) (rear nil))

(defun enqueue (q item &aux (new-cell (list item)))
  (if (emptyp q)
      (setf (queue-front q) new-cell)
      (setf (cdr (queue-rear q)) new-cell))
  (setf (queue-rear q) new-cell))

(defun emptyp (q)
  (null (queue-front q)))

(defun dequeue (q)
  (unless (emptyp q)
    (prog1 (car (queue-front q))
      (setf (queue-front q) (cdr (queue-front q)))
      (if (emptyp q) 
          (setf (queue-rear q) nil)))))

(defun water-jug-dfs-id (limit goal states)
  (if (= (length states) limit)
      (when (or (= goal (first (car states)))
                (= goal (second (car states))))
        (print (reverse states))
        t)
      (loop as fn in op-list
            as new-state = (funcall fn (car states))
            unless (member new-state states :test #'equal)
            when (water-jug-dfs-id limit goal (cons new-state states))
            return t)))

(defun water-jug-id (goal)
  (loop as depth = 1 then (1+ depth)
        do (format t "-~d-~%" depth)
        if (water-jug-dfs-id depth goal '((0 0)))
        return t))

(loop as x below 8
      do (format t "~a~%" (water-jug-id x)))
