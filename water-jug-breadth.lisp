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

(defstruct queue (rear nil))

(setf *print-circle* t)

(defun enqueue (q item &aux (new-rear (list item)))
  (if (emptyp q)
      (setf (cdr new-rear) new-rear)
      (progn
        (setf (cdr new-rear) (cdr (queue-rear q)))
        (setf (cdr (queue-rear q)) new-rear)))
  (setf (queue-rear q) new-rear))

(defun emptyp (q)
  (null (queue-rear q)))

(defun dequeue (q)
  (unless (emptyp q)
    (let ((front (cdr (queue-rear q))))
      (if (eq front (cdr front))
          (setf (queue-rear q) nil)
          (setf (cdr (queue-rear q)) (cdr front)))
      (car front))))

(defun water-jug (goal &aux (que (make-queue)))
  (enqueue que '((0 0)))
  (loop until (emptyp que)
        as states = (dequeue que)
        if (or (= (first (car states)) goal)
               (= (second (car states)) goal))
        do (print (reverse states)) (terpri) (return)
        do (loop as fn in op-list
                 as new-state = (funcall fn (car states))
                 unless (member new-state states :test #'equal)
                 do (enqueue que (cons new-state states)))))

(loop as x from 1 to 8 do (water-jug x))
