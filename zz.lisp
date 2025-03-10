(setf *PRINT-CIRCLE* t)

(defstruct queue (rear nil))

(defvar bar (make-queue))

(defun emptyp (q)
  (null (queue-rear q)))

(defun enqueue (item q)
  (let ((new-rear (list item)))
    (if (emptyp q)
        (setf (cdr new-rear) new-rear)
        (setf (cdr new-rear) (cdr (queue-rear q))
              (cdr (queue-rear q)) new-rear))
    (setf (queue-rear q) new-rear)))

(defun dequeue (q)
  (unless (emptyp q)
    (let ((front (cdr (queue-rear q))))
      (if (eq front (queue-rear q))
          (setf (queue-rear q) nil)
          (setf (cdr (queue-rear q)) (cdr front)))
      (car front))))


