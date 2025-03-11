(defvar *ADJACENT*
  '((a b c)
    (b a c d)
    (c a b e)
    (d b e f)
    (e c d g)
    (f d)
    (g e)))

(defstruct
  (queue (:CONSTRUCTOR make-queue
          (&optional (size 20)
                &aux (buffer (make-array size)))))
  (front 0) (rear 0) (count 0) size buffer)

(defun enqueue (item q)
  (unless (fullp q)
    (setf (aref (queue-buffer q) (queue-rear q)) item)
    (incf (queue-count q))
    (incf (queue-rear q))
    (if (= (queue-rear q) (queue-size q))
        (setf (queue-rear q) 0))
    t))

(defun dequeue (q)
  (unless (emptyp q)
    (prog1
      (aref (queue-buffer q) (queue-front q))
      (decf (queue-count q))
      (incf (queue-front q))
      (if (= (queue-front q) (queue-size q))
          (setf (queue-front q) 0)))))

(defun fullp (q)
  (= (queue-count q) (queue-size q)))

(defun emptyp (q)
  (zerop (queue-count q)))

(defun breadth-first-search (start goal)
  (let ((q (make-queue)))
    (enqueue (list start) q)
    (loop until (emptyp q)
          as path = (dequeue q) then (dequeue q)
          if (eql (car path) goal) do (print (reverse path))
          do (loop as x in (cdr (assoc (car path) *ADJACENT*))
                   unless (member x path)
                   do (enqueue (cons x path) q)))))

(breadth-first-search 'a 'g)
