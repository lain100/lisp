(defstruct (queue (:CONSTRUCTOR make-queue
                   (&optional (size 20)
                    &aux (buffer (make-array size)))))
  (front 0) (rear 0) (count 0) size buffer)

(defun enqueue (q item)
  (unless (fullp q)
    (setf (aref (queue-buffer q) (queue-rear q)) item)
    (incf (queue-rear q))
    (incf (queue-count q))
    (if (= (queue-rear q) (queue-size q))
        (setf (queue-rear q) 0))
    t))

(defun fullp (q)
  (= (queue-count q) (queue-size q)))

(defun dequeue (q)
  (unless (emptyp q)
    (prog1 (aref (queue-buffer q) (queue-front q))
      (decf (queue-count q))
      (incf (queue-front q))
      (if (= (queue-front q) (queue-size q))
          (setf (queue-front q) 0)))))

(defun emptyp (q)
  (zerop (queue-count q)))

(defvar *ADJACENT*
  '((a b c)
    (b a c d)
    (c a b e)
    (d b e f)
    (e c d g)
    (f d)
    (g e)))

(defun dfs (limit goal path)
  (if (= (length path) limit)
      (if (eql (car path) goal)
          (print (reverse path)))
      (dolist (x (cdr (assoc (car path) *ADJACENT*)))
        (unless (member x path)
          (dfs limit goal (cons x path))))))

(defun id-search (start goal)
  (loop as depth from 1 to 7 do
        (dfs depth goal (list start))))

(id-search 'a 'g)
