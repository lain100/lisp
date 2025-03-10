(defstruct (queue
             (:CONSTRUCTOR create-queue
              (size &aux (buffer (make-array size)))))
  (front 0) (rear 0) (count 0) size buffer)

(defun enqueue (item q)
  (unless (fullp q)
    (setf (aref (queue-buffer q) (queue-rear q)) item)
    (incf (queue-rear q))
    (incf (queue-count q))
    (if (= (queue-size q) (queue-rear q))
        (setf (queue-rear q) 0))
    t))

(defun dequeue (q)
  (unless (emptyp q)
    (prog1 (front q)
      (incf (queue-front q))
      (decf (queue-count q))
      (if (= (queue-size q) (queue-front q))
          (setf (queue-front q) 0)))))

(defun fullp (q)
  (= (queue-count q) (queue-size q)))

(defun emptyp (q)
  (zerop (queue-count q)))

(defun front (q)
  (unless (emptyp q)
    (aref (queue-buffer q) (queue-front q))))

(defun clear (q)
  (setf (queue-front q) 0
        (queue-rear  q) 0
        (queue-count q) 0))

(defvar que (create-queue 10))

(dotimes (x 7 que) (print (enqueue x que)))
(loop repeat 10 do (print (dequeue que)))

(clear que)
