(defconstant max-a 11)
(defconstant max-b 5)

(defun full-a (state)
  (list max-a (second state)))

(defun clear-a (state)
  (list 0 (second state)))

(defun a-to-b (state)
  (let ((a (first state))
        (b (second state)))
    (list (max (- (+ a b) max-b) 0) (min (+ a b) max-b))))

(defun full-b (state)
  (list (first state) max-b))

(defun clear-b (state)
  (list (first state) 0))

(defun b-to-a (state)
  (let ((a (first state))
        (b (second state)))
    (list (min (+ a b) max-a) (max (- (+ a b) max-a) 0))))

(defconstant op-list
  (list #'full-a #'clear-a #'a-to-b #'full-b #'clear-b #'b-to-a))

(defun water-jug-dfs (goal &optional (states '((0 0))))
  (if (or (= (first (car states)) goal)
          (= (second (car states)) goal))
      (progn (format t "~a~%~%" (reverse states)) t)
      (dolist (fn op-list)
        (let ((new-state (funcall fn (car states))))
          (unless (member new-state states :test #'equal)
            "見つかった時点で探索を打ち切る"
            (when (water-jug-dfs goal (cons new-state states))
              (return t)))))))

(water-jug-dfs 7)
