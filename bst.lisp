(defstruct node data left right)

(defun search-node (node x)
  (loop
    while node
    if (= x (node-data node)) return t
    do (if (< x (node-data node))
           (setf node (node-left node))
           (setf node (node-right node)))))

(defun insert-node (node x)
  (cond
    ((null node)
     (return-from insert-node (make-node :data x)))
    ((< x (node-data node))
     (setf (node-left node)
           (insert-node (node-left node) x)))
    ((> x (node-data node))
     (setf (node-right node)
           (insert-node (node-right node) x))))
  node)

(defvar root nil)
(dotimes (x 5 root) (setf root (insert-node root x)))
(loop as x from 0 downto -5 do (setf root (insert-node root x)))

(loop as x in '(-3 3 4 10 -5)
      do (print (search-node root x)))

(defun search-min-node (node)
  (loop
    unless (node-left node)
    return (node-data node)
    do (setf node (node-left node))))

(defun delete-min-node (node)
  (cond
    ((null (node-left node))
     (node-right node))
    (t
     (setf (node-left node)
           (delete-min-node (node-left node)))
     node)))

(search-min-node root)
(setf root (delete-min-node root))

(defun delete-node (node x)
  (when node
    (cond
      ((= x (node-data node))
       "nodeを返すべき"
       (cond
         ((null (node-left node))
          (return-from delete-node (node-right node)))
         ((null (node-right node))
          (return-from delete-node (node-left node)))
         (t
          "node-rightがnilでないことが保障される"
          (setf (node-data node)
                (search-min-node (node-right node))
                (node-right node)
                (delete-min-node (node-right node))))))
      ((< x (node-data node))
       (setf (node-left node)
             (delete-node (node-left node) x)))
      (t
       (setf (node-right node)
             (delete-node (node-right node) x)))))
  node)

(setf root (delete-node root 0))

(defun traverse-node (func node)
  (when node
    (traverse-node func (node-left node))
    (funcall func (node-data node))
    (traverse-node func (node-right node))))

(traverse-node #'print root)

(defun search-max-node (node)
  (if (null (node-right node))
      (node-data node)
      (search-max-node (node-right node))))

(defun delete-max-node (node)
  (cond
    ((null (node-right node))
     (node-left node))
    (t
     (setf (node-right node)
           (delete-max-node (node-right node)))
     node)))

(defstruct binary-tree root)

(defun search-binary-tree (tree x)
  (search-node (binary-tree-root tree) x))

(defun search-max-binary-tree (tree)
  (when (binary-tree-root tree)
    (search-max-node (binary-tree-root tree))))

(defun search-min-binary-tree (tree)
  (when (binary-tree-root tree)
    (search-min-node (binary-tree-root tree))))

(defun insert-binary-tree (tree x)
  (setf (binary-tree-root tree)
        (insert-node (binary-tree-root tree) x)))

(defun delete-binary-tree (tree x)
  (setf (binary-tree-root tree)
        (delete-node (binary-tree-root tree) x)))

(defun delete-max-binary-tree (tree)
  (when (binary-tree-root tree)
    (setf (binary-tree-root tree)
          (delete-max-node (binary-tree-root tree)))))

(defun delete-min-binary-tree (tree)
  (when (binary-tree-root tree)
    (setf (binary-tree-root tree)
          (delete-min-node (binary-tree-root tree)))))

(defun traverse-binary-tree (func tree)
  (traverse-node func (binary-tree-root tree)))

(defun print-binary-tree (tree)
  (traverse-binary-tree (lambda (x) (format t "~d " x)) tree)
  (terpri))

(defun make-random-data (n)
  (loop repeat n
    with buff do (push (random 10000) buff)
    finally (return buff)))

(make-random-data 10)

(defun test ()
  (let ((tree (make-binary-tree))
        (data (make-random-data 10)))
    (loop
      as x in data do
      (format t "insert ~4d: " x)
      (insert-binary-tree tree x)
      (print-binary-tree tree))
    (loop
      as x in data do
      (format t "~4d => ~a~%" x (search-binary-tree tree x))
      (format t "~4d => ~a~%" (1+ x) (search-binary-tree tree (1+ x))))
    (format t "max = ~d~%" (search-max-binary-tree tree))
    (format t "min = ~d~%" (search-min-binary-tree tree))
    (format t "delete max: ")
    (delete-max-binary-tree tree)
    (print-binary-tree tree)
    (format t "delete min: ")
    (delete-min-binary-tree tree)
    (print-binary-tree tree)
    (loop
      as x in data do
      (delete-binary-tree tree x)
      (format t "delete ~4d: " x)
      (print-binary-tree tree))))

(test)

;;;
;;; binarytree.lisp : 二分探索木
;;;
;;;                   Copyright (C) 2020 Makoto Hiroi
;;;
