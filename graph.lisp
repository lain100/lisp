(defvar *ADJACENT*
  '((A B C)
    (B A C D)
    (C A B E)
    (D B E F)
    (E C D G)
    (F D)
    (G E)))

(dolist (node '(a b c d e f g))
  (print (cdr (assoc node *ADJACENT*))))

(defun depth-first-search (fn item path)
  (if (eql item (car path))
      (funcall fn (reverse path)))
  (dolist (x (cdr (assoc (car path) *ADJACENT*)))
    (unless (member x path)
      (depth-first-search fn item (cons x path)))))

(depth-first-search #'print 'g '(a))
