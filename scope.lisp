(defvar *foo* 3 "value")

(defparameter pear 101 "teto")
(documentation 'pear 'variable)
pear

(defconstant apple 29)
apple

(defun cat-file (&rest args)
  (loop as file in args do
        (with-open-file
            (input file :DIRECTION :INPUT
                   :IF-DOES-NOT-EXIST :CREATE)
          (loop as buff = (read-line input nil)
                while buff do (write-line buff)))))

(with-open-file
    (*STANDARD-OUTPUT* "teto.pear" :DIRECTION :OUTPUT
                       :IF-DOES-NOT-EXIST :CREATE
                       :IF-EXISTS :APPEND)
  (cat-file "pear1" "pear2"))

(cat-file "teto.pear")

(defun add-n (x) (+ n x))

(defun pear (n lst)
  (declare (special n))
  (mapcar #'add-n lst))

(pear 10 `(3 3 4))

(block aaaa 3 (return-from aaaa 4) 5)
(block nil (return 1))


(defun search-matrix (func a max-x max-y)
  (loop as x below max-x do
        (loop as y below max-y
              as val = (aref a x y)
              if (funcall func val) do
              (return-from search-matrix val))))

(search-matrix #'oddp #2A((2 4) (3 5)) 2 2)

(defun foo (xs)
  (mapcar (lambda (x)
            (if (minusp x)
                (return-from foo) (* x x))) xs))

(foo '(3 3 4 5 5 6))

(defvar *ADJACENT*
  '((A B C)
    (B A C D)
    (C A B E)
    (D B E F)
    (E C D G)
    (f d)
    (g e)))

(defun id-search (start goal)
  (labels
      ((dfs (limit goal path)
         (if (= (length path) limit)
             (when (eq (car path) goal)
               (return-from id-search (reverse path)))
             (loop as x in (cdr (assoc (car path) *ADJACENT*))
                   unless (member x path) do
                   (dfs limit goal (cons x path))))))
    (loop as i from 2 to 7 do
          (format t "--- ~d ---~%" i)
          (dfs i goal (list start)))))

(id-search 'a 'f)

(defun sum-list (failure xs)
  (loop as x in xs sum x
        if (minusp x) do (funcall failure)))

(defun sum-matrix (ys)
  (loop as xs in ys
        sum (sum-list (lambda () (return-from sum-matrix -1)) xs)))

(sum-matrix '((1 2 3 3 4) (3 3 3 3) (-4 4 4 4)))

(defun fact (x &aux (result 1) (num 1))
  (tagbody
    next-loop
    (if (> num x)
        (return-from fact result))
    (setf result (* result num))
    (incf num)
    (go next-loop)))

(fact 10)

(defun foo () (bar1) (bar2) (bar3))
(defun bar1 () (print "call bar1"))
(defun bar2 () (throw 'error "throw error at bar2"))

(catch 'error (foo))

(catch 'error 
       (unwind-protect
           (foo)
         (print "cleanup1")
         (print "cleanup2")))

(block nil
  (unwind-protect
      (progn
        (print "first")
        (return "return")
        (print "second"))
    (print "cleanup")))
