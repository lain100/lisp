(defun insert-sort (buff)
  (loop
    as i from 1 below (length buff) do
    (loop
      as j from i downto 1
      until (<= (aref buff (1- j)) (aref buff j))
      do (psetf (aref buff (1- j)) (aref buff j)
                (aref buff j) (aref buff (1- j))))
    finally
    (return-from insert-sort buff)))

(defun qsort (buff left light)
  (loop
    initially
    (unless (< left light) (return))
    with pivot = (aref buff (floor (+ left light) 2))
    as i from left to light
    as j from light downto i do
    (loop while (< (aref buff i) pivot) do (incf i))
    (loop while (> (aref buff j) pivot) do (decf j))
    while (< i j) do
    (psetf (aref buff i) (aref buff j)
           (aref buff j) (aref buff i)) 
    finally
    (qsort buff left (1- i))
    (qsort buff (1+ j) light)))

(defun quick-sort (buff)
  (qsort buff 0 (1- (length buff)))
  buff)

"4uあたり質量減少量u 4p -> 2e + He"
0.0267

"4pの質量u"
4.0292

"減少量の割合"
6.62662e-3
(format nil "~,3f%" (* 6.62662e-3 100))

"1kgの水素が核融合したときに放出するエネルギー量"
(format nil "~,3gJ" (* 6.62662e-3 9e16))
(format nil "~,3gJ" (* 0.0267e-3 (/ 1.0073) 250 9e16))

(defun make-random-data (n)
  (loop with buff = (make-array n)
        as i from 0 below n do
        (setf (aref buff i) (random 1000000))
        finally (return buff)))

(defun make-sort-data (n)
  (loop with buff = (make-array n)
        as i from 0 below n do
        (setf (aref buff i) i)
        finally (return buff)))

(defun make-rev-data (n)
  (reverse (make-sort-data n)))

(defun make-mountain-data (n)
  (loop with buff = (make-array n)
        as i from 0 below (floor n 2) do
        (setf (aref buff i) i
              (aref buff (- n i 1)) i)
        finally (return buff)))

(mapcar (lambda (maker)
          (time (quick-sort (funcall maker 10000))))
        '(make-random-data
           make-sort-data
           make-rev-data
           make-mountain-data))

(defun select-pivot (buff low high)
  (let ((a (aref buff low))
        (b (aref buff (floor (+ low high) 2)))
        (c (aref buff high)))
    (if (> (max a b) c)
        (if (< (min a b) c)
            c
            (min a b))
        (max a b))))

(defconstant qsort-limit 10)

(defun quick-sort-fast (buff)
  (loop
    with low = 0
    with high = (1- (length buff))
    with stack
    if (> (- high low) qsort-limit) do
    (loop
      with p = (select-pivot buff low high)
      as i from low to high
      as j from high downto i do
      (loop while (< (aref buff i) p) do (incf i))
      (loop while (> (aref buff j) p) do (decf j))
      while (< i j) do
      (psetf (aref buff i) (aref buff j)
             (aref buff j) (aref buff i))
      finally
      (cond
        ((> (- i low) (- high j))
         (push (list low (1- i)) stack)
         (setf low (1+ j)))
        (t
         (push (list (1+ j) high) stack)
         (setf high (1- i)))))
    else do
    (unless stack (insert-sort buff) (return buff))
    (multiple-value-setq
      (low high)
      (apply #'values (pop stack)))))

(defun quicksort-fast (buff &aux (low 0) (high (1- (length buff))))
  (let (stack)
    (loop
      (cond
        ((<= (- high low) qsort-limit)
         (unless stack (return))
         (setq low  (first (car stack))
               high (second (car stack)))
         (pop stack))
        (t
         (let ((p (select-pivot buff low high))
               (i low)
               (j high))
           (loop
             (loop (if (<= p (aref buff i)) (return)) (incf i))
             (loop (if (>= p (aref buff j)) (return)) (decf j))
             (if (>= i j) (return))
             (psetf (aref buff i) (aref buff j)
                    (aref buff j) (aref buff i))
             (incf i)
             (decf j))
           (cond
             ((> (- i low) (- high j))
              (push (list low (1- i)) stack)
              (setq low (1+ j)))
             (t
              (push (list (1+ j) high) stack)
              (setq high (1- i))))))))))
    
(time (quick-sort-fast (make-mountain-data 80000)))
(time (quicksort-fast (make-mountain-data 100)))

(format nil "~,2g MeV" (* 8.79 56))
(format nil "~,1g MeV" (* (- (+ 1.17 0.783) 1.78) 1e3))

(let ((dM (* (- (* 4 1.00783) (+ 4.0026 (* 2 0.00055))) 1.66e-27)))
  "4個の水素原子の核融合による質量欠損および放出エネルギー"
  (format t "~,2g kg~%~,2g J~%" dm (* dm 9e+16))

  "1個あたり放出するエネルギーに対する毎秒の全放出エネルギー 水素原子の個数"
  (format t "~,2g /s~%" (/ 3.87d+26 (* 1/4 dm 9d+16)))

  "1秒あたり太陽の質量減少量"
  (format t "~,2g kg/s~%" (/ 3.87e+26 9e+16))

  "1個あたり放出するエネルギーに対する日本の年間発電量に相当する水素原子の質量"
  (format t "~,2g kg" (* 1.00783 1.66e-27 (/ (* 1e+12 3600e+3) (* 1/4 dm 9e+16)))))

(defun binary-search (item buff)
  (loop
    with low = 0
    with high = (1- (length buff))
    as mid = (floor (+ low high) 2)
    until (> low high)
    if (= item (aref buff mid))
    return t
    else if (< item (aref buff mid)) do
    (setf high (1- mid))
    else do
    (setf low (1+ mid))))

(loop
  as x in '(1 -1 2 -2 3 -3 3 4 5 6 7 8) do
  (print (binary-search x (make-sort-data 6))))

(defvar pear (make-hash-table :test #'equalp)) ;case-insensitive

(type-of pear)
(setf (gethash "abc" pear) 100)
(setf (gethash "def" pear) 200)
(setf (gethash "ghi" pear) 300)
(gethash "abc" pear)
(gethash "ABC" pear)

(remhash "aaa" pear)
(remhash "abc" pear)
(clrhash pear)
(hash-table-count pear)

(maphash (lambda (key val) (format t "key = ~a, val = ~a~%" key val)) pear)
(with-open-file
    (*STANDARD-OUTPUT* "maphash-doc"
                       :direction :OUTPUT
                       :IF-EXISTS :append
                       :IF-DOES-NOT-EXIST :CREATE)
    (write-line (documentation 'maphash 'function)))

(with-hash-table-iterator (next pear)
  (loop
    (multiple-value-bind
        (more key value)
        (next)
      (unless more (return))
      (format t key value))))
(setf (gethash "~{~a ~}~%" pear) (list "hello" "lain"))

(defun make-point ()
  (list (random 100) (random 100) (random 100)))

(defun make-data (n &optional zs)
  (if (zerop n)
      zs
      (let ((xs (make-point)))
        (if (member xs zs :test #'equal)
            (make-data n zs)
            (make-data (1- n) (cons xs zs))))))

(time (make-data 10000)) ;member is linear search

(defun make-data-fast (n)
  (loop
    with zs = (make-hash-table :test #'equal)
    as xs = (make-point)
    if (zerop n) return zs
    unless (gethash xs zs)
    do (setf (gethash xs zs) t) (decf n)))

(time (make-data-fast 40000))
