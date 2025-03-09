(defstruct (foo
             (:CONSTRUCTOR build-foo
                (&optional (x 1)
                      &aux (a (* 10 x))
                           (b (* 20 x))))) a b)

(build-foo 100)
(build-foo)

