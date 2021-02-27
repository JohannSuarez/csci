
(defmacro Q2 (f a &rest b)
    (if (null b) `(,f ,a)
        `(,f ,a (Q2 ,f ,@b)))) 


