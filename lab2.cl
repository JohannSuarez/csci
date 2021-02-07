; What's your plan?
; First, make f take three optional parameters in addition to N.

; Essentially, you're going to use the optional parameters
; as accumulator and throw in default values.

; Check each parameter, N and the three others must be ints

; Follow the definition of tail recursion from the slides.
; " A function is tail-recursive iff the result of each recursive
;   call is immediately returned, i.e. not processed before
;   returning"

; fhelper has the procedure, just find a way to integrate it into f.

(defun f (N &optional (next 3) (older 1) (recent 1)) 
   (cond
      ((not (integerp N)) nil) ; You will keep these first two lines (the checks)
      ((< N next) recent)      ; If N is less than three, it just handles the base case, return one (But the first number is actually 0?)
      (t (f N (+ next 1) recent (+ older recent)))
   )
)

(defun fhelper (N next older recent)
   (cond
      ((< N next) recent)
      (t (fhelper N (+ next 1) recent (+ older recent)))))

(defun gt (M &rest R)
   ; If R is inside a list, we set R into the first of its element
   ; eliminating the nested list issue.
   (if (listp (car R)) (setf R (car R)))   
   
   (format t "~A~%" R)
   (cond 
      ((not (realp M)) nil)
      ((null R) nil)
      ; If the head isn't a real number, we recurse without it. 
      ((not (realp (car R))) (gt M (cdr R))) 
      ; If the head of R is greater than M, connect the head of R to the rest of 
      ; whatever the next recursion of gt will return.
      ((> (car R) M) (cons (car R) (gt M (cdr R))) ) 
      ; If the head of R is not greater than M
      ; Then we forget about it and recurse with the rest of R.
      (t (gt M (cdr R)))  

  )
)

(defun isTree (tree)
; is meant to take a single parameter
   (cond
      ((null tree) t)
      ((and (listp (car tree)) (integerp (car (cdr tree))) (listp (cdr (cdr tree)) )) t)

      (t "hello")
   )
)

(defun sumTree (tree)
   )

