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
      ((< N next) recent)      ; The base case, if N is less than next
      (t (f N (+ next 1) recent (+ older recent))) ; Just moved fhelper's recursing line over
   )
)

(defun fhelper (N next older recent)
   (cond
      ((< N next) recent)
      (t (fhelper N (+ next 1) recent (+ older recent)))))

(defun gt (M &rest R)
   ; If R is inside a list, we set R into the first of its element
   ; eliminating the nested list issue.
   (if (listp (car R)) (setf R (first R)))   
   
   ; (format t "~A~%" R) This is just a debugging statement.
   (cond 
      ((not (realp M)) nil) ; Nil if M isn't real
      ((null R) nil) ; nil if R is empty (we need at least one to compare with)
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
      ; I find that using first, second, and third
      ; is more intuitive than car,cdr,caadr, etc.

      ; Very clear code, just checking that the middle is int, and the right
      ; and left are lists.
      ((and (listp (first tree)) (integerp (second tree)) (listp (third tree) )) t)

      ; If it's not a valid tree, we return nil (specified in video)
      (t NIL)
   )
)

(defun sumTree (tree)

   (format t "~A~%" tree)
   (format t "called~%")

   (cond
      ; TOOK ME FOREVER TO FIGURE OUT! 
      ; 4 Hours, 2 existential crisis moments, and 5 Mazzy Star songs later...
      ; Anyways..

      ; If what we get is just an integer (that's not even in a list),
      ; it means we've recursed in enough that we can't go any deeper
      ; so we return it, because it's a leaf of the binary tree.
      ((integerp (first tree)) (first tree))

      ; If the tree's just... empty, we hand back a zero
      ; because remember we still need to do addition with
      ; whatever's expecting us when we return from this recursion.
      ((null tree) 0)
      ((not (isTree tree)) nil) ; Keep this. Written in documentation.

      ; Immediately add the middle of the tree (because int)
      ; and add whatever the left and right brings back
      ; from recursion
      (t (+ (sumTree (first tree)) (second tree) (sumTree (third tree)) ))

   )
   ; If the tree is not empty, we take the current element (middle value)
   ; and sum it together with the result of the left side and right side of tree
)

