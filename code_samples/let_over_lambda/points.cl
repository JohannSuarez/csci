;#! /usr/bin/gcl -f

; let over lambda:
; ----------------
;   using let blocks that return functions,
;   the function returned gives us access to the
;       "hidden" local variables in the let block
;   effectively making the function an access method
;       to private data

; the let block has an internal counter, i, initialized to 0
; the return value from the let block is a function
;     whose purpose is to increment the counter
; here we're setting upd to this returned function
(setf upd (let ((i 0)) (lambda () (incf i))))

; each time we call the function (through variable upd)
;   it increments the "hidden" counter
(format t "calling (upd): ~A~%" (funcall upd))
(format t "    and again: ~A~%" (funcall upd))
(format t "    and again: ~A~%" (funcall upd))
(format t "    and again: ~A~%" (funcall upd))


; applying let over lambda to create and manipulate objects
; ---------------------------------------------------------
; the pointBuilder initializes and x,y point at 0,0
;     and return a function that allows the user to
;     move the point around the x,y plane
(defun pointBuilder ()
   (let ; initialize the point's location
        ((x 0) (y 0))
      ; the lambda function allows the user to supply commands to:
      ;     get the point's current coordinates
      ;     move the point a certain amount in the x,y plane
      ;     jump the point to specific new coordinates
      ;     supply a command to return the current coordinates,
      (lambda (command &optional (xval 0) (yval 0))
          (cond
             ((equalp command 'move) 
               (progn (setf x (+ x xval)) (setf y (+ y yval)) (list x y)))
             ((equalp command 'jump) 
               (progn (setf x xval) (setf y yval) (list x y)))
             ((equalp command 'get) 
               (list x y))
             (t (progn (format t "Invalid command ~A~%" command) (list x y)))
           ))))


; try out the point manipulator on a couple of points
(format t "~%Working on pointBuilder example~%")
(setf p1 (pointBuilder))
(setf p2 (pointBuilder))
(format t "p1 current location: ~A~%" (funcall p1 'get))
(format t "p2 current location: ~A~%" (funcall p2 'get))
(format t "p1 new location: ~A~%" (funcall p1 'move 3 10))
(format t "p1 new location: ~A~%" (funcall p1 'move 2 -1))
(format t "p2 new location: ~A~%" (funcall p2 'jump -3 4))

; --- Why it works ---
; lisp actually allocates lists on the heap,
;    list variables and parameters are really just references into the heap
; lisp keeps track of whether lists are being referenced or not,
;    and doesn't reclaim their heap space until they are no longer referenced by anything
; parameter lists and local variable lists (in let blocks)
;    are themselves just lists, referenced from the stack for the active function
; the lambda functions above reference the lists of parameters/local variables,
;    hence even when the "constructor" function ends, the heap space for
;    those lists is not released, effectively giving the lambda functions
;    sole access to a private chunk of the heap
; (when the lambda functions go out of scope the heap space is reclaimed)