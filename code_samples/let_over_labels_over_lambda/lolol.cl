#! /usr/bin/gcl -f 

; using let-over-lambda to create points with a collection
; of internal methods, accessed through a public dispatcher
; ---------------------------------------------------------
;
; the pointBuilder initializes and x,y point at 0,0
;     and return a function that acts as a dispatcher,
;  allowing the user to move the point around the x,y plane
;     and look up its current coordinates
(defun pointBuilder (&optional (xinit 0) (yinit 0))
   (let* ( ; the point's private/hidden data
             ; declare and initialize the x,y coordinates
             (x xinit)
             (y yinit)
        )
        (labels ( ; the point's local access methods
             ; (checkpt xv yv) returns t iff xv and yv are real numbers
             (checkpt (xv yv) (and (realp xv) (realp yv)))
             ; function to alter x and y by certain amounts
             (move (xv yv) (setf x (+ x xv)) (setf y (+ y yv)))
             ; function to change x and y directly to specific values
             (jump (xv yv) (setf x xv) (setf y yv))
             ; function to return (x y) 
             (getcoords () (list x y))
              )
      ; the lambda function allows the user to supply commands to:
      ;     get the point's current coordinates
      ;     move the point a certain amount in the x,y plane
      ;     jump the point to specific new coordinates
      (lambda (command &optional (xval 0) (yval 0))
         (cond
             ; check the point for validity
             ((not (checkpt xval yval)) 
                (format t "Error: invalid x/y value(s) supplied: ~A,~A~%" xval yval))
             ; process commands for move, jump, or get
             ((equalp command 'move) (move xval yval))
             ((equalp command 'jump) (jump xval yval))
             ((equalp command 'get) (getcoords))
             ; handle invalid commands
             (t (format t "Error: invalid command supplied ~A~%" command)))
          ; always return the current (x y) coordinates of the point
          (getcoords)))))


; try out the point manipulator on a couple of points
(setf p1 (pointBuilder 50 100))
(setf p2 (pointBuilder))
(format t "p1 current location: ~A~%" (funcall p1 'get))
(format t "p2 current location: ~A~%" (funcall p2 'get))
(format t "p1 new location: ~A~%" (funcall p1 'move 3 10))
(format t "p1 new location: ~A~%" (funcall p1 'move 2 -1))
(format t "p2 new location: ~A~%" (funcall p2 'jump -3 4))

