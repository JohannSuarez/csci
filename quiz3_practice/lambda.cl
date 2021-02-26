
;#! /usr/bin/gcl -f

(format t "Lambda exercises!~%")

(defvar f (lambda (x) (if (numberp x) (- x))))

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

(format t "~% Working on pointBuilder example ~%")

(setf p1 (pointBuilder))
(setf p2 (pointBuilder))