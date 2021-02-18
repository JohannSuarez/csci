#! /usr/bin/gcl -f

(format t "~%Loading lab3.cl~%")
(load "lab3.cl")


; sample test calls: basic use of dispatcher
; ------------------------------------------

(format t "~%Creating dispatcher~%")
(defvar TT (buildTimeTracker '("Earth" ("Theta" 12300) ("Omicron" 61000) ("Delta" 37200))))

;(format t "~%Creating TE (Empty)~%")
;(defvar TE (buildTimeTracker '()))
;(format t "~A~%" TE) 
; Setting speed of space craft that exists
(format t "~%Setting speed of Theta~%")
(format t "~A~%" (funcall TT 'Speed "Theta" 12000000))

; Setting speed of space craft that doesn't exist
(format t "~A~%" (funcall TT 'Speed "Gamma" 12000000))

;(format t "~%Setting time passed to 1000~%")
;(defvar r (funcall TT 'TimePassed 1000))
;(format t "    result is ~A~%" r)
;
;(format t "~%Looking up time for Ivan~%")
;(setf   r (funcall TT 'CurrentTime "Ivan"))
;(format t "    result is ~A~%" r)
;
;(format t "~%Changing Zalika speed to 12345~%")
;(setf   r (funcall TT 'Speed "Zalika" 12345))
;(format t "    result is ~A~%" r)


; sample test calls: use of macros
; --------------------------------

;(format t "~%Using macro to set time passed to 22222~%")
;(setf r (setTime TT 22222))
;(format t "    result is ~A~%" r)
;
;(format t "~%Using macro to look up current time for Emma~%")
;(setf r (getTime TT "Emma"))
;(format t "    result is ~A~%" r)
;
;(format t "~%Using macro to change Zalika speed to 500~%")
;(setf r (setSpeed TT "Zalika" 500))
;(format t "    result is ~A~%" r)
;
;(format t "~%...end of testing~%~%")


