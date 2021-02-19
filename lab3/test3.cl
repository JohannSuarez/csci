#! /usr/bin/gcl -f

(format t "~%Loading lab3.cl~%")
(load "lab3.cl")


; sample test calls: basic use of dispatcher
; ------------------------------------------
;(format t "~%Creating dispatcher~%")
;(defvar TT (buildTimeTracker '("Pathos-II" ("Theta" 12300) ("Omicron" 61000) ("Upsilon" 37200))))
;
;(format t "~%Creating TE (Empty)~%")
;(defvar TE (buildTimeTracker '()))
;(format t "~A~%" TE) 
;; Setting speed of space craft that exists
;(format t "~%Setting speed of Theta~%")
;(format t "~A~%" (funcall TT 'Speed "Theta" 12000000))
;
;
;
;; ERROR CHECKS BEGIN HERE
;
;; Setting speed of space craft that doesn't exist
;(format t "~%Setting speed of Spacecraft that doesn't exist~%")
;(format t "~A~%" (funcall TT 'Speed "Gamma" 12000000))
;
;(format t "~%Setting speed of Spacecraft to non-number~%")
;(format t "~A~%" (funcall TT 'Speed "Omicron" "fake speed"))
;
;(format t "~%Setting speed of Spacecraft to 0 or less~%")
;(format t "~A~%" (funcall TT 'Speed "Omicron" 0))
;
;
;
;(format t "~%Giving dispatcher an invalid planet name~%")
;(defvar TR (buildTimeTracker '(2 ("Kappa" 88888) ("Pi" 99999) ("Rho" 44444))))
;
;(format t "~%Giving dispatcher an empty planet name~%")
;(defvar TG (buildTimeTracker '("" ("Theta" 12300) ("Omicron" 61000) ("Upsilon" 37200))))
;
;; ERROR CHECKS END HERE
;(format t "~%Giving dispatcher a valid planet name~%")
(defvar TF (buildTimeTracker '("PATHOS-III" ("Iota" 12345) ("Phi" 67890) ("Sigma" 45678))))
;(format t "~%Changing speed of Iota~%")
;
;(format t "~A~%" (funcall TF 'Speed "Iota" 450000))
;
;
; TIME CHECKS BEGIN HERE

; Providing bad time inputs..
;(defvar j (funcall TF 'TimePassed 0))
;(defvar k (funcall TF 'TimePassed -4))
;(defvar l (funcall TF 'TimePassed "Snow Patrol - Chasing Cars"))

; Good inputs
(format t "~%Setting planet time passed to 1000~%")
(defvar r (funcall TF 'TimePassed 1000))
(format t "Result is ~A~%" r)






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


