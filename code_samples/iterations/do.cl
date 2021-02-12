
; do loops consist of three parts
;   - a list of local variables, their initial values, and 
;     how they get updated at the end of each pass through the loop
;   - the stopping condition for the loop and
;     a list of statements to execute after the loop stops
;     (the return value of do is the return value of the last statement)
;   - the body of the loop: the statements to execute each pass
(do  
     ; initialization/update descriptions
     ( (x 0 (+ x 1))    ; x = 0 initially, add 1 after every pass
       (y 10 (+ y 2)))  ; y = 10 initially, add 2 after every pass

     ; stopping section
     ( ; stopping condition
       (OR (= x 10) (> y 15))  ; stop if (x == 10) or (y > 15)
       ; actions to perform after stopping
       (format t "~%exited loop~%" x)
       (format t "x stopped at ~A~%" x)
       (format t "y stopped at ~A~%" y)
       (format t "returning ~A~%" (+ x y))
       ; return value
       (+ x y))

    ; loop body
    (format t "inside loop ~%")
    (format t "x is currently ~A~%" x)
    (format t "y is currently ~A~%" y))


     