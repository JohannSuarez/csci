#! /usr/bin/gcl -f

; example of dolist to iterate through a list

; print each element of a list 
(defvar L '(1 2 3 4))

(dolist
     ; variable to use for iterating through L
     (x L)
     ; expression to apply to each element (x) of L
     (format t "x is ~A~%" x)
)


