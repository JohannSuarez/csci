
; dotimes allows us to iterate a fixed number of times,
;    allowing us to specify an index variable,
;    the number of times to repeat,
; and the expression to use as a return value
;
; e.g.
;   (dotimes (x 5 (* x 2)) (format t "Hi ~A~%" x))
; displays
;    Hi 0
;    Hi 1
;    Hi 2
;    Hi 3
;    Hi 4
; and returns 10

