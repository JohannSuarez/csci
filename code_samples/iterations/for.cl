#! /usr/bin/gcl -f 

; emulating a for loop like
; for (current = small; current < large; current *= 2) {
;     print("%d squared is %d\n", current, current*current)
; }
(defun printsquares (small large)
   (loop
      ; count which iteration we're on
      for pass from 1 
      ; update for the current base value:
      ;    set it to small if we're on the first pass, 
      ;    otherwise double it on each pass
      for current = (if (eql pass 1) small (* 2 current))
      ; keep going as long as we haven't hit the end value
      while (< current large)
      ; body of the loop: prints the square of the current value
      do (format t "~A squared is ~A~%" current (* current current))
   )
)

; emulating a for loop like
; for (current = L; member(E,current); current = tail(current)) {
;     print current;
; }
(defun printlist (E L)
   (loop
      ; count which iteration we're on
      for pass from 1
      ; update for the current list to examing:
      ;    the entire list if we're on the first pass,
      ;    otherwise chop off the front element
      for current = (if (eql pass 1) L (cdr current))
      ; keep going as long as we the current list still contains the test element
      while (member E current)
      ; body of the loop: print the current list
      do (format t "~A~%" current)
   )
)

; sample calls
(format t "Calling printsquares~%")
(printsquares 1 10)
(format t "~%Calling printlist~%")
(printlist 3 '(6 5 4 3 2 1))

