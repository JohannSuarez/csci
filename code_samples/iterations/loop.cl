#! /usr/bin/gcl -f  

; loops can contain any of the following:
;  - a name (used to create a named block for the loop, e.g. one we can return-from)
;  - a set of local variable specifications
;      (detailing how the variables are initialized and updated)
;  - a set of action statements
;      (detailing what happens during the loop, and under what circumstances)


; declare some variables we'll use in the loop examples
(defvar L '(1 2 3 4 5))
(defvar L2 '(1 "foo" 15 #\x 13))
(defvar H (make-hash-table))
(setf (gethash "first" H) 1)
(setf (gethash "second" H) 2)
(defvar V (vector 10 30 20))

; loop illustrating the "for" statement
;    the for statement isn't like a for loop in other languages,
;    instead it specifies new local variables and if/how they
;    are set/updated as the "FirstExample" loop executes
(loop
   ; specify the block name we'll use
   named FirstExample

   ; version 1a,1b: iterating through lists, vectors
   for x in L
   for y across V

   ; version 2a, 2b: iterating through numeric values
   for i upfrom 3 below 10 by 2
   for j downfrom 12 above 1 by 3

   ; version 3: using one expression for the initial value, another for the update
   for a = 2 then (* a a)

   ; version 4a, 4b: iterating across the keys/values of a hash table
   for key being the hash-keys of H
   for val being the hash-values of H

   ; a simple list body, using do to specify an action
   do (format t "~A,~A,~A,~A,~A,~A,~A~%" x y i j a key val))

; loops illustrating some of the ways we can process data

; quit and return if we find a 5
(defvar r
   (loop for x in L2 when (and (numberp x) (equalp x 5)) return t))
(format t "returned ~A~%" r)

; quit and return if we find a string
(setf r
   (loop for x in L2 thereis (stringp x)))
(format t "returned ~A~%" r)

; quit and return true if all are numbers
(setf r
   (loop for x in L always (numberp x)))
(format t "returned ~A~%" r)

; loops illustrating some of the ways we can accumulate data

; use collect to make and return a list of all the
;     relevant values we create during the loop
(setf r
   (loop for x in L2 when (numberp x) collect (* 2 x)))
(format t "returned ~A~%" r)

; use count to count the relevant values
(setf r
   (loop for x in L2 when (numberp x) count x))
(format t "returned ~A~%" r)

; use maximize to find the biggest relevant value
(setf r
   (loop for x in L2 when (numberp x) maximize x))
(format t "returned ~A~%" r)

; example of a loop that behaves like nested for loops in C
(defvar iMin 1) (defvar iMax 3) ; make up some loop boundaries for i
(defvar jMin 1) (defvar jMax 5) ; make up some loop boundaries for j
(defvar result
   (loop
      for i = iMin then (if (< j jMax) i (+ i 1)) ; when/how does i change
      for j = iMax then (if (< j jMax) (+ 1 j) 1) ; when/how does j change
      for sum = 0 then (+ sum j i)                ; update sum each pass
      do (format t "...i: ~A, j: ~A, sum: ~A~%" i j sum) ; print stats every pass
      when (and (= i iMax) (= j jMax)) return sum)) ; quit when i and j both maxed
(format t "Final result: ~A~%" result)

; note that in C/C++ we would typically write this like
; sum = 0;
; for (i = iMin; i < iMax; i++) {
;     for (j = jMin; j < jMax; j++) {
;         sum += i + j;
;         printf("...i: %d, j: %d, sum: %d\n", i, j, sum);
;     }
; }
; printf("Final result: %d\n", sum);
;
; but the closest parallel in C code would use the , and ?: C operators:
; for ( // the three initialization statements
;       i = iMin,
;       j = jMin,
;       sum = 0;
;       // the condition to test
;       (i < iMax) || (j < jMax) ;
;       // the three update statements
;        i = (j < jMax)? i : i+1,
;        j = (j < jMax)? j+1 : 1
;        sum += i + j
;     ) {
;      // the loop body
;      printf("...i: %d, j: %d, sum: %d\n", i, j, sum);
; }
; printf("Final result: %d\n", sum);

