; let-over-labels exercise
; ------------------------
; the goal is to write a function, buildTimeTracker, that creates and returns a dispatcher
;     (following our let-over-labels-over-lambda approach) that keeps track of elapsed time
;     for a home planet and a fleet of spacecraft travelling from that planet.
; for each spacecraft, it tracks the speed relative to the home planet (in km/s)
;     and the time that has elapsed since launch for that spacecraft,
;     accounting for time dilation (the time that has passed on the home planet will
;     be greater than the time passed for each spacecraft)
; the commands supported by the dispatcher, and the formula for calculating the passage
;     of time based on speed, are provided below.
;
; buildTimeTracker expects to be given a single list as its parameter, e.g.
;       (buildTimeTracker ("fred" ("bob" 10) ("joe" 23)))
;     assumes first name in list must be a string, giving a name for the home planet
;     the rest of the list are in pairs, each pair consisting of
;        (1) a string specifying the name of a spacecraft
;        (2) a non-negative integer specifying its speed relative to the home planet (in km/s)
;
;     the returned dispatcher handles commands as follows:
;
;        set a new speed for a spacecraft,
;            dispatch command is 'Speed, arguments are the name of the spacecraft and new speed
;                e.g. (funcall Dispatcher 'Speed "Bob" 2500)
;            returns their updated speed
;                (if spacecraft or new speed is invalid it doesn't change anything, returns nil)
;
;        query what the current time is (seconds since launch) for a spacecraft or home planet
;            dispatch command is 'CurrentTime, argument is the spacecraft/planet name
;                e.g. (funcall Dispatcher 'CurrentTime "Bob")
;            returns current time for that user (seconds since launches)
;                 (if the user isn't in the list then it returns nil)
;
;        specify a new (additional) amount of time that has passed on earth
;            dispatch command is 'TimePassed, argument is the amount of time that has passed
;                e.g. (funcall Dispatcher 'TimePassed 100)
;            calculates and updates each spacecraft's current time
;                based on their speed relative to the home planet
;            returns the updated time on the home planet (seconds since launches)
;                (if the new time is invalid then it doesn't change the times, returns nil)
;
;
; example:
;  setting up a dispatcher for a home planet named Max and three spacecraft
;     (defvar TT (buildTimeTracker ("Max" ("Ivan" 12300) ("Zalika" 61000) ("Emma" 37200))))
;  calls utilizing the dispatcher
;     (defvar r (funcall TT 'TimePassed 1000)            ; 1000 seconds have passed on earth
;     (setf   r (funcall TT 'CurrentTime "Emma"))        ; lookup Emma's current time
;     (setf   r (funcall TT 'Speed "Zalika" 12345))      ; set Zalika's new speed to 12345 km/s

(defun buildTimeTracker (L)
    (let

         (  ; First element of let are "bindings" - list of variables
            (fleet_list '())
            (planet_name "")
            (fleet_table (make-hash-table :test 'equal))
            ; (fleet_table (make-hash-table :test 'equal :size ((- (length L) 1)))   ) Doesn't work fix later
            (time_elapsed 0) ; The time elapsed on our home planet
            (light_speed_kms 299792)
            ; hash tables with names (make-hash-table). Use (strings) as keys
            ; so create using (make-hash-table :test 'equal)
            ; values for each are a pair (speed elapsed time)
         )

         ; The rest of the elements are the "body" of let.
         ; A check on L, if it's empty there's nothing to do.
         (if (null L) (return-from buildTimeTracker nil)) ; Also check

         ; Check if head of L is string, if it is, then it becomes the planet name.

         (cond
            ((not (stringp (first L)))
               (block
                  pname_error_block_1
                  (format t "Invalid planet name! Name: ~A~%" (first L))
                  (return-from buildTimeTracker 'Error)
               )
            )

            ((= (length (first L)) 0)
               (block
                  pname_error_block_2
                  (format t "Problem with formatting. Planet name is an empty string~%")
                  (return-from buildTimeTracker 'Error)
               ))
         )

         (setf planet_name (first L))



         (labels (

            (initialize ()


               ; Check if tail of L has something, if it is, then they are the fleet.
               (if (not (null (cdr L))) (setf fleet_list (cdr L)))

               ; Loop that inserts fleet into hash table
               (dolist (element fleet_list)

                  (cond
                     ((not (stringp (first element)))
                        (let ()
                           (format t "Problem with formatting. Invalid ship name~% ~A~" (first element))
                           (return-from initialize 'Error)
                        ))

                     ((= (length (first element )) 0)
                        (let ()
                           (format t "Problem with formatting. Ship name is an empty string~%")
                           (return-from initialize 'Error)
                        ))

                     ((not (realp (second element)))
                        (let ()
                           (format t "Problem with formatting. Ship speed isn't a real number: ~A~~%" (second element))
                           (return-from initialize 'Error)
                        ))

                     (t
                        (setf (gethash (first element) fleet_table) (list (second element) 0))
                        )
                  )

               )


            )

            (print_all ()
               (format t "Printing hash table..~%")
               (format t "~A~%"
                  (loop for key being the hash-keys of fleet_table collect
                  (list key (gethash key fleet_table))
                  )
               )
            )

            (set_new_speed (sc_name new_speed) ; Expects two vars, first a string, second an int

               (cond
                  ; Check that the given spacecraft name is string.
                  ((not (stringp sc_name))
                     (let ()
                           (format t "Ship name is not a string!~%")
                           (return-from set_new_speed nil)
                     ))

                  ; Check that the given spacecraft speed is int.
                  ((not (realp new_speed))
                     (let ()
                           (format t "Ship speed is not a real number!~%")
                           (return-from set_new_speed nil)
                     ))

                  ; Check that the given spacecraft speed is greater than 0.
                  ((not (> new_speed 0))
                     (let ()
                           (format t "Ship speed 0 or negative!~%")
                           (return-from set_new_speed nil)
                     ))
               )

               (if (nth-value 1 (gethash sc_name fleet_table))
                    ; If we're in this let block, that means we found a value for our key.
                   (let ()
                     ; We need a time variable to temporarily hold our spacecraft's elapsed time.
                     ; Then it gets restored to the new list that we build for the value of our
                     ; spacecraft's new speed and elapsed time.
                     (setf time (second (gethash sc_name fleet_table)))

                     (setf (gethash sc_name fleet_table) (list new_speed time))
                     (print_all)

                     (return-from set_new_speed new_speed)

                   )

                   (format t "Table does not contain ~A~%" sc_name)
               )


            )

            (lookUpTime (name_input)  ; First check if passed is string. Returns current time of ship.


               (cond
                     ((not (stringp name_input))
                        (let ()
                           (format t "Problem with formatting. Invalid planet/ship name~% ~A~" (first name_input))
                           (return-from lookUpTime'Error)
                        ))

                     ((= (length name_input) 0)
                        (let ()
                           (format t "Problem with formatting. Planet/Ship name is an empty string~%")
                           (return-from lookUpTime 'Error)
                        ))
               )

               (if (equalp name_input planet_name)
                  (let ()
                     (format t "Planet time is: ~A~%" time_elapsed)
                     (return-from lookUpTime time_elapsed)
                  )
               )

               (if (nth-value 1 (gethash name_input fleet_table))
                    ; If we're in this let block, that means we found a value for our key.
                   (let ()

                     (return-from lookUpTime (second (gethash name_input fleet_table)))
                   )

                   (format t "Table does not contain ~A~%" name_input)
               )

            ;        query what the current time is (seconds since launch) for a spacecraft or home planet
            ;            dispatch command is 'CurrentTime, argument is the spacecraft/planet name
            ;                e.g. (funcall Dispatcher 'CurrentTime "Bob")
            ;            returns current time for that user (seconds since launches)
            ;        ----------> Just returns the current_time of that spacecraft <----------------
            ;                 (if the user isn't in the list then it returns nil)

            )



            (calcTime (given_time given_speed)
               ; We've already done all the necessary
               ; prior checks for the input data in the
               ; function that called this.
               ; So all we really need is this one-liner
               (* given_time (sqrt (- 1 (/ (* given_speed given_speed) (* light_speed_kms light_speed_kms)))))

            )

            (time_passed (input_time)   ; First check if passed var is int.
                  ; Adds to the home planet time.
                  ; Adds time all the spacecraft based on their speed (using time dilation formula)

                  (cond
                     ; Check that the given time is int.
                     ((not (realp input_time))
                        (let ()
                              (format t "Provided time is not a real number!~%")
                              (return-from time_passed nil)
                        ))

                     ; Check that the given spacecraft speed is greater than 0.
                     ((not (> input_time 0))
                        (let ()
                              (format t "Provided time 0 or negative!~%")
                              (return-from time_passed nil)
                        ))
                  )

                  (setf time_elapsed (+ time_elapsed input_time))

                  ; We're going to look through each value of the spacecraft's,
                  ;
                  (loop for key being the hash-keys of fleet_table collect
                     (let ()


                        ; Getting speed, so we can put it back when we build the
                        ; new list that will be the value for some spacecraft
                        (setf speed (first (gethash key fleet_table)))


                        ; Getting old time, we need this
                        (setf old_time (second (gethash key fleet_table)))

                        (setf (second (gethash key fleet_table))
                           (+ old_time (calcTime input_time speed)) ; Calling time dilation formula
                        )
                     )
                  )


                  ;(print_all)
                  (return-from time_passed time_elapsed)

            )  ; Closing for time_passed

         )


         ;  Call local methods here.
         (initialize)


              ; building and returning dispatcher
               (lambda (cmd arg1 &optional (arg2 nil))


                  (cond
                     ((equalp cmd 'TimePassed) (time_passed arg1))
                     ((equalp cmd 'CurrentTime) (lookUpTime arg1))
                     ((equalp cmd 'Speed) (set_new_speed arg1 arg2))

                     (t (format t "Error: invalid command"))

                  )
               ) ; Closing for lambda
         ) ; Closing for labels
   ) ; Closing for let
) ; Closing for defun


; macro exercise
; --------------
; create a set of macros that lets the programmer use the following
;
;   (setTime TT n) to set a new time on the home planet,
;        assuming TT is a dispatcher returned by a call to buildTimeTracker,
;        e.g. (defvar X (buildTimeTracker ...regular args....))
;             (setTime X 10000)      ; becomes (funcall X 'TimePassed 10000)
;
;   (getTime TT u) to query the current time for the specified spacecraft,
;        e.g. (getTime X "bob")      ; becomes (funcall X 'CurrentTime "bob")
;
;   (setSpeed TT s) to set a new speed for the specified spacecraft,
;        e.g. (setSpeed X "Max" 500) ; becomes (funcall X 'Speed "Max" 500)

(defmacro setTime (Dispatcher NewTime)
   `(funcall ,Dispatcher 'TimePassed ,NewTime))
   ;`(format t "I am the result of a setTime macro~%")

(defmacro getTime (Dispatcher Spacecraft)
   `(funcall ,Dispatcher 'CurrentTime ,Spacecraft))
   ;`(format t "I am the result of a getTime macro~%"))

(defmacro setSpeed (Dispatcher Spacecraft NewSpeed)
   `(funcall ,Dispatcher 'Speed ,Spacecraft ,NewSpeed))
   ;`(format t "I am the result of a setSpeed macro~%"))


