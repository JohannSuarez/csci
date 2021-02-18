
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
;            the time computation for spacecraft is as follows:
;                if N seconds pass on the home planet while the craft is travelling at speed S
;                   then the time that passes for them is N * sqrt(1 - (S*S/C*C))
;                   where C = 299792 (approximate speed of light in km/s)
;                assuming (a) that the speed is expressed relative to the home planet,
;                     and (b) that I haven't messed up the formula :)
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
            (time_elapsed 0) ; The time elapsed on our home planet
         )


         ; The rest of the elements are the "body" of let.
         ; A check on L, if it's empty there's nothing to do.
         (if (null L) (return-from buildTimeTracker nil)) ; Also check 

         ; Check if head of L is string, if it is, then it becomes the planet name.
         (if (stringp (first L)) (setf planet_name (first L)))

         ; Check if tail of L has something, if it is, then they are the fleet.
         (if (not (null (cdr L))) (setf fleet_list (cdr L)))

         (format t "Building dispatcher. Planet name: ~A~%" planet_name)
         (format t "Spacecraft name(s): ~A~%" fleet_list)


                
         (labels ( 

            (set_new_speed (sc_name new_speed) ; Expects two vars, first a string, second an int

               (format t "In set_new_speed~%")
               (cond
                  ((not (stringp sc_name)) nil)
                  ((not (realp new_speed)) nil)
               )
               
               (dolist (el fleet_list)
                  (cond 
                     ((equal (first el) sc_name) 
                        (block update_speed
                           (setf (second el) new_speed)
                           (return-from set_new_speed (second el))
                        )
                     )
                  )
               )



               (format t "Spacecraft is ~A.~%New speed is ~A~%" sc_name new_speed)
               (dolist (el fleet_list); Debug statement that prints the fleet list.
                  (format t "~A~%" el))

               
            )     

            (check_time ()  ; First check if passed is string. Returns current time of ship.
               (format t "In check_time~%")

            ;        query what the current time is (seconds since launch) for a spacecraft or home planet
            ;            dispatch command is 'CurrentTime, argument is the spacecraft/planet name
            ;                e.g. (funcall Dispatcher 'CurrentTime "Bob")
            ;            returns current time for that user (seconds since launches)
            ;        ----------> Just returns the current_time of that spacecraft <----------------
            ;                 (if the user isn't in the list then it returns nil)

            )                                 

            (time_passed ()   ; First check if passed var is int.
                  (format t "In time_passed~%")
            )  


         )

          
              ; building and returning dispatcher
               (lambda (cmd arg1 &optional (arg2 nil))
                  ;(format t "I am a dispatcher~%")
                  
                  (cond 
                     ((equalp cmd 'TimePassed) (time_passed))
                     ((equalp cmd 'CurrentTime) (check_time))
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

;(defmacro setTime (Dispatcher NewTime)
;   `(format t "I am the result of a setTime macro~%"))
;
;(defmacro getTime (Dispatcher Spacecraft)
;   `(format t "I am the result of a getTime macro~%"))
;
;(defmacro setSpeed (Dispatcher Spacecraft NewSpeed)
;   `(format t "I am the result of a setSpeed macro~%"))


