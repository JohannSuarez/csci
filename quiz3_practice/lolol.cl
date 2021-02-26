(defun student (L) 
    (let 
        ; First param of let is a list of all the internal variables
        ; right below in this pair of brackets. 
        
        (
            (stu_name (first L))
            (stu_id (car (cdr L)))
            (stu_gpa 0)
            
        )

        ; The rest can be getters or setters, depending 
        ; on what your class would need.
    
        (labels (

            (initialize ()
                (format t "Initialize function! ~%")
            )

            (set_gpa (arg1)
            
                (format t "In set GPA.~%")
            )

            (print_stuff ()
                (format t "Printing student name: ~A~%" stu_name)
                (format t "Printing student ID: ~A~%" stu_id)
            )

            )
        
            (lambda (cmd &optional (arg1 nil) (arg2 nil))


                  (cond
                    ((equalp cmd 'initialize) (initialize))
                    ((equalp cmd 'set_gpa) (set_gpa arg1))
                    ((equalp cmd 'print_stuff) (print_stuff))

                     (t (format t "Error: invalid command"))

                  )
               ) ; Closing for lambda
       
            

        ) ; Closing for labels. Labels encapsulates lambda. 
        ; But the group of functions are enclosed in their own bracket.
    
    ) ; Closing for let


) ; closing for the student class