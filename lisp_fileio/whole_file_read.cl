#! /usr/bin/gcl -f


(defun processFile (filename)
    "Reads file contents, as list elements, into a list and returns it."
    (let ((L nil))
        (with-open-file (stream filename)
            (do
                ; local var e, set with the next file line
                ((e (read-line stream nil) (read-line stream nil)))

                ; quit at end of file, reverse L to fix order of lines
                ((null e) (reverse L))


                (format t "    read line ~A~%" e)
                (setf L (cons e L))
            
            
            
            )
        )
    }

)


; main body of script

(let ((fname nil) (result "unread"))


    (format t "Enter a filename: ~%")
    (setf fname (read-line))
    (cond 
        ((not (probe-file fname)) (format t "Error: ~A file not accessible~%" fname))
    )
)

