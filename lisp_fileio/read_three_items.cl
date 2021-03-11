#! /usr/bin/gcl -f


(format t "~A~%" (probe-file "tester.cl"))


; Note: read-line doesn't seem to work
; If this cl file is loaded.
; It has to be ran as an executable.

; Assumes there are three items in a given file
; and they're separated by spaces.
(defun processFile (filename)
    (format t "Processing file ~A~%" filename)
    (let ((a nil) (b nil) (c nil))
        (with-open-file (stream filename)
            (setf a (read stream))
            (setf b (read stream))
            (setf c (read stream)))

        (list a b c)
    )
)


; Main body of script 

(let ((fname nil) (result "incomplete"))
    (format t "Enter a filename:~%")
    (setf fname (read-line))

    (cond 
        ((not (stringp fname)) (format t "Error: ~A cannot be filename~%" fname))
        ((not (probe-file fname)) (format t "Error: ~A file not accessible~%" fname))
        (t (setf result (processFile fname)))
    )
    (format t "Final status: ~A~%" result)

)