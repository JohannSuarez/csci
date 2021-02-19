#! /usr/bin/gcl -f

; create a hash table to store key/value pairs
(setf myTable (make-hash-table :test 'equal))

; assign a key-value pair with key 16 and value "blah"
(setf (gethash "Dolores" myTable) 46)
(setf (gethash "Avril" myTable) 36)
(setf (gethash "Hope" myTable) 54)


; lookup the value associated with key 16
(format t "~A~%" (gethash "Dolores" myTable))
(format t "~A~%" (gethash "Hope" myTable))
(format t "~A~%" (gethash "Avril" myTable))


; ----------------- By this point we assume we have a fully functional table ----

; Loop to see if we got Mike
(if (nth-value 1 (gethash "Mike" myTable))
    (format t "key is ~A~%" (gethash "Mike" myTable))
    (format t "table does not contain ~A~%" "Mike"))


; Loop to see if we got Avril
(if (nth-value 1 (gethash "Avril" myTable))
    (format t "key is ~A~%" (gethash "Avril" myTable))
    (format t "table does not contain ~A~%" "Avril"))
