(defun custom_array (len wid) 

    (format t "~A ~A~%" len wid )
    
    (cond 
        ((not (integerp len)) (let () (format t "ERROR: Length must be int" ) nil))
        ((not (integerp wid)) (let () (format t "ERROR: Width must be int" ) nil))
        (t (make-array (list len wid)))
    
    
    )
    
    
)

(custom_array 2 3)
(format t "Enter two numbers")

; Current problem: You want to have a let with dynamic variables (user provided)
; I think you'll be doing a let over lambda, as a simpler version of what you'll need for lab3
