#! /usr/bin/gcl -f

(format t "~%Loading lolol.cl~%")
(load "lolol.cl")

(format t "~%Creating dispatcher~%")
(defvar Lise (student '("laise" "24329")))
(format t "~A~%" (funcall Lise 'initialize 1))
(format t "~A~%" (funcall Lise 'print_stuff 1))