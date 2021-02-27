#! /usr/bin/gcl -f

(defvar L '("Jake" "Esmae" "Finnegan"))

(dolist (el L)
    (format t "~A" el))
    