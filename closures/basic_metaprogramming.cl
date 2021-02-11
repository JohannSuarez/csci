(defun buildPrt (username)
  (lambda (msg) (format t "~A ~A~%" username msg)))

(defvar prt (buildPrt "Super Me"))
(defvar prt2 (buildPrt "Someone Else"))

(funcall prt "wants to go home")
(funcall prt2 "does not want to go home")