#! /usr/bin/gcl -f

; macros allow us to create psuedo-functions that run at compile time
;    to generate other code that is embedded in our source code
; macros run before our code is compiled, so the code they generate
;    gets compiled and is ready when the time comes for execution
;
; macros are often used to effectively expand on the language:
;    creating language functionality that otherwise doesn't exist
;
; in general, macros are recommended when trying to accomplish
;    something that cannot readily be done with functions, or
;    to improve run time behaviour by performing some computation
;    at compile-time instead

; macro definition syntax
; -----------------------
; (defmacro macroName (macro parameter list)
;    body of macro)
;
; -----------------------------------------------------------------------------------
; example 1: (from Paul Graham's book, "Ansi Common Lisp"
; ----------  see the course reference material)
;    we want to to be able to use (nil! variableName) to set a
;    variable's value to nil, so we'll create a macro that looks
;    for things like (nil! x) and translates them into (setf x nil)
(defmacro nil! (varname)
    `(setf ,varname nil))

; syntax explanation for ` and ,
; ------------------------------
;    the ` is like ' except that elements inside the list are
;        "automatically" treated as quoted too,
;    e.g. `(foo x (2 3 4)) is like '('foo 'x '(2 3 4))
;    ... it makes for much simpler writing/reading of the macros
;
;    the , essentially unquotes a term - i.e. it allows you to insert the contents
;        of the term instead of the term itself
;    e.g. if d had value 3, then `(a b c ,d e f g) would produce (a b c 3 e f g)
;

; thus if our source code contains (nil! x) and (nil! y)
;    then (just prior to compilation) they would be translated into
;    (setf x nil) and (setf y nil)

; -----------------------------------------------------------------------------------
; example 2
; ---------
; suppose we want to be able to use head and tail for lists, since they're more
;    intuitive than car and cdr, but we don't want the efficiency penalty from
;    defining them as functions that simply call car and cdr
;
; we can implement them as macros, so we use head and tail in our code,
;    but they get translated into car and cdr before the code compiles
(defmacro head (L) `(car ,L))
(defmacro tail (L) `(cdr ,L))

; thus if our source code contains (head '(1 2 3))
;    then prior to compilation it would be translated into (car '(1 2 3))

; DEBUGGING NOTE: since it gets translated (called expanded) before compilation,
;    any compilation error messages are based on the translated code,
;    not the original source code (so error messages might talk about errors
;    from CAR and CDR, not HEAD and TAIL)

; if we want, we can throw in some error checking while we're at it:
(defmacro headSafe (L) `(if (and (listp ,L) (not (null ,L))) (car ,L) nil))
(defmacro tailSafe (L) `(if (and (listp ,L) (not (null ,L))) (cdr ,L) nil))

; -----------------------------------------------------------------------------------
; example 3 (also from Paul Graham's book, "Ansi Common Lisp"
; ---------  see the course reference material)
; suppose we wanted the language feature (avg x1 x2 ... xn)
;     to compute the average of values x1...xn
(defmacro avg (&rest args)
   `(/ (+ ,@args) ,(length args)))

; syntax explanation for ,@
; -------------------------
;    the ,@ assumes the following variable is a list, and inserts the list contents next,
;    e.g. if d had value (1 2) then
;            `(a b c ,d  e) produces (a b c (1 2) e)
;            `(a b c ,@d e) produces (a b c 1 2 e)
;
; remember the &rest indicates args is supposed to be the list of whatever
;    values are supplied - as many as desired - and the ,@ embeds the
;    list contents into the summation term, and the length of the list
;    is computed at compile time, instead of runtime
;
; thus if our source code contains (avg 10 20 30 40)
;    then the macro translates it into (/ (+ 10 20 30 40) 5)

; -----------------------------------------------------------------------------------
; example 4
; ---------
; suppose we wanted swap functionality, e.g. (swap x y)
;    we can write a simple macro using a temporary variable,
;    but how do we ensure the temporary variable name we use inside swap
;    doesn't already exist in the code the macro is embedded in?
;
;    (gensym) generates a new, unique symbol, so we'll use that within our macro:
(defmacro swap (x y)
   (let ((insym (gensym)))
        `(let ((,insym ,x))
           (setf ,x ,y)
           (setf ,y ,insym))))

; if our code contains (swap a b), it gets replaced with
;   (let ((TTTT a)) (setf a b) (setf b TTTT)
; where TTTT would actually be some unique name gensym generated for us

; -----------------------------------------------------------------------------------
; example 5
; ---------
; suppose we want to be able to run a block of code that alters a variable value,
;    but restore the variable to its original value afterwards

; (reset var body)
;    stores the value of the variable, var
;    runs the code body (which may change the variable value),
;    then resets the variable to its starting value
; note that the &body tag below acts very much like &rest, but
;    is traditionally used where the "rest" is list of actions
(defmacro reset (var &body body)
   (let
      ; use tmp to hold a unique variable name
      ((tmp (gensym)))
      ; generated code:
      `(let
          ; store the original value of var
          ((,tmp ,var))
          ; run the body of the code
          ,@body
          ; reset the variable value
          (setf ,var ,tmp))))

(defvar x 1)
(format t "~%testing reset: original x ~A~%" x)
(reset x
   (setf x 2)
   (format t "changed x ~A~%" x))
(format t "x after leaving reset ~A~%~%" x)

; -----------------------------------------------------------------------------------
; example 6
; ---------

; suppose we have a costly computation to perform,
;    and we need to pass the result to two or more functions afterward
;
; a common solution is to use a variable in a let block
;    to compute/store the value,
; then pass the variable to the various functions in the body of the let block
; (let ((value (sqrt x)))
;      (f1 value)
;      (f2 value)
;      (f3 value))

; we could use a macro to add simpler syntax for this, e.g. useComputed:
;   (useComputed (sqrt x)
;       (f1 value)
;       (f2 value)
;       (f3 value))

; computation is the expensive computation
; body is the set of statements it gets used in,
; note: the name 'value' must be used by the programmer
;    for the shared/computed value
(defmacro useComputed (computation &body body)
   `(let ((value ,computation)) ; store the computed value
      (when value ,@body)))     ; pass it along

(format t "trying our useComputed macro~%")
(useComputed (sqrt 10)
    (format t "sqrt(10) is ~A~%~%" value))


; -----------------------------------------------------------------------------------
; example 7
; ---------
; suppose we wanted the following language feature, ifsafe:
;
; (ifsafe check expressions)
; --------------------------
; if 'check' is true then run the series of expressions
;    (returning the value of the last one)
; otherwise do nothing
;
; in our source code (once the macro existed) we could write things like
;
; (ifsafe (realp x)
;     (format t "computing sqrt(~A)~%" x)
;     (sqrt x))
;
; we can use the following macro that implements ifsafe:
(defmacro ifsafe (check &rest body)
   `(if ,check (progn ,@body)))

; let's try out ifsafe:

(defvar x 3)
(defvar result nil)
(format t "~%about to try ifsafe on the good x value, should compute a sqrt...~%")

(setf  result  (ifsafe (realp x)
                  (format t "computing sqrt(~A)~%" x)
                  (sqrt x)))

(format t "result is now ~A~%" result)

; let's try ifsafe again, with an unsafe value (the body of ifsafe shouldn't run)
(defvar y "foo")
(format t "~%about to try ifsafe on the bad y value, nothing should come out...~%")
(ifsafe (and (realp y) (>= y 0))
    (format t "computing sqrt(~A)~%" y)
    (sqrt y))
(format t "...hopefully nothing happened?~%~%")

; if you want to see what a macro expands to after the first step,
;    you can use macroexpand-1 then the expression, 
; e.g. for the first ifsafe above:
(setf expCode (macroexpand-1 '(ifsafe (realp x)
                  (format t "computing sqrt(~A)~%" x)
                  (sqrt x))))
(format t "~%The expanded macro code is:~% ~A~%~%" expCode)

; -----------------------------------------------------------------------------------
; example 8
;
; macros to grab the number of command line arguments and any one command line argument

; (argc) - returns the number of command line arguments supplied
; ------
(defmacro argc ()
    `(length si::*command-args*))

; (argv i) - returns the i'th command line argument (or 'NoSuchArg)
; --------
(defmacro argv (i)
    `(cond
         ((not (integerp ,i)) 'NoSuchArg)
         ((< ,i 0) 'NoSuchArg)
         ((>= ,i (length si::*command-args*)) 'NoSuchArg)
         (t (nth ,i si::*command-args*))))

(format t "Num args: ~A~%" (argc))
(format t "Argv[-1]: ~A~%" (argv -1))
(format t "Argv[0]: ~A~%" (argv 0))
(format t "Argv[1]: ~A~%" (argv 1))


; -----------------------------------------------------------------------------------
; example 9
;
; the and operation can be represented using if statements, e.g.
;    y and z == (if y z)
;    x and y and z == (if x (if y z))
;    w and x and y and z == (if w (if x (if y z)))
; etc
; observe that this returns either nil (if any terms are nil)
;   or the value of the last term (if all terms are non-nil)
;
; logical or can similarly be represented using or statements, e.g.
;    y or z == (if y y z)
;    x or y or z == (if x x (if y y (if z z)))
; etc
; this returns nil if all terms are nil,
;   or the value of the first non-nil term
;
; both approaches use short-circuiting - i.e. returning an answer as soon
;    as it is known (as soon as we hit a nil term in the AND, or as soon as
;    we hit a non-nil term in OR)
; this speeds up execution, but if any of the 'skipped' arguments have side
;    effects those side effects will not take place (a common implementation
;    choice, but something the programmer needs to be aware of)

; AND? a recursive macro to support "and" with any number of arguments
;    returns nil as soon as a nil operand is encountered,
;    otherwise returns value of the final operand
(defmacro AND? (&rest args)
    (cond
        ; treat (AND?) as true
        ((null args) t)
        ; if there is only one element just use that as the value of the expression
        ((null (cdr args)) (car args))
        ; if there are multiple arguments, we want the form
        ;    (if firstArg  OTHERS)
        ; where others are the result of applying AND? to the remaining args
        (t `(if ,(car args) (AND? ,@(cdr args))))))

; OR? a recursive macro to support "or" with any number of arguments
;    returns nil if all operands are nil,
;    otherwise returns value of the first non-nil operand
(defmacro OR? (&rest args)
    (cond
        ; treat (OR?) as true
        ((null args) t)
        ; if there is only one element just use that as the value of the expression
        ((null (cdr args)) (car args))
        ; if there are multiple arguments, we want the form
        ;    (if firstArg  firstarg OTHERS)
        ;    where others are the result of applying OR? to the remaining args
        ; however, if firstArg is actually an expensive computation,
        ;    or an expression with side effects, we don't want to evaluate
        ;    it multiple times, so we'll use a let block and gensym to create
        ;    a local variable name, evaluate firstarg, and store it in the variable
        ; then we'll use the local variable in the expression
        (t (let ((v (gensym)))
              `(let ((,v ,(car args)))
                   (if ,v ,v (OR? ,@(cdr args))))))))

; show some examples
(defvar x 1)
(defvar y t)
(defvar z nil)
(format t "x is ~A, y is ~A, z is ~A~%" x y z)
(format t "(OR?) = ~A, expands to ~A~%" (OR?) (macroexpand-1 '(OR?)))
(format t "(OR? x) = ~A, expands to ~A~%" (OR? x) (macroexpand-1 '(OR? x)))
(format t "(OR? y x) = ~A, expands to ~A~%" (OR? y x) (macroexpand-1 (macroexpand-1 '(OR? y x))))
(format t "(OR? z x y) = ~A, expands to ~A~%" (OR? z x y) (macroexpand '(OR? z x y)))

; (NOT? x) macro, should result in nil if x is true, t otherwise 
(defmacro NOT? (&optional (x nil)) `(if ,x nil t))

; XOR? recursive macro to support "xor" with any number of arguments
;    returns nil if an even number of operands are non-nil
;    otherwise returns value of the final operand
(defmacro XOR? (&rest args)
    (cond
        ; treat (XOR?) as true
        ((null args) t)
        ; if there is only one element just use that as the value of the expression
        ((null (cdr args)) (car args))
        ; if there are multiple arguments, we want the form
        ;    (if firstArg  (XNOR? otherArgs) (XOR? otherArgs))
        (t `(if ,(car args) (XNOR? ,@(cdr args)) (XOR? ,@(cdr args))))))

; XNOR? recursive macro to support "xnor" with any number of arguments
;    returns nil if an odd number of operands are non-nil
;    otherwise returns value of the final operand
(defmacro XNOR? (&rest args)
    (cond
        ; treat (XNOR?) as true
        ((null args) t)
        ; if there is only one element just use its inverse as the value of the expression
        ((null (cdr args)) (NOT? (car args)))
        ; if there are multiple arguments, we want the form
        ;    (if firstArg  (XNOR? otherArgs) (XOR? otherArgs))
        (t `(if ,(car args) (XOR? ,@(cdr args)) (XNOR? ,@(cdr args))))))

; show some examples
(defvar x 1)
(defvar y t)
(defvar z '(1 2))
(format t "x is ~A, y is ~A, z is ~A~%" x y z)
(format t "(XOR?) = ~A, expands to ~A~%" (XOR?) (macroexpand-1 '(XOR?)))
(format t "(XNOR?) = ~A, expands to ~A~%" (XNOR?) (macroexpand-1 '(XNOR?)))
(format t "(XOR? x) = ~A, expands to ~A~%" (XOR? x) (macroexpand-1 '(XOR? x)))
(format t "(XNOR? x) = ~A, expands to ~A~%" (XNOR? x) (macroexpand-1 '(XNOR? x)))
(format t "(XOR? x y) = ~A, expands to ~A~%" (XOR? x y) (macroexpand-1 '(XOR? x y))))
(format t "(XNOR? x y) = ~A, expands to ~A~%" (XNOR? x y) (macroexpand-1 '(XNOR? x y))))
(format t "(XOR? x y z) = ~A, expands to ~A~%" (XOR? x y z) (macroexpand '(XOR? x y z)))
(format t "(XNOR? x y z) = ~A, expands to ~A~%" (XNOR? x y z) (macroexpand '(XNOR? x y z)))

; -----------------------------------------------------------------------------------
; example 10
;
; suppose we wanted to include c-style while and for loops, e.g.
;
; (while (< x y)
;     ... have one or more statements in the body ...)
;
; (for ((x 1) (< x y) (setf x (+ x 1))
;     ... have one or more statements in the body ...)
;
(defmacro while (test &rest body)
   `(do nil ((not ,test)) ,@body))

(defmacro for (init test update body)
   `(do (,init) ((not ,test)) ,@body ,update))

