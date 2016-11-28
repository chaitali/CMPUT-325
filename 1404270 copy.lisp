;Chaitali Patel
;1404270
;cmput 325 LEC B1
;assignment 2


#|

The interpreter takes in a simple programming language FL, and evaluates it in lisp and returns the result.  
The function fl-interp takes in an expression E, and a program P and returns the result of the evaluated E. 
The program P, is a list of function definitions, and E is a function application. 
 
test cases:

primitives:
(fl-interp '(rest (1 2 (3))) nil) => (2 (3))
(fl-interp '(eq (1 2 3) (1 2 3)) nil) => NIL

user defined:
(fl-interp '(f (f 2)) 
            '( (f X =  (* X X)) )
    )
=>  16

|#


(defun fl-interp (E P)
   (cond 
         ((atom E) E)   ;%this includes the case where expr is nil
         (t
            (let ( (f (car E))  (arg (cdr E)) )
	       (cond 
                     ; handle built-in functions
                     ((eq f 'first)  (car (fl-interp (car arg) P)))
		     ((eq f 'rest)  (cdr (fl-interp (car arg) P)))
		     ((eq f '+) (+ (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
		     ((eq f '-) (- (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
		     ((eq f '*) (* (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
		     ((eq f '>) (> (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
		     ((eq f '<) (< (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
		     ((eq f '=) (= (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
                     ((eq f 'and) (returnBool (and (fl-interp (car arg) P) (fl-interp (cadr arg) P))))
		     ((eq f 'or) (returnBool (or (fl-interp (car arg) P) (fl-interp (cadr arg) P))))
		     ((eq f 'not) (returnBool (not (fl-interp (car arg) P))))
		     ((eq f 'eq) (eq (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
		     ((eq f 'equal) (equal (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
		     ((eq f 'cons) (cons (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
		     ((eq f 'number) (numberp (fl-interp (car arg) P)))
		     ((eq f 'atom) (atom (fl-interp (car arg) P)))
		     ((eq f 'null) (null (fl-interp (car arg) P)))
		     ((eq f 'if) (if (fl-interp (car arg) P) (fl-interp (cadr arg) P) (fl-interp (caddr arg) P)))
		     ((eq P nil) E)
	             (t (findfunc E P P))))))
)

#| returnBool ensures the return value of and/or/not is returned as a T/F, instead of a constant. 

eg. (returnBool 5) => T

|#

(defun returnBool (num)
   (if num
       t
       nil)
)

#| findfunc takes in the expression E, and the program P and finds the function definition matching the expression. 

If the function names are equal, and the number of arguments matches the function is found. If the correct function 
is not found, the expression is returned.  

|#

(defun findfunc (E program P)
   (cond
         ((eq program nil) E)
         ((and (checkArg (cdr E) (findparameters E (car program) nil)) (equal (car E) (caar program))) 
               (fl-interp (swaps (cdr E) (car (getbody (car program))) (findparameters E (car program) nil)) P)) 
         (t (findfunc E (cdr program) P)))
)

#| checkArg takes in the input list from E, and the parameter list from the function definition and checks to make sure
   the number of arguments to both functions is the same. 

If the number of arguments is not the same, the function returns nil. 
|# 

(defun checkArg (input var)
   (cond 
          ((eq (countArgs input 0) (countArgs var 0)) t)
           (t nil))
)  

(defun countArgs (E argnum)
   (cond
         ((null (car E)) argnum)
         (t (countArgs (cdr E) (+ argnum 1))))
)

#| findparameters gets the parameters of the function defined in P, and returns them as a list. 

eg. (findparameters '(a 2) '(a X = (+ 1 X)) nil) => (X)

|#

(defun findparameters (E func parameterL)
	(let ((x (cadr func)))
	   (cond
	   ((eq x '=) parameterL) ;(fl-interp (swaps (cdr E) (car body) parameterL) P))
	   (t (findparameters E (cdr func) (append parameterL (list x))))))
)

#| getbody gets the body of the function defined in P, and returns the body as a list. 

eg. (getbody '(a X = (+ 1 X))) => ((+ 1 X))
 
|#

(defun getbody (program)
   (let ((para (car program)))
        (cond 
              ((eq para '=) (cdr program))
              (t (getbody (cdr program)))))
)

#| swaps iterates through the list of input and parameters and passes them onto evals

|#

(defun swaps (input body parameterL)
   (cond 
         ((or (null input) (null parameterL)) body)
          (t (swaps (cdr input) (evals (car input) (car parameterL) body) (cdr parameterL))))
)

#| evals takes in a single input and parameter and each time it encounters the parameter in the body, it switches 
the parameter with the input. 

|#

(defun evals (input parameter body)
   (cond 
   	 ((null body) nil)
   	 ((and (atom body) (equal body parameter)) input)
   	 ((and (atom body) (not (equal body parameter))) body)
   	 ((and (atom (car body)) (equal parameter (car body))) (cons input (evals input parameter (cdr body))))
   	 ((and (atom (car body)) (not (equal (car body) parameter))) (cons (car body) (evals input parameter (cdr body)))) 
   	 (t (cons (evals input parameter (car body)) (evals input parameter (cdr body)))))
)
  
