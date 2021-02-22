;   Int-1.scm
;   Interpreter - Release 1

;   This version of the interpreter is capable of evaluating :
;   1.  Literal numbers. 1, 2, 3, ...
;   2.  Literal predicates. #T, #F
;   3.  Quoted objects. 'A, 'Hello, '(1 2 3), ...

;   As mentioned in class, the basic model our interpreter follows is :
;   1.  Read an expression from the keyboard.
;   2.  Evaluate the expression.
;   3.  Repeat.

;   As you read this module, you will notice that all of the functions
;   defined in this module begin with the letters m-. I have used this
;   prefix on all functions defined in my program to avoid name conflicts
;   with primitive procedures built into the underlying interpreter. Be
;   careful naming procedures. If you accidentally redefine a primitive
;   procedure, you will have to reload the underlying interpreter to
;   retrieve the original definition.

;   To launch the interpreter type : (m-scheme)

;   **********************************************************************

;   M-Scheme
;   Interpret expressions until the user enters 'Done.
;   Parameters  :   None.
;   Returns     :   Nothing.

;   This procedure relies on the primitive procedure, read, which returns
;   2 types of information :
;   1.  Atom - A single object.
;   2.  Pair - A pair of objects.
;   You can determine whether the information returned from read is an
;   atom or a pair by applying the primitive procedure pair? to the value
;   returned by the procedure.

;   The basic logic of scheme is as follows :
;   1.  Read an expression from the keyboard.
;   2.  Hand the expression to m-evaluate for translation.
;   3.  If the return value is 'DONE, terminate the session.
;   4.  Otherwise invoke m-interpret recursively to translate the next
;       expression.

(define (m-scheme)
    (newline)
    (print   "Type 'Done To Exit")      ; Exit instructions.
    (display "Expression : ")           ; Input prompt.

    (let
        ((result (m-evaluate (read))))  ; Read an expression from the
                                        ; keyboard
        (display "Outcome    : " )

        (if (equal? result 'Done)       ; See if the user wants to quit.
            (print 'Bye)

            (begin                      ; If not, display the result and
                (print result)          ; translate the next expression.
                (m-scheme)))))

;   **********************************************************************

;   M-Evaluate.
;   Evaluate an expression.
;   Parameters  :   Expression - The expression being evaluated.
;   Returns     :   The value of the expression being evaluated.

;   This procedure evaluates the components of an expression, including
;   recognizing special forms.

(define (m-evaluate expression)
    (cond
        ((m-number? expression)        ; Is the expression a number?
            (m-evaluate-number expression))
        ((m-predicate? expression)     ; Is the expression a predicate?
            (m-evaluate-predicate expression))
        ((m-quoted? expression)        ; Is the expression quoted?
            (m-evaluate-quoted expression))
        (else                           ; Otherwise it is an error.
            (m-stop "Invalid Expression" "M-Evaluate" expression))))

;   **********************************************************************

;   M-number?.
;   Determine whether an expression consists solely of a literal number.
;   Parameters  :   Expression - The expression being evaluated.
;   Returns     :   #T if the expression consists solely of a literal
;               :   number.
;               :   #F if the expression does not consist solely of a
;               :   literal number.

(define (m-number? expression)
    (and (not (pair? expression)) (number? expression)))

;   **********************************************************************

;   M-predicate?.
;   Determine whether an expression consists solely of a literal
;   predicate.
;   Parameters  :   Expression - The expression being evaluated.
;   Returns     :   #T if the expression consists solely of a literal
;               :   predicate.
;               :   #F if the expression does not consist solely of a
;               :   literal predicate.

(define (m-predicate? expression)
    (and (not (pair? expression))
         (or (eq? expression #t) (eq? expression #f))))

;   **********************************************************************

;   M-quoted?.
;   Determine whether an expression consists solely of a quoted object.
;   Parameters  :   Expression - The expression being evaluated.
;   Returns     :   #T if the expression consists solely of a quoted
;               :   object.
;               :   #F if the expression does not consist solely of a
;               :   quoted object.
;   Comment     :   A quoted object is a list whose car is the word
;               :   'quote and whose cadr is the text of the quotation.

(define (m-quoted? expression)
    (and
        (pair? expression)
        (eq? (car expression) 'quote)
        (pair? (cdr expression))))

;   **********************************************************************

;   M-Evaluate-Number.
;   Evaluate an expression consisting solely of a literal number.
;   Parameters  :   Expression - The expression being evaluated.
;   Returns     :   The value of the expression.

(define (m-evaluate-number expression)
    (if (m-number? expression)
        expression
        (m-stop "Invalid Number" "M-Evaluate-Number" expression)))

;   **********************************************************************

;   M-Evaluate-Predicate.
;   Evaluate an expression consisting solely of a literal predicate.
;   Parameters  :   Expression - The expression being evaluated.
;   Returns     :   The value of the expression.

(define (m-evaluate-predicate expression)
    (if (m-predicate? expression)
        expression
        (m-stop "Invalid Predicate" "M-Evaluate-Predicate" expression)))

;   **********************************************************************

;   M-Evaluate-Quoted.
;   Evaluate an expression consisting solely of a quoted object.
;   Parameters  :   Expression - The expression being evaluated.
;   Returns     :   The value of the expression.

(define (m-evaluate-quoted expression)
    (if (m-quoted? expression)
        (cadr expression)
        (m-stop
            "Invalid Quotation" "M-Evaluate-Quoted" expression)))

;   **********************************************************************

;   M-Stop.
;   Stop the program in the face of an error.
;   Parameters  :   Message - A message describing the problem.
;               :   Procedure - The name of the procedure in which the
;               :   error occured.
;               :   Expression - The text of the expression causing the
;               :   error.
;   Returns     :   Nothing.
;   Side Effect :   Terminates the program.

(define (m-stop message procedure expression)
    (newline)                           ; Display a diagnostic message
    (display "Fatal Error : ")          ; describing the problem.
    (print message)
    (display "Procedure   : ")
    (print procedure)
    (display "Expression  : ")
    (print expression)
    (newline)
    (error ""))                         ; Stop the program.


