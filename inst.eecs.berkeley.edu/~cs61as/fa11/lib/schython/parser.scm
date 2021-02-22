;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File: PARSER.SCM
;; Author: Hoa Long Tam (hoalong.tam@berkeley.edu)
;;
;; Adapted for use in Python from a Logo-in-Scheme interpreter written by Brian
;; Harvey (bh@cs.berkeley.edu), available at ~cs61a/lib/logo.scm
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Selectors for the list returned by py-read.
(define indentation car)
(define tokens cdr)

(define (make-line-obj line)
  (instantiate line-obj (indentation line) (tokens line)))

;; A class to represent a sequence of tokens to be used by the evaluator.
(define-class (line-obj indentation tokens)
  (method (empty?)
	  (null? tokens))
  (method (exit?)
	  (member tokens '((exit |(| |)|) (quit |(| |)|))))
  (method (peek)
	  (car tokens))
  (method (push token)
	  (set! tokens (cons token tokens)))
  (method (next)
	  (let ((token (car tokens)))
	    (set! tokens (cdr tokens))
	    token)))

;; Parser utility functions
(define (char->symbol ch) (string->symbol (make-string 1 ch)))
(define operators '(#\+ #\- #\* #\/ #\% #\< #\> #\! #\=))
(define (comma? symbol) (eq? symbol '|,|))
(define (colon? symbol) (eq? symbol '|:|))
(define open-brace-symbol (char->symbol #\{))
(define close-brace-symbol (char->symbol #\}))
(define open-paren-symbol (char->symbol #\())
(define close-paren-symbol (char->symbol #\)))
(define open-bracket-symbol (char->symbol #\[))
(define close-bracket-symbol (char->symbol #\]))
(define (char-newline? char)
  (or (eq? char #\newline) ;; you're in
      (and (eq? char #\return)
	   (eq? (peek-char) #\newline)
	   (read-char))))  ;; chomp off newline

;;;;
;; The main tokenizer.  Reads in a line from standard input and returns a list
;; of the form (indentation token1 token2 token3 ...).  Turns the line
;; 'def foo(a,b):' into (def foo |(| a |,| b |)| :).
;;;
(define (py-read)
  (define (get-indent-and-tokens)
    ;; TODO: Both Partners, Question 2
    (cons 0 (get-tokens '())))
  (define (reverse-brace char)
    (let ((result (assq char '((#\{ . #\}) (#\} . #\{)
			       (#\( . #\)) (#\) . #\()
			       (#\[ . #\]) (#\] . #\[)))))
      (if result
	  (cdr result)
	  (read-error "SyntaxError: bad closing brace: " char))))
  (define (get-tokens braces)
    ;; Reads in until the end of the line and breaks the stream of input into a
    ;; list of tokens.  Braces is a list of characters representing open brace
    ;; ([, (, and {) tokens, so it can throw an error if braces are mismatched.
    ;; If it reaches the endof a line while inside braces, it keeps reading
    ;; until the braces are closed.
    (let ((char (peek-char)))
      (cond
       ((char-newline? char)
	(if (not (null? braces))
	    (begin (read-char) (get-tokens braces))
	    (begin (read-char) '())))
       ((eof-object? char)
	(if (not (null? braces))
            (read-error "SyntaxError: End of file inside expression")
            '()))
       ((eq? char #\space)
        (read-char)
        (get-tokens braces))
       ((eq? char #\#)
        (ignore-comment)
        '())
       ((memq char (list #\[ #\( #\{))
        (let ((s (char->symbol (read-char))))
          (cons s (get-tokens (cons char braces)))))
       ((memq char (list #\] #\) #\}))
	(if (and (not (null? braces)) (eq? char (reverse-brace (car braces))))
            (let ((t (char->symbol (read-char))))
              (cons t (get-tokens (cdr braces))))
            (read-error "SyntaxError: mismatched brace: " char)))
       ((memq char (list #\, #\:))
        (let ((t (char->symbol (read-char))))
          (cons t (get-tokens braces))))
       ((memq char (list #\" #\'))
        (let ((t (list->string (get-string (read-char)))))
          (cons t (get-tokens braces))))
       ((memq char operators)
        (let ((t (get-operator)))
          (cons t (get-tokens braces))))
       ((char-numeric? char)
	(let ((num (get-num "")))
	  (if (string? num)
              (cons (string->number num) (get-tokens braces))
              (cons num (get-tokens braces)))))
       (else
	(let ((token (get-token (char->symbol (read-char)))))
	  (cond
           ((and (string? token)
                 (eq? (string-ref token 0) #\.)
                 (char-numeric? (string-ref token 1)))
	    (cons (word (string->symbol (string-append "0" token)))
		  (get-tokens braces)))
           ((string? token)
	    (cons (string->symbol token) (get-tokens braces)))
           (else (cons token (get-tokens braces)))))))))
  (define (get-token so-far)
    (let ((char (peek-char)))
      (if (eof-object? char)
	  (begin
	    (read-char)
	    so-far)
	  (if (not (or (char-alphabetic? char)
		   (char-numeric? char)
		   (eq? char #\_)))
	      so-far
	      (get-token (word so-far (char->symbol (read-char))))))))
  (define (get-num num-so-far)
    ;; Reads in a number.  Num-so-far a Scheme word (we will convert back into
    ;; a Scheme number in the get-tokens procedure).
    ;; TODO: Person B, Question 3
    (let ((char (peek-char)))
      (if (eof-object? char)
	  (begin
	    (read-char)
	    num-so-far)
	  (if (char-numeric? char)
	      (get-num (word num-so-far (char->symbol (read-char))))
	      num-so-far))))
  
  (define (get-operator)
    (let ((char (read-char))
	  (next (peek-char)))
      (cond ((eq? char #\+) (if (eq? next #\=) (begin (read-char) '+=) '+))
	    ((eq? char #\-) (if (eq? next #\=) (begin (read-char) '-=) '-))
	    ((eq? char #\%) (if (eq? next #\=) (begin (read-char) '%=) '%))
	    ((eq? char #\<) (if (eq? next #\=) (begin (read-char) '<=) '<))
	    ((eq? char #\>) (if (eq? next #\=) (begin (read-char) '>=) '>))
	    ((eq? char #\=) (if (eq? next #\=) (begin (read-char) '==) '=))
	    ((eq? char #\/) (if (eq? next #\=) (begin (read-char) '/=) '/))
	    ((eq? char #\!)
	     (if (eq? next #\=)
		 (begin (read-char) '!=)
		 (read-error "Unknown operator: !")))
	    ((eq? char #\*)
	     (cond ((eq? next #\*)
		    (read-char)
		    (if (eq? (peek-char) #\=)
			(begin (read-char) '**=)
			'**))
		   ((eq? next #\=) (read-char) '*=)
		   (else '*))))))
  (define (get-string type)
    ;; Reads in a string and returns a list of Scheme characters, up to, but not
    ;; including the closing quote.  Type is the Scheme character that opened
    ;; the string.  The first character returned by (read-char) when this
    ;; function is executed will be the first character of the desired string.
    (read-error "TodoError: Person A, Question 3"))
  (define (ignore-comment)
    (read-error "TodoError: Both Partners, Question 1"))
  (get-indent-and-tokens))

;; Error handler for py-read.  Needs to eat remaining tokens on the line from
;; user input before throwing the error.

(define (read-error . args)
  (define (loop)
    (let ((char (read-char)))
      (if (or (char-newline? char) (eof-object? char))
	  (apply py-error args)
	  (loop))))
  (loop))
