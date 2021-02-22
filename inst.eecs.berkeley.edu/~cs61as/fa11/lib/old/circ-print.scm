;;; Procedures for printing circular lists.

;; This uses the Common-Lisp syntax: #n=OBJECT defines label n (an
;; unsigned decimal numeral) to be object.  Any occurrence of #n#
;; subsequently stands for a structure eq? to OBJECT.
;;
;; For example, if you evaluate
;;
;;   (define foo (list 1 2))
;;   (define bar (list foo (list 1 2) foo))
;;
;; then bar will normally print as
;;
;;   ((1 2) (1 2) (1 2))
;;
;; while (cprint bar) will print
;;
;;   (#1=(1 2) (1 2) #1#)
;;
;; The "#1=" defines #1# to stand for foo.  Wherever that same pair
;; appears in the list, it is printed as #1#.
;;
;; If you evaluate
;;
;;   (define circ (list 1 2 3))
;;   (set-cdr! (cddr circ) (cdr circ))
;;
;; then circ would normally print as
;;
;;   (1 2 3 2 3 2 3 2 3 ... and so on infinitely)
;;
;; while (cprint circ) will print
;;
;;   (1 . #1=(2 3 . #1#))
;;

(define (cprint x)
  (cprinc x)
  (newline))

(define (cprinc x)
  (let ((dictionary '())
	(count 1))
    (define (prefix context left cdr midvector)
      (case context
	((left) (princ left))
	((cdr) (princ cdr))
	(else (princ midvector))))
    (define (mark x)
      (if (or (pair? x) (vector? x))
	  (let ((p (assq x dictionary)))
	    (if p
		(set-car! (cdr p) (1+ (cadr p)))
		(begin
		  (set! dictionary (cons (list x 1) dictionary))
		  (cond ((pair? x)
			 (mark (car x))
			 (mark (cdr x)))
			(else
			 (do ((i 0 (1+ i)))
			     ((>= i (vector-length x)))
			   (mark (vector-ref x i))))))))))
    (define (cprinc2 x context)
      (cond ((or (symbol? x) (number? x) (string? x) (procedure? x)
		 (char? x) (boolean? x))
	     (prefix context "" " " " ")
	     (princ x))
	    ((null? x)
	     (prefix context "()" "" "()"))
	    ((or (pair? x) (vector? x))
	     (let ((p (assq x dictionary)))
	       (cond ((and (> (cadr p) 1) (not (null? (cddr p))))
		      (prefix context "#" " . #" " #")
		      (princ (cddr p))
		      (princ "#"))
		     ((and (> (cadr p) 1) (null? (cddr p)))
		      (prefix context "#" " . #" " #")
		      (set-cdr! (cdr p) count)
		      (princ count)
		      (princ "=")
		      (set! count (1+ count))
		      (cprinc-pair-or-vect x 'left))
		     (else
		      (cprinc-pair-or-vect x context)))))
	    (else (error "CPRINC: what is this I'm printing? -- " x))))
    (define (cprinc-pair-or-vect x context)
      (if (pair? x)
	(begin
	  (prefix context "(" " " " (")
	  (cprinc2 (car x) 'left)
	  (cprinc2 (cdr x) 'cdr)
	  (prefix context ")" "" ")"))
	(begin
	  (prefix context "#(" " . #(" " #(")
	  (do ((context 'left 'midvector)
	       (i 0 (1+ i)))
	      ((>= i (vector-length x)))
	    (cprinc2 (vector-ref x i) context))
	  (princ ")"))))
    (mark x)
    (cprinc2 x 'left)))
