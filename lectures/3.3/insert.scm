;; Functional version of insert (no side effects):

(define (insert n olist)
  (cond ((null? olist) (cons n '()))
	((< n (car olist)) (cons n olist))
	((= n (car olist)) olist)
	(else (cons (car olist)
		    (insert n (cdr olist)) )) ))

;; Examples to try:
;;
;; > (define a (list 3 7 20))
;; > (define b (insert 15 a))
;; > b
;; > a
;;
;; > (trace cons)
;; > (insert 1 b)    ; only does one cons
;; > (insert 27 b)   ; has to do 5 conses


;; New version (avoids copying pairs from the original list):

(define (insert! n olist)
  (cond ((null? (cdr olist)) (set-cdr! olist (cons n '())))
	((< n (cadr olist)) (set-cdr! olist (cons n (cdr olist))))
	((= n (cadr olist)) #f)   ; returned value doesn't matter
	(else (insert! n (cdr olist))) ))

;; Examples to try:
;;
;; > (define a (list '*olist* 3 7 20))    ; remember we need a header
;; > (define b (insert! 15 a))
;; > b   ; Note that b doesn't have the answer!
         ; Insert! works by changing a:
;; > a
;;
;; > (trace cons)
;; > (insert! 1 a)    ; only does one cons
;; > (insert! 27 a)   ; so does this one


;; Improved version of insert!, using an ADT:

(define (make-empty-olist)
  (cons '*olist* '())) ;; header is ignored generally

(define (empty-olist? olist)
  (null? (cdr olist)))

(define (first-in-olist olist)
  (cadr olist))

(define (rest-of-olist olist)
  (cdr olist))

(define (add-to-olist! item olist)
  (set-cdr! olist (cons item (cdr olist))))

(define (insert! n olist)
  (cond ((empty-olist? olist) (add-to-olist! n olist))
	((< n (first-in-olist olist)) (add-to-olist! n olist))
	((= n (first-in-olist olist)) #f)   ; returned value doesn't matter
	(else (insert! n (rest-of-olist olist))) ))

;; > (define c (make-empty-olist))
;; > (insert! 5 c)
;; > (insert! 8 c)
;; > ...
