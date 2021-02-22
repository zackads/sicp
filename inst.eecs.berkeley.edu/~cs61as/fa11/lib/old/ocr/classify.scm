;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Naive Bayes Classifier CS61A
;; Author: Min Xu
;;
;; - based on Classification Project by Prof. Dan Klein
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "samples.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pay attention to the comments in the code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Label->Feature Counter List ADT definitions
;;
;; For every label, it stores a probability for each feature
;; Used to store P(feature|label)
;;
;; You will use the following data structure but
;; you DO NOT need to understand the code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-label/feat)
  (list (cons 'label/feat-list nil)))

(define (label/feat? ls)
  (and (list? ls)
       (list? (car ls))
       (eq? (caar ls) 'label/feat-list)))

;;;;;;;;;
;; IN: label, feature, label/feat list ADT
;; OUT: P(feature | label) as stored in the input label/feat list

(define (get-label/feat  label  feat  label/feat)
  (let ((label/feat-pair (assoc label label/feat)))
    (if label/feat-pair

	(let* ((feat-counter (cdr label/feat-pair))
	       (feat-count-pair (assoc feat feat-counter)))
	  (if feat-count-pair
	      (cdr feat-count-pair)
	      0))
	(error "Label DOES NOT EXIST in label/feat"))))
	   
;;;;;;;;;;;;;
;; For every label, normalize feature-probability
;; list so total probability sum to 1

(define (normalize-label/feat  label-counter  label/feat)
  (if (null? label-counter)
      (begin
	(display "Done Normalizing.")
	(newline))
      (let* ((label (caar label-counter))
	     (total (cdar label-counter))
	     (feat-counter-pair (assoc label label/feat)))
	(if (not feat-counter-pair)
	    (error "LABEL NOT FOUND in label/feat")
	    (for-each
	       (lambda (feat-count-pair)
		  (set-cdr! feat-count-pair
			    (/ (cdr feat-count-pair) total)))
	       (cdr feat-counter-pair)))
	(normalize-label/feat (cdr label-counter) label/feat))))

;;;;;;;;;;;;;
;; IN: label, list of features, label/feature list ADT
;; Increases count of
;;   P(feature | label) by 1 for every feature in the feat-list

(define (add-to-label/feat  label  feat-list  label/feat)
  (let ((label/feat-pair (assoc label label/feat)))
    (if label/feat-pair
	(let ((feat-counter (cdr label/feat-pair)))

	  (define (merge feat-list)
	    (if (null? feat-list) 'okay
		(begin
		  (let* ((feat (car feat-list))
			 (feat-count-pair (assoc feat feat-counter)))
		    (if feat-count-pair
			(set-cdr! feat-count-pair
				  (+ 1 (cdr feat-count-pair)))
			(append! feat-counter
				 (list (cons feat 1)))))
		  (merge (cdr feat-list)))))
	  (merge feat-list))

	(begin
	  (append! label/feat
		 (list (cons label
			     (list (cons 'sentinel 0)))))
	  (add-to-label/feat label feat-list label/feat)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Label Counter ADT
;;
;; Stores a probability for every label
;; Used to store p(label)
;;
;; You will use the following data structure but
;; You DO NOT need to understand the code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-label-counter)
  (list (cons 'label-counter 0)))

;;;;;;;;;
;; returns a list of all the possible labels

(define (get-keys-label-counter  label-counter)
  (map (lambda (x) (car x)) (cdr label-counter)))

;;;;;;;;;
;; Normalizes count associated with given label-counter
;; so probability sums to 1

(define (normalize-label-counter  label-counter)
  (let ((total (accumulate + 0 (map (lambda (x) (cdr x)) (cdr label-counter)))))
    (for-each
     (lambda (x) (set-cdr! x (/ (cdr x) total)))
     (cdr label-counter))))


(define (get-label-count-pairs  label-counter)
  (cdr label-counter))

;;;;;;;
;; updates p(label) with an addition count

(define (add-to-label-counter  label  label-counter)
  (let ((cur-count-pair (assoc label label-counter)))
    (if cur-count-pair
	(set-cdr! cur-count-pair (+ 1 (cdr cur-count-pair)))
	(append! label-counter
		 (list (cons label 1))))))

;;;;;;;;
;; returns p(label) as stored in input label-counter
;;

(define (get-label-count  label  label-counter)
  (let ((cur-count-pair (assoc label label-counter)))
    (if cur-count-pair
	(cdr cur-count-pair)
	(error "Label NOT FOUND in get-label-count"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global Counters to Keep Statistics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define label-counter (make-label-counter))
(define label/feat-counter (make-label/feat))
(define total-samples 0)
(define all-features '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Training Procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (refresh)  
  (set! label-counter (make-label-counter))
  (set! label/feat-counter (make-label/feat))
  (set! total-samples 0))

(define (train in-stream feat-getter label-getter  max-samples)
  (cond ((or (stream-null? in-stream) (> total-samples max-samples))
	 (normalize)
	 (display "Finished Training.")
	 (newline))
	(else
	 (let ((new-inst (stream-car in-stream)))

	   ;; EXERCISE 1: FILL IN HERE
	   ;;
	   ;; read comments on Label Counter ADT
	   ;; and Label/Feature Counter List ADT
	   ;;
	   ;; pay attention to procedures
	   ;; "add-to-label/feat" and
	   ;; "add-to-label-counter"
	   
	   
	   (set! total-samples (+ total-samples 1))

	   (if (= (remainder total-samples 50) 0)
	       (begin
		 (flush)
		 (display ".")
		 (flush)))

	   (train (stream-cdr in-stream) feat-getter label-getter max-samples)))))



(define (normalize)
  (normalize-label/feat  (get-label-count-pairs label-counter)  label/feat-counter)
  (normalize-label-counter  label-counter))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inference Procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (infer-label  instance  feat-getter  all-feats)
  (let* ((feat-list (feat-getter instance))
	(label-prob-pairs
	 (map                         ;; maps over all LABELS
		(lambda (label)          
		 (cons label
		     
			    ;; READ:
				;; we are defining a cons pair
				;; the car is the label
				;; the cdr (you will fill in) should be the
				;; score for that label
			    
				;; EXERCISE 2 FILL IN here
				;;
				;; fill in code that gives a score
				;; for the label according to Naive Bayes Algorithm
				;;
				;; Pay attention to our global
				;; statistics counters
				;; and to
				;; "get-label/feat" procedure
				;; in label/feat-list ADT definition
				;;
				;; You do not need to normalize the score
				
				))
				       
		(get-keys-label-counter label-counter))))
    
    (arg-max
     (normalize-kv-pairs label-prob-pairs))
    
	  ))

(define (normalize-kv-pairs kv-pairs)
  (let ((total (accumulate + 0
			   (map cdr kv-pairs))))
    (if (> total 0)
	(for-each (lambda (pair)
		    (set-cdr! pair (/ (cdr pair) total)))
		  kv-pairs))
    kv-pairs))

;;;;;;;
;; IN: list of key-value pairs whose value are numbers
;; OUT: key-value pair whose value is the largest
;;

(define (arg-max kv-pairs)
  (cond ((null? kv-pairs) nil)
	((null? (cdr kv-pairs)) (car kv-pairs))
	((> (cdar kv-pairs)
	    (cdr (arg-max (cdr kv-pairs))))
	 (car kv-pairs))
	(else
	 (arg-max (cdr kv-pairs)))))


;;;;;;;;;;;;;;;
;; Mini-test Cases:
;;
;; Cat vs. Bear
;;
;; EXERCISE 3 goes in this section:
;;;;;;;;;;;;;;;

(define c1 (list 'cat 'furry 'small 'yellow))
(define c2 (list 'cat 'furry 'medium 'brown))
(define c3 (list 'cat 'furry 'small 'black))

(define b1 (list 'bear 'furry 'small 'brown))
(define b2 (list 'bear 'furry 'big 'white))
(define b3 (list 'bear 'furry 'medium 'black))
(define b4 (list 'bear 'furry 'big 'black))
(define b5 (list 'bear 'furry 'medium 'black))

(define test-stream
 (cons-stream c1
 (cons-stream c2
 (cons-stream c3
 (cons-stream b1
 (cons-stream b2
 (cons-stream b3
 (cons-stream b4
 (cons-stream b5
	      the-empty-stream)))))))))
	 

(define (catbear-label-getter test-case)
  (car test-case))

(define (catbear-feat-getter test-case)
  (cdr test-case))

(define f (list 'cat 'furry 'small 'black))
(define g (list 'bear 'furry 'big 'black))
(define h (list 'bear 'furry 'medium 'black))
(define i (list 'cat 'furry 'medium 'yellow))
(define j (list 'bear 'furry 'small 'brown))

(define all-catbear-feats (list 'furry 'small 'big 'medium 'brown 'black 'yellow 'white))

(train test-stream catbear-feat-getter  catbear-label-getter  1000)

(display "Mini-test 1 (should be cat): ") (print (infer-label f catbear-feat-getter all-catbear-feats))
(display "Mini-test 2 (should be bear): ") (print (infer-label g catbear-feat-getter all-catbear-feats))
(display "Mini-test 3 (should be bear): ") (print (infer-label h catbear-feat-getter all-catbear-feats))
(display "Mini-test 4 (should be cat): ") (print (infer-label i catbear-feat-getter all-catbear-feats))
(display "Mini-test 5 (should be bear after EX 3): ") (print (infer-label j catbear-feat-getter all-catbear-feats))

(display "Mini-tests completed.") (newline) (newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OCR Test Case and Evaluation
;;
;; interfacing with images
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(refresh)

(define all-feats '())

(define (add-to-all-feats)
  (define (loop n)
    (if (= n (* IMG-DIM IMG-DIM)) '()
	(let ((x (remainder n IMG-DIM))
	      (y (div n IMG-DIM)))
	  (if (and (= (remainder x 2) 0)
		   (= (remainder y 2) 0))
	      (set! all-feats (cons (list x y) all-feats)))
	  (loop (+ n 1)))))
  (loop 0))

(add-to-all-feats)

(define (img-feat-getter img)
  (define (loop n)
    (if (= n (* IMG-DIM IMG-DIM)) '()
	(let ((x (remainder n IMG-DIM))
	      (y (div n IMG-DIM)))
	  (if (and (= (remainder x 2) 0)
		  (= (remainder y 2) 0)
		  (or (> (get-xy x y img) 0)
		      (> (get-xy (+ x 1) y img) 0)
		      (> (get-xy x (+ y 1) img) 0)
		      (> (get-xy (+ x 1) (+ y 1) img) 0)))
	      (cons (list x y)
		    (loop (+ n 1)))
	      (loop (+ n 1))
	      ))))

  (loop 0))

;;;;;;;;;;;;;;;;;;;
;; Procedure to run "infer-label" on all images in test-stream
;; report incorrectness
  
(define (evaluate in-stream verb max-tests)
  (define (loop in-stream total total-right)
    (if (or (stream-null? in-stream) (> total max-tests))
	(begin
	  (display "Total tests: ") (display total)
	  (newline)
	  (display "Total correct: ") (display total-right)
	  (newline)
	  (display "Percent Correct: ") (display (/ total-right total))
	  (newline))
	(begin
	  (display "Test Number: ") (display total) (newline)
	  (let* ((cur-inst (car in-stream))
		 (guess-prob (infer-label cur-inst img-feat-getter all-feats))
		 (guess (car guess-prob))
		 (gold (get-img-label cur-inst)))
	    (if (not (equal? gold guess))
		(begin
		  (if verb
		      (pretty-print cur-inst))
		  (newline) (display "NOPE")
		  (newline) (display "Guess: ")(display guess)(newline)
		  (display "Gold: ") (display gold) (newline)
		  (loop (stream-cdr in-stream) (+ total 1) total-right))
		(begin
		  (if verb 
		      (pretty-print cur-inst))
		  (newline) (display "YES") 
		  (newline) (display "Guess: ") (display guess) (newline)
		  (display "Gold: ") (display gold) (newline)
		  (loop (stream-cdr in-stream) (+ total 1) (+ total-right 1))))))))
  (loop in-stream 0 0))


;;;;;;;;;;;;;;
;; Procedure calls to start OCR classification
;; uncomment and load "classify.scm" to run
;;;;;;;;;;;;;;

;;(train img-streams img-feat-getter get-img-label 500)
;;(evaluate vimg-stream #t 50)
