(define (stream-withdraw balance amount-stream)
    (cons-stream balance
                 (stream-withdraw (- balance
                                     (head amount-stream))
                                  (tail amount-stream))))

;; a withdrawal stream

(define (wd-stream)(cons-stream (prompt)(wd-stream)))

(define (prompt)(print "new withdrawal -->")(read))

(define mywd (wd-stream))

;;; insert a few values by  (tail(tail (tail mywd)))

;;; (stream-withdraw  100 mywd)


(define (for-each-stream proc stream)
  (if (empty-stream? stream)
      'done
      (sequence (proc (head stream))
                (for-each-stream proc (tail stream)))))

;; print a (finite) stream.

(define (print-stream s)
  (for-each-stream print s))

(define ps print-stream)

(define (for-each-stream-count  proc stream n)
  (if (or (= n 0) (empty-stream? stream))
      'done
      (sequence (proc (head stream))
                (for-each-stream-count  proc (tail stream) (+ n -1)))))

(define (psn s n)(for-each-stream-count print s n))

;;;;;;;;;;;;;;;;

;;; TAYLOR SERIES
;;;  (we could use abstraction better here...)
;;; a term in a series looks like  (a b) , a list
;;; signifying  b*x^a

;;   here is the number 1 = 1*x^0

 (define unit-term (list 0 1))   ;; could be (make-term 0 1)

(define (integrate-series series)
  (map integrate-term series))

;;; to integrate b*x^a with respect to x, produce
;;; (b/(a+1)*x^(a+1)

(define (integrate-term tt)
    (let ((new-order (1+ (car tt))))   ;; could use (order tt) instead of car
      (list new-order (/ (cadr tt) new-order)))) ;; make-term; coeff;

(define exp-series (cons-stream unit-term (integrate-series exp-series)))

;; (- integrate sin) to get cos  (also, cos(0) = 1) 

;; cos x is 1 -x^2/2 + x^4/4! + ..

;;          1 -0.5 x^2 + 0.04166 x^4+ ...

(define cos-series (cons-stream unit-term
				  (neg-st    (integrate-series sin-series))))


;; we  need to compute the negative of a "taylor series stream"

(define (neg-st s)(if (empty-stream? s) s
		      (cons-stream (list (car (head s))(- (cadr (head s))))
				   (neg-st (tail s)))))

;;  integrate cos to get sin

(define sin-series  (integrate-series cos-series))

;;; sin x   = x  -x^3/3!+ x^5/5! + ...
;;;           x - 0.166 x^3 + 0.0083*x^5 + ...

;;(psn sin-series 5)
