(define (make-resistor resistance)
  (attach-type 'resistor resistance))

(define (resistor? ckt)
  (eq? (type ckt) 'resistor))

(define (make-series ckt1 ckt2)
  (attach-type 'series (list ckt1 ckt2)))

(define (series? ckt)
  (eq? (type ckt) 'series))

(define (make-parallel ckt1 ckt2)
  (attach-type 'parallel (list ckt1 ckt2)))

(define (parallel? ckt)
  (eq? (type ckt) 'parallel))


(define (resistance ckt)
  (cond ((resistor? ckt)
	 (resistance-resistor (contents ckt)))
	((parallel? ckt)
	 (resistance-parallel (contents ckt)))
	((series? ckt)
	 (resistance-series (contents ckt)))))

(define (conductance ckt)
  (cond ((resistor? ckt)
	 (conductance-resistor (contents ckt)))
	((parallel? ckt)
	 (conductance-parallel (contents ckt)))
	((series? ckt)
	 (conductance-series (contents ckt)))))


(define (resistance-resistor resistor)
  resistor)

(define (conductance-resistor resistor)
  (/ 1 (resistance-resistor resistor)))


(define (resistance-series ckt)
  (+ (resistance (left-branch ckt))
     (resistance (right-branch ckt))))

(define (conductance-series ckt)
  (/ 1 (resistance-series ckt)))

(define (conductance-parallel ckt)
  (+ (conductance (left-branch ckt))
     (conductance (right-branch ckt))))

(define (resistance-parallel ckt)
  (/ 1 (conductance-parallel ckt)))



(define left-branch car)
(define right-branch cadr)
(define attach-type cons)
(define type car)
(define contents cdr)



(define (repeated f n)
  (lambda (x)
    (if (= n 0)
	x
	((repeated f (-1+ n)) (f x)))))

(define (L-extend base series-part parallel-part)
  (make-series series-part (make-parallel parallel-part base)))

(define (ladder-extension stages base series-part parallel-part)
  ((repeated (lambda (x) (L-extend x series-part parallel-part)) stages)
   base))
