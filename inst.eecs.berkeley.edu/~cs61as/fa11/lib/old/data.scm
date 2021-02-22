;; An objected - oriented / data directed
;; English Grammar demonstration

(define (make-regular root)
  (attach-type 'regular root))

(define (make-es root)
  (attach-type 'es root))

(define (make-irreg fp sp tp)
  (attach-type 'irreg (list fp sp tp)))

(define (type thing)
  (car thing))

(define (contents thing)
  (cdr thing))

(define (attach-type type thing)
  (cons type thing))

(define (root thing)
  thing)

(define (fp thing)
  (car thing))

(define (sp thing)
  (cadr thing))

(define (tp thing)
  (caddr thing))

(define (add-s wd)
  (word wd 's))

(define (add-es wd)
  (word wd 'es))

(define eat (make-regular 'eat))
(define drink (make-regular 'drink))
(define go (make-es 'go))
(define box (make-es 'box))
(define be (make-irreg 'am 'are 'is))

(define (operate op obj)
  (let ((proc (get (type obj) op)))
    (if  proc  ; will be #f if none 
	(proc (contents obj))
	(error "Undefined operator" (list op obj)))))

(define (fps obj) (operate 'fps obj))
(define (sps obj) (operate 'sps obj))
(define (tps obj) (operate 'tps obj))
(define (fpp obj) (operate 'fpp obj))
(define (spp obj) (operate 'spp obj))
(define (tpp obj) (operate 'tpp obj))

(put 'regular 'fps root)
(put 'regular 'sps root)
(put 'regular 'tps (lambda (x) (add-s (root x))))
(put 'regular 'fpp root)
(put 'regular 'spp root)
(put 'regular 'tpp root)

(put 'es 'fps root)
(put 'es 'sps root)
(put 'es 'tps (lambda (x) (add-es (root x))))
(put 'es 'fpp root)
(put 'es 'spp root)
(put 'es 'tpp root)

(put 'irreg 'fps fp)
(put 'irreg 'sps sp)
(put 'irreg 'tps tp)
(put 'irreg 'fpp sp)
(put 'irreg 'spp sp)
(put 'irreg 'tpp sp)



