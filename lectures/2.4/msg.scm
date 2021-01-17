(define (make-regular root)
  (lambda (msg)
    (cond ((eq? msg 'fps) root)
	  ((eq? msg 'sps) root)
	  ((eq? msg 'tps) (add-s root))
	  ((eq? msg 'fpp) root)
	  ((eq? msg 'spp) root)
	  ((eq? msg 'tpp) root))))

(define (make-es root)
  (lambda (msg)
    (cond ((eq? msg 'fps) root)
	  ((eq? msg 'sps) root)
	  ((eq? msg 'tps) (add-es root))
	  ((eq? msg 'fpp) root)
	  ((eq? msg 'spp) root)
	  ((eq? msg 'tpp) root))))

(define (make-irreg fp sp tp)
  (lambda (msg)
    (cond ((eq? msg 'fps) fp)
	  ((eq? msg 'sps) sp)
	  ((eq? msg 'tps) tp)
	  ((eq? msg 'fpp) sp)
	  ((eq? msg 'spp) sp)
	  ((eq? msg 'tpp) sp))))

(define (operate op obj)
  (obj op))

(define (fps obj) (operate 'fps obj))
(define (sps obj) (operate 'sps obj))
(define (tps obj) (operate 'tps obj))
(define (fpp obj) (operate 'fpp obj))
(define (spp obj) (operate 'spp obj))
(define (tpp obj) (operate 'tpp obj))


(define (add-s wd)
  (word wd 's))

(define (add-es wd)
  (word wd 'es))

(define eat (make-regular 'eat))
(define drink (make-regular 'drink))
(define go (make-es 'go))
(define box (make-es 'box))
(define be (make-irreg 'am 'are 'is))



