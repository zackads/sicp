(define pi 3.141592654)

;; tagged data

(define attach-tag cons)
(define type-tag car)
(define contents cdr)

(define (make-square side)
  (attach-tag 'square side))

(define (make-circle radius)
  (attach-tag 'circle radius))

;; conventional style

(define (area shape)
  (cond ((eq? (type-tag shape) 'square)
	 (* (contents shape) (contents shape)))
	((eq? (type-tag shape) 'circle)
	 (* pi (contents shape) (contents shape)))
	(else (error "Unknown shape -- AREA"))))

(define (perimeter shape)
  (cond ((eq? (type-tag shape) 'square)
	 (* 4 (contents shape)))
	((eq? (type-tag shape) 'circle)
	 (* 2 pi (contents shape)))
	(else (error "Unknown shape -- PERIMETER"))))

;; Data-directed programming

(put 'square 'area (lambda (s) (* s s)))
(put 'circle 'area (lambda (r) (* pi r r)))
(put 'square 'perimeter (lambda (s) (* 4 s)))
(put 'circle 'perimeter (lambda (r) (* 2 pi r)))

(define (operate op obj)
  (let ((proc (get (type-tag obj) op)))
    (if proc
	(proc (contents obj))
	(error "Unknown operator for type"))))

(define (area shape)
  (operate 'area shape))

(define (perimeter shape)
  (operate 'perimeter shape))

;; message passing

(define (make-square side)
  (lambda (message)
    (cond ((eq? message 'area)
	   (* side side))
	  ((eq? message 'perimeter)
	   (* 4 side))
	  (else (error "Unknown message")))))

(define (make-circle radius)
  (lambda (message)
    (cond ((eq? message 'area)
	   (* pi radius radius))
	  ((eq? message 'perimeter)
	   (* 2 pi radius))
	  (else (error "Unknown message")))))

(define (operate op obj)
  (obj op))
