(define x (list 1 2 3))
(define y (list 'a 'b 'c))
(define z (append x y))

;; Try:
;; > x
;; > y
;; > z

(set-car! x 'frog)

;; > x
;; > y
;; > z

(set-car! y 'strange)

;; > x
;; > y
;; > z
