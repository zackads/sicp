;;;; DON'T JUST LOAD THIS FILE!

;;;; Open this file and then paste it into Scheme.
;;;; The part after (mce) is for pasting into the non-deterministic evaluator.


(load "~cs61a/lib/traced-ambeval.scm")

(mce)

(define (even-sum a b)
    (require (= (remainder (+ a b) 2) 0))
    (list a b)) 

(even-sum (amb 1 2 3) (amb 4 5 6))

try-again

try-again

(define (int-range low high)
    (if (> low high) (amb)
        (amb low (int-range (+ low 1) high))))

(define (pyth i j k)
    (require (= (+ (* i i) (* j j)) (* k k)))
    (list i j k))

(define (find-pyth low high)
    (let ((i (int-range low high)))
         (let ((j (int-range i high)))
              (let ((k (int-range j high)))
                   (pyth i j k)))))

(find-pyth 3 6)

try-again
