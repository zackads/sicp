(define (count sent)		;; wrong!
  (if (empty? sent)
      '()
      (+ 1 (count (butfirst sent)))))


(define (count sent)		;; still wrong!
  (if (empty? sent)
      0
      (+ 1 (count (butfirst snt)))))
