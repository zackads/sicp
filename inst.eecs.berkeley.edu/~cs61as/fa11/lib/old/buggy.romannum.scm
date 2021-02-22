; Return the decimal value of the Roman numeral whose digits are
; contained in roman-numeral.
; Roman-numeral is assumed to contain only Roman digits.
; Sample call: (decimal-value 'xiv), which should return 14.

(define (decimal-value roman-numeral)
  (element-sum
    (prefix-values-removed
      (digit-values roman-numeral))))
      
; Return a sentence containing the decimal values of the Roman digits
; in roman-numeral.
; Roman-numeral is assumed to contain only Roman digits.
; Sample call: (digit-values 'xiv), which should return (10 1 5).

(define (digit-values roman-numeral)
  (if (empty? roman-numeral)
      '()
      (sentence 
       (decimal-digit-value (first roman-numeral))
       (digit-values (butfirst roman-numeral)))))
      
; Return the decimal value of the given Roman digit.

(define (decimal-digit-value roman-digit)
  (cond
    ((equal? roman-digit 'm) 1000)
    ((equal? roman-digit 'd)  500)
    ((equal? roman-digit 'c)  100)
    ((equal? roman-digit 'l)   50)
    ((equal? roman-digit 'x)   10)
    ((equal? roman-digit 'v)    5)
    ((equal? roman-digit 'i)    1)))

; Return the result of removing prefixes from number-sent.
; number-sent is assumed to contain only positive numbers.
; A prefix is a number that is less than its successor in the sentence.
; The prefix and its successor are replaced by the difference between
; the successor value and the prefix.
; Sample call: (prefix-values-removed '(10 1 5)), which should return 
; (10 4).

(define (prefix-values-removed number-sent)
  (cond
    ((empty? number-sent) '())
    ((empty? (butfirst number-sent)) number-sent)
    ((and (= (count number-sent) 2)
          (>= (first number-sent) (first (bf number-sent))))
     number-sent)
    ((not (starts-with-prefix? number-sent))
     (sentence 
       (first number-sent)
       (prefix-values-removed (butfirst number-sent))))
    ((starts-with-prefix? number-sent)
     (sentence
       (- (first (prefix-values-removed number-sent)) (first number-sent))
       (butfirst (prefix-values-removed (butfirst number-sent)))))))
			
; Return true if the number-sent starts with a prefix, i.e. a number
; that's less than the second value in the sentence.
; number-sent is assumed to be of length at least 2 and to contain 
; only positive numbers.

(define (starts-with-prefix? number-sent)
  (< (first number-sent) (first (bf number-sent))))
	
; Return the sum of the values in number-sent.
; number-sent is assumed to contain only positive numbers.

(define (element-sum number-sent)
  (if (empty? number-sent) 
    0
    (+ (first number-sent) (element-sum (butfirst number-sent)))))
