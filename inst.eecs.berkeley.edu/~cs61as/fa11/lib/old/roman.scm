; Return the decimal value of the Roman numeral whose digits are
; contained in roman-digit-list.
; Roman-digit-list is assumed to contain only Roman digits.
; Sample call: (decimal-value '(x i v)), which should return 14.

(define (decimal-value roman-digit-list)
  (roman-sum (digit-values roman-digit-list) ) )
  
(define (digit-values roman-digit-list)
  (if (null? roman-digit-list) '( )
    (cons 
      (decimal-digit-value (car roman-digit-list))
      (digit-values (cdr roman-digit-list)) ) ) )
      
; Return the decimal value of the given Roman digit.
(define (decimal-digit-value roman-digit)
  (cadr 
    (assoc 
      roman-digit 
      '((m 1000) (d 500) (c 100) (l 50) (x 10) (v 5) (i 1)) ) ) )
                
; Return the decimal value of a Roman numeral. The decimal equivalents
; of its Roman digits are contained in number-list.
; Sample call: (roman-sum '(10 1 5)), which should return 14.

(define (roman-sum number-list)
  (cond
    ((null? number-list) 0)
    ((null? (cdr number-list)) (car number-list))
    ((not (starts-with-prefix? number-list))
     (+ (car number-list) (roman-sum (cdr number-list)) ) )
    ((starts-with-prefix? number-list)
     (+ (- (cadr number-list) (car number-list))
            (roman-sum (cddr number-list)) )) ) )
            
; Return true if the number-list starts with a prefix, i.e. a number
; that's less than the second value in the list.
; Number-list is assumed to be of length at least 2 and to contain 
; only positive numbers.

(define (starts-with-prefix? number-list)
  (< (car number-list) (cadr number-list)) )
