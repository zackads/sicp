; Some sample manipulations of sets.
; Note that any true (non-#f) value is acceptable anywhere #t appears.

(define s1 (new-set '(7 2 13 11 6 3) list-set-type))
(member? 11 s1)         ; should be #t
(member? 12 s1)         ; should be #f
(elements s1)

(define s2 (with-element 11 s1))    ; should be the same set as s1
(member? 11 s2)         ; should be #t
(member? 12 s2)         ; should be #f
(elements s2)

(define s3 (with-element 12 s1))    ; one more element than s1 has
(map (lambda (x) (member? x s3)) '(11 12 14))   ; should be (#t #t #f)
(elements s3)

(define s4 (new-set '(7 2 13 11 6 3) intvls-set-type))
; the following should return (#f #t #t #f #f #t #t #f #f #f #t #f #t #f)
(map 
  (lambda (x) (member? x s4)) 
  '(1 2 3 4 5 6 7 8 9 10 11 12 13 14))
(elements s4)

(define s5 (with-element 2 s4))    ; should be the same set as s4
; the following should return (#f #t #t #f #f #t #t #f #f #f #t #f #t #f)
(map 
  (lambda (x) (member? x s5)) 
  '(1 2 3 4 5 6 7 8 9 10 11 12 13 14))
(elements s5)

(define s6 (with-element 1 s4))
; the following should return (#t #t #t #f #f #t #t #f #f #f #t #f #t #f)
(map 
  (lambda (x) (member? x s6)) 
  '(1 2 3 4 5 6 7 8 9 10 11 12 13 14))
(elements s6)

(define s7 (with-element 4 s4))
; the following should return (#f #t #t #t #f #t #t #f #f #f #t #f #t #f)
(map 
  (lambda (x) (member? x s7)) 
  '(1 2 3 4 5 6 7 8 9 10 11 12 13 14))
(elements s7)

(define s8 (with-element 5 s4))
; the following should return (#f #t #t #f #t #t #t #f #f #f #t #f #t #f)
(map 
  (lambda (x) (member? x s8)) 
  '(1 2 3 4 5 6 7 8 9 10 11 12 13 14))
(elements s8)

(define s9 (with-element 9 s4))
; the following should return (#f #t #t #f #f #t #t #f #t #f #t #f #t #f)
(map 
  (lambda (x) (member? x s9)) 
  '(1 2 3 4 5 6 7 8 9 10 11 12 13 14))
(elements s9)

(define s10 (with-element 12 s4))
; the following should return (#f #t #t #f #f #t #t #f #f #f #t #t #t #f)
(map 
  (lambda (x) (member? x s10)) 
  '(1 2 3 4 5 6 7 8 9 10 11 12 13 14))
(elements s10)

(define s11 (with-element 14 s4))
; the following should return (#f #t #t #f #f #t #t #f #f #f #t #f #t #t)
(map 
  (lambda (x) (member? x s11)) 
  '(1 2 3 4 5 6 7 8 9 10 11 12 13 14))
(elements s11)

(define s12 (with-element 15 s4))
; the following should return (#f #t #t #f #f #t #t #f #f #f #t #f #t #f #t #f)
(map 
  (lambda (x) (member? x s12)) 
  '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
(elements s12)

(define s13 (with-element 8 s9))
; the following should return (#f #t #t #f #f #t #t #t #t #f #t #f #t #f)
(map 
  (lambda (x) (member? x s13)) 
  '(1 2 3 4 5 6 7 8 9 10 11 12 13 14))
(elements s13)

(define s14 (with-element 10 s9))
; the following should return (#f #t #t #f #f #t #t #f #t #t #t #f #t #f)
(map 
  (lambda (x) (member? x s14)) 
  '(1 2 3 4 5 6 7 8 9 10 11 12 13 14))
(elements s14)