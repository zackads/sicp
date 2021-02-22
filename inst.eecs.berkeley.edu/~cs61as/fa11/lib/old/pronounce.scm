; How to pronounce cxr-family function names

(define (say name)
  (accent (pronounce name)))

; PRONOUNCE takes a cxr-family word as argument and returns a sentence
; of syllables.

(define (pronounce name)
  (cond ((eq? name 'car) '(car))
	((eq? name 'caar) '(cuh are))
	((empty? name) '())
	((eq? (first-two name) 'ca) (se 'caa (pronounce (bf-two name))))
	((eq? (first name) 'c) (se 'cuh (pronounce (bf name))))
	((eq? (last-three name) 'dar) (se (pronounce (bl-three name)) 'dar))
	((eq? (last-two name) 'ar) (se (pronounce (bl-two name)) 'are))
	((eq? (last-two name) 'dr) (se (pronounce (bl-two name)) 'der))
	((eq? (first-two name) 'da) (se 'daa (pronounce (bf-two name))))
	((eq? (first name) 'a) (se 'aa (pronounce (bf name))))
	(else (se 'de (pronounce (bf name))))))

; ACCENT takes a sentence of syllables (as returned by PRONOUNCE) and inserts
; an exclamation point at the emphasized syllable.  I'm less sure that these
; rules are universally agreed to than for PRONOUNCE.

; In particular, all my life I've said "CUH de der" for cddr, but apparently
; to be consistent I should be saying "cuh DE der."  I think I have heard people
; say it that way, though, so I'm reluctant to special-case it like the first
; syllable of caar in PRONOUNCE.

(define (accent syls)
  (define (help prev rest)
    (if (null? rest)
	(se (word prev '!))
	(let ((winover (assoc prev '((caa . (daa dar de der))
				     (cuh . (der))
				     (aa  . (dar de der))
				     (daa . (aa are daa dar de der))
				     (de  . (de der))))))
	  (if (member? (car rest) (cdr winover))
	      (cons (word prev '!) rest)
	      (cons prev (help (car rest) (cdr rest)))))))
  (if (null? (cdr syls))
      syls
      (help (car syls) (cdr syls))))

; Utility functions to get a table of pronunciations of all names up to length n

(define (table n)
  (for-each (lambda (w) (print (list w (say w)))) (cr n)))

(define (cr n)
  (if (= n 0)
      '()
      (append (cr (- n 1))
	      (map (lambda (w) (word 'c w 'r)) (cross n)))))

(define (cross n)
  (if (= n 0)
      '("")
      (let ((small (cross (- n 1))))
	(append (map (lambda (w) (word w 'a)) small)
		(map (lambda (w) (word w 'd)) small)))))

; Helper functions are obvious except to note that they test for too-short
; arguments so that we can make more specific tests first in PRONOUNCE above.
; ("More specific" means that we test for long substrings before short ones.)

(define (first-two wd)
  (if (< (count wd) 2)
      wd
      (word (first wd) (first (bf wd)))))

(define (bf-two wd)
  (if (< (count wd) 2)
      ""
      (bf (bf wd))))

(define (last-two wd)
  (if (< (count wd) 2)
      wd
      (word (last (bl wd)) (last wd))))

(define (bl-two wd)
  (if (< (count wd) 2)
      ""
      (bl (bl wd))))

(define (last-three wd)
  (if (< (count wd) 3)
      wd
      (word (last (bl (bl wd))) (last (bl wd)) (last wd))))

(define (bl-three wd)
  (if (< (count wd) 3)
      ""
      (bl (bl (bl wd)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Results:

; > (table 5)
; (car (car))
; (cdr (cuh! der))
; (caar (cuh are!))
; (cdar (cuh dar!))
; (cadr (caa! der))
; (cddr (cuh de! der))
; (caaar (caa aa are!))
; (cdaar (cuh daa! are))
; (cadar (caa! dar))
; (cddar (cuh de dar!))
; (caadr (caa aa! der))
; (cdadr (cuh daa! der))
; (caddr (caa! de der))
; (cdddr (cuh de! de der))
; (caaaar (caa aa aa are!))
; (cdaaar (cuh daa! aa are))
; (cadaar (caa! daa are))
; (cddaar (cuh de daa! are))
; (caadar (caa aa! dar))
; (cdadar (cuh daa! dar))
; (caddar (caa! de dar))
; (cdddar (cuh de! de dar))
; (caaadr (caa aa aa! der))
; (cdaadr (cuh daa! aa der))
; (cadadr (caa! daa der))
; (cddadr (cuh de daa! der))
; (caaddr (caa aa! de der))
; (cdaddr (cuh daa! de der))
; (cadddr (caa! de de der))
; (cddddr (cuh de! de de der))
; (caaaaar (caa aa aa aa are!))
; (cdaaaar (cuh daa! aa aa are))
; (cadaaar (caa! daa aa are))
; (cddaaar (cuh de daa! aa are))
; (caadaar (caa aa daa! are))
; (cdadaar (cuh daa! daa are))
; (caddaar (caa! de daa are))
; (cdddaar (cuh de! de daa are))
; (caaadar (caa aa aa! dar))
; (cdaadar (cuh daa! aa dar))
; (cadadar (caa! daa dar))
; (cddadar (cuh de daa! dar))
; (caaddar (caa aa! de dar))
; (cdaddar (cuh daa! de dar))
; (cadddar (caa! de de dar))
; (cddddar (cuh de! de de dar))
; (caaaadr (caa aa aa aa! der))
; (cdaaadr (cuh daa! aa aa der))
; (cadaadr (caa! daa aa der))
; (cddaadr (cuh de daa! aa der))
; (caadadr (caa aa daa! der))
; (cdadadr (cuh daa! daa der))
; (caddadr (caa! de daa der))
; (cdddadr (cuh de! de daa der))
; (caaaddr (caa aa aa! de der))
; (cdaaddr (cuh daa! aa de der))
; (cadaddr (caa! daa de der))
; (cddaddr (cuh de daa! de der))
; (caadddr (caa aa! de de der))
; (cdadddr (cuh daa! de de der))
; (caddddr (caa! de de de der))
; (cdddddr (cuh de! de de de der))
