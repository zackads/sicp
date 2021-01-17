;;; Non-parallel pseudo-mapreduce

; Here's what it ISN'T (but sort of is conceptually):
;
; (define (mapreduce mapper reducer base-case data)
;    (accumulate reducer base-case (map mapper data)))

;; key-value pair ADT

(define make-kv-pair cons)
(define kv-key car)
(define kv-value cdr)

;; Reminder: an association list (a-list) is a list of key-value pairs.

; So the parallel version has THREE phases, not just the two in its name:

(define (mapreduce mapper reducer base-case data)   ; handwavy approximation
  (GROUPREDUCE reducer base-case
	       (SORT-INTO-BUCKETS (MAP mapper data))))

;; Instead of one call to MAPREDUCE, we separate out the stages, using
;; plain old MAP and the following GROUPREDUCE:

(define (groupreduce reducer base-case buckets)
  (map (lambda (subset)
	 (make-kv-pair
	  (kv-key (car subset))
	  (accumulate reducer base-case (map kv-value subset))))
       buckets))

; (Note: Sort-into-buckets is too messy for lecture, but straightforward.)

;; Example 1: add grades per student.

; The data.  In a parallel implementation, each midterm would be handled by
; a separate processor; we'll just concatenate them.

(define mt1 '((cs61a-xc . 27) (cs61a-ya . 40) (cs61a-xw . 35)
	      (cs61a-xd . 38) (cs61a-yb . 29) (cs61a-xf . 32)))
(define mt2 '((cs61a-yc . 32) (cs61a-xc . 25) (cs61a-xb . 40)
	      (cs61a-xw . 27) (cs61a-yb . 30) (cs61a-ya . 40)))
(define mt3 '((cs61a-xb . 32) (cs61a-xk . 34) (cs61a-yb . 30)
	      (cs61a-ya . 40) (cs61a-xc . 28) (cs61a-xf . 33)))

; >> (sort-into-buckets (append mt1 mt2 mt3))
; >> (groupreduce + 0 (sort-into-buckets (append mt1 mt2 mt3)))
; >> (groupreduce (lambda (new old) (+ 1 old)) 0
;		  (sort-into-buckets (append mt1 mt2 mt3)))

;; Example 2: word frequency count.

; First just for one sentence:

(define (wordcounts1 sent)
  (groupreduce + 0 (sort-into-buckets (map (lambda (wd) (make-kv-pair wd 1))
					   sent))))

; >> (wordcounts1 '(cry baby cry))

; Now for simulated files.

;; pseudo-file ADT

(define filename car)
(define lines cdr)

;; Some data (yeah, I left out some songs)

(define file1 '((please please me) (i saw her standing there) (misery)
		(anna go to him) (chains) (boys) (ask me why)
		(please please me) (love me do) (ps i love you)
		(baby its you) (do you want to know a secret)))
(define file2 '((with the beatles) (it wont be long) (all ive got to do)
		(all my loving) (dont bother me) (little child)
		(till there was you) (roll over beethoven) (hold me tight)
		(you really got a hold on me) (i wanna be your man)
		(not a second time)))
(define file3 '((a hard days night) (a hard days night) 
		(i should have known better) (if i fell)
		(im happy just to dance with you) (and i love her)
		(tell me why) (cant buy me love) (any time at all)
		(ill cry instead) (things we said today) (when i get home)
		(you cant do that) (ill be back)))

;; Mapreduce presents files to us in the form of key-value pairs in which the
;; key is the filename and the value is one line, as a sentence.  Here we
;; simulate that:

(define (file->linelist file)
  (map (lambda (line) (make-kv-pair (filename file) line))
       (lines file)))

; >> (file->linelist file1)

(define (wordcounts files)
  (groupreduce + 0 (sort-into-buckets
		    (flatmap (lambda (kv-pair)
			       (map (lambda (wd) (make-kv-pair wd 1))
				    (kv-value kv-pair)))
			     files))))

; >> (wordcounts (append (file->linelist file1)
;			 (file->linelist file2)
;			 (file->linelist file3)))

(define (mostfreq files)
  (accumulate (lambda (new old)
		(cond ((> (kv-value new) (kv-value (car old)))
		       (list new))
		      ((= (kv-value new) (kv-value (car old)))
		       (cons new old))	; In case of tie, remember both.
		      (else old)))
	      (list (make-kv-pair 'foo 0))	; Starting value.
	      (groupreduce + 0 (sort-into-buckets
				(flatmap (lambda (kv-pair)
					   (map (lambda (wd)
						  (make-kv-pair wd 1))
						(kv-value kv-pair)))
					 files)))))

; >> (mostfreq (append (file->linelist file1)
;		       (file->linelist file2)
;		       (file->linelist file3)))


;; Example 3: Searching for a pattern.

; >> (match? '(* i * her *) '(i saw her standing there))
; >> (match? '(* i * her *) '(and i love her))
; >> (match? '(* i * her *) '(ps i love you))

(define (grep pattern files)
  (groupreduce cons '()
	       (sort-into-buckets
		(flatmap (lambda (kv-pair)
			   (if (match? pattern (kv-value kv-pair))
			       (list kv-pair)
			       '()))
			 files))))

; >> (grep '(* i * her *) (append (file->linelist file1)
;				  (file->linelist file2)
;				  (file->linelist file3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Don't read this part, just take it on faith.

(define (sort-into-buckets alist)
  (group (sort-by-key alist)))

(define (group alist)
  (define (help in complete current)
    (cond ((null? in)
	   (reverse (cons (reverse current) complete)))
	  ((null? current)
	   (help (cdr in) complete (cons (car in) current)))
	  ((equal? (kv-key (car in)) (kv-key (car current)))
	   (help (cdr in) complete (cons (car in) current)))
	  (else
	   (help in (cons current complete) '()))))
  (if (null? alist)
      '()
      (help alist '() '())))

(define (sort-by-key alist)
  (define (keyinsert pair alist)
    (cond ((null? alist) (list pair))
	  ((bef? (kv-key pair) (kv-key (car alist)))
	   (cons pair alist))
	  ((equal? (kv-key pair) (kv-key (car alist)))
	   (cons pair alist))
	  (else (cons (car alist) (keyinsert pair (cdr alist))))))
  (if (null? alist)
      '()
      (keyinsert (car alist) (sort-by-key (cdr alist)))))

(define (bef? a b)	; word or sentence version of BEFORE?
  (cond ((word? a) (bef? (se a) b))
	((word? b) (bef? a (se b)))
	((empty? a) (not (empty? b)))
	((empty? b) #f)
	((before? (first a) (first b)) #t)
	((equal? (first a) (first b))
	 (bef? (bf a) (bf b)))
	(else #f)))



(define (match? pattern text)
  (cond ((null? pattern) (null? text))
	((null? text)
	 (and (equal? (car pattern) '*)
	      (match? (cdr pattern) text)))
	((equal? (car pattern) '*)
	 (or (match? (cdr pattern) text)
	     (match? pattern (cdr text))))
	((equal? (car pattern) (car text))
	 (match? (cdr pattern) (cdr text)))
	(else #f)))
