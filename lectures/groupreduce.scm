; Pseudo-mapreduce on one processor to illustrate the concept early.

(define make-kv-pair cons)
(define kv-key car)
(define kv-value cdr)

(define (groupreduce fn base buckets)
  (map (lambda (subset)
	 (cons (kv-key (car subset))
	       (accumulate fn base (map kv-value subset))))
       buckets))

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
	   (help in (cons (reverse current) complete) '()))))
  (if (null? alist)
      '()	; Return (), not (()), if no data at all.
      (help alist '() '())))

(define (sort-by-key alist)
  (sort alist super-before?))

(define (super-before? a b)
  (cond ((and (number? (kv-key a)) (number? (kv-key b)))
	 (<= (kv-key a) (kv-key b)))
	((and (word? (kv-key a)) (word? (kv-key b)))
	 (not (before? (kv-key b) (kv-key a))))
	((and (pair? (kv-key a)) (pair? (kv-key b)))
	 (and (not (before? (car (kv-key b)) (car (kv-key a))))
	      (not (super-before? (cdr (kv-key b)) (cdr (kv-key a))))))
	(else #t)))	; punt if not comparable

(define filename car)
(define lines cdr)

(define (file->linelist file)
  (map (lambda (line) (make-kv-pair (filename file) line))
       (lines file)))

(define (match? pattern sent)
  (cond ((null? pattern) (null? sent))
	((null? sent) (and (eq? (car pattern) '*)
			   (match? (cdr pattern) sent)))
	((eq? (car pattern) '*) (or (match? (cdr pattern) sent)
				    (match? pattern (cdr sent))))
	((eq? (car pattern) (car sent)) (match? (cdr pattern) (cdr sent)))
	(else #f)))

(define (file->linelist file)
  (map (lambda (line) (make-kv-pair (filename file) line))
       (lines file)))

; Some sample data

(define mt1 '((cs61a-xc . 27) (cs61a-ya . 40) (cs61a-xw . 35)
	      (cs61a-xd . 38) (cs61a-yb . 29) (cs61a-xf . 32)))
(define mt2 '((cs61a-yc . 32) (cs61a-xc . 25) (cs61a-xb . 40)
	      (cs61a-xw . 27) (cs61a-yb . 30) (cs61a-ya . 40)))
(define mt3 '((cs61a-xb . 32) (cs61a-xk . 34) (cs61a-yb . 30)
	      (cs61a-ya . 40) (cs61a-xc . 28) (cs61a-xf . 33)))

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

; demos

; (sort-into-buckets (append mt1 mt2 mt3))

; (groupreduce + 0 (sort-into-buckets (append mt1 mt2 mt3)))

'(groupreduce (lambda (new old) (+ 1 old)) 0
	      (sort-into-buckets (append mt1 mt2 mt3)))

; (map (lambda (wd) (list (make-kv-pair wd 1))) '(cry baby cry))

(define (wordcounts1 sent)
  (groupreduce + 0 (sort-into-buckets (map (lambda (wd) (make-kv-pair wd 1))
					   sent))))

; (wordcounts1 '(cry baby cry))

; (file->linelist file1)

(define (wordcounts files)
  (groupreduce + 0 (sort-into-buckets
		    (flatmap (lambda (kv-pair)
			       (map (lambda (wd) (make-kv-pair wd 1))
				    (kv-value kv-pair)))
			     files))))

'(wordcounts (append (file->linelist file1)
		     (file->linelist file2)
		     (file->linelist file3)))

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

'(mostfreq (append (file->linelist file1)
		   (file->linelist file2)
		   (file->linelist file3)))

; (match? '(* i * her *) '(i saw her standing there))
; (match? '(* i * her *) '(and i love her))
; (match? '(* i * her *) '(ps i love you))

(define (grep pattern files)
  (groupreduce cons '()
	       (sort-into-buckets
		(flatmap (lambda (kv-pair)
			   (if (match? pattern (kv-value kv-pair))
			       (list kv-pair)
			       '()))
			 files))))

'(grep '(* i * her *) (append (file->linelist file1)
			      (file->linelist file2)
			      (file->linelist file3)))

