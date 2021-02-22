;;Begin Project 1
(load "adjectives.scm") 
;;Below is some procedures to help you out. Dont worry about what it says or does.
;;Questions are below.
(define bad-chars (string->regexp "[., -'!? ]"))

(define (remove-bad-chars x)
  (regexp-replace-all bad-chars x  ""))

(define (preprocess line)
  (filter (lambda (x) (not (equal? x "")))
          (map (lambda (x)
                 (word
                  (remove-bad-chars (string-lower (& x)))))
               line)))

(define (want-exit? line)
  (or (member? 'exit line) (member? 'quit line) (member? 'bye line)))

(define (print-sentence sent)
  (for-each (lambda (x) (display x) (display " "))
            sent)
  (newline))

(define (interact bot)
  (define (helper)
    (display "> ") (flush)
    (let ((line (preprocess (read-line))))
      (unless (want-exit? line)
              (print-sentence (bot line))
              (helper))))
  (read-line)
  (helper))

(define (chatter bot1 bot2 start iterations)
  (define (helper b1 b2 sent i)
    (when (< i iterations)
          (display "bot ") (display (1+ (remainder i 2))) (display ": ")
          (let ((out (b1 sent)))
            (print-sentence out)
            (helper b2 b1 (preprocess out) (1+ i)))))
  (display "start: ") (print-sentence start)
  (helper bot1 bot2 (preprocess start) 0))

;;; Checks if a given word is an adjective or not
;;; Requires adjectives.scm to be loaded beforehand
(define adjective?
  (let ((hash (make-hash-table)))
    (for-each (lambda (adj)
		(hash-table-put! hash adj #t))
	      adjectives)
    (lambda (wd) (hash-table-get hash wd #f))))


;; Begin Questions:
;;Q1 - babybot
  (define (babybot sent)
    ;;insert your answer here
    (error "not yet implemented")
  )

;;Q2 - stupidbot-creator
  (define (stupidbot-creator sent)
    ;;insert your answer here
    (error "not yet implemented")    
  )

;;Q3 - matcherbot-creator
  (define (matcherbot-creator phrase)
    ;;insert your answer here
    (error "not yet implemented")
  )

;;Q4 - substitutebot-creator
  (define (substitutebot-creator from to)
    ;;insert your answer here
    (error "not yet implemented")
  )

;;Q5 - switcherbot
  (define (switcherbot sent)
    ;;insert your answer here
    (error "not yet implemented")
  )

;;Q6 - inquisitivebot
  (define (inquisitivebot sent)
    ;;insert your answer here
    (error "not yet implemented")
  )
  
;;Q7 - eliza
  (define (eliza sent)
    ;;insert your answer here
    (error "not yet implemented")
  )

;;Q8 - reactorbot-creator
  (define (reactorbot-creator bot pat out)
    ;;insert your answer here
    (error "not yet implemented")
  )

;;Q9 - replacerbot-creator
  (define (replacerbot-creator bot pat before after)
    ;;insert your answer here
    (error "not yet implemented")
  )

;;Q10 - eliza2
  (define eliza2
    ;;insert your answer here. Replace the old definition
    eliza
  )

;;Q11 - randombot-creator
(define (randombot-creator bot1 bot2)
  ;;insert your answer here
    (error "not yet implemented")
  )

;;Q12 - exagerate
(define (exaggerate bot n)
  ;;insert your answer here
    (error "not yet implemented")
  )
;;END OF PROJECT 1
