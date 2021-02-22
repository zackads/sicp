;; (MOSTLY) TESTED CODE - Proof of concept for a chatterbot-themed project 1.

;; Simple bots.

;; Stupid bot takes a sentence X, and always says X, no matter what. 
(define (stupid-bot x)
  (lambda (sent) x))

;; Like a child, repeats whatever you say
(define (repeater sent)
  sent)

(define (questioner sent)
  (se (switch-viewpoint sent) '?))

;; Takes two sentences: SENT and PHRASE
;; If PHRASE is in SENT, returns whatever follows PHRASE (first time PHRASE appears)
;; If not, returns #f
;; Given empty PHRASE, return SENT.
;; Examples:
;; (pattern-match '(i feel tired) '(i feel)) => '(tired)
;; (pattern-match '(brains are neat) '(i feel)) => #f
;; (pattern-match '(i came i saw i conquered) '(i)) => '(came i saw i conquered)

(define (pattern-match-beg sent phrase)
  (cond ((empty? phrase) sent)
        ((empty? sent) #f)
        ((eq? (first sent) (first phrase))
         (pattern-match-beg (bf sent) (bf phrase)))
        (else #f)))

(define (pattern-match sent phrase)
  (cond ((pattern-match-beg sent phrase) (pattern-match-beg sent phrase))
        ((empty? sent) #f)
        (else (pattern-match (bf sent) phrase))))


;; Similar to substitute

(define (sub-one wd from to)
  (cond ((empty? from) wd)
        ((eq? wd (first from)) (first to))
        (else (sub-one wd (bf from) (bf to)))))

(define (substitute-many sent from to)
  (every (lambda (w) (sub-one w from to)) sent))

(define (switch-viewpoint sent)
  (substitute-many sent
                   '(am was i my are your yours you me)
                   '(are were you your am my mine i you)))


(define (eliza sent)
  (cond ((pattern-match sent '(hello))
         '(hello there!))
        ((pattern-match sent '(i am))
         (se '(how long have you been)
             (switch-viewpoint (pattern-match sent '(i am)))
             '?))
        (else (se (switch-viewpoint sent) '?))))


;; Takes a chatterbot BOT, sentence pat, and sentence OUT
;; Gives a new chatterbot that works just like BOT,
;; except when PAT is present in the sentence.
;; Then, it says OUT.
(define (reacter bot pat out)
  (lambda (sent)
    (if (pattern-match sent pat)
        out
        (bot sent))))

;; Like REACT, but instead of just returning OUT,
;; also appends what pattern-match returns, in between BEFORE and AFTER.
(define (react-end bot pat before after)
  (lambda (sent)
    (if (pattern-match sent pat)
        (se before (switch-viewpoint (pattern-match sent pat)) after)
        (bot sent))))


(define eliza2
  (react-end
   (reacter questioner '(hello) '(hello there!))
   '(i am)
   '(why are you)
   '(?)))

;; Following should be given.
;; Just nice to have an interact function =D
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
