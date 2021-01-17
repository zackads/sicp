;; CS 61A Mapreduce lecture demos

;; Count how many times any word is used in the title of a Beatles song:

(define (wordcount-mapper document-line-kv-pair)
  (map (lambda (wd-in-line) (make-kv-pair wd-in-line 1))
       (kv-value document-line-kv-pair)))

(define wordcounts (mapreduce wordcount-mapper + 0 "/beatles-songs"))

(ss wordcounts)

;; How to examine a distributed file

(ss (mapreduce list cons-stream the-empty-stream "/beatles-songs"))

;; Find the most commonly used word in any Beatles title.
;; (This actually gets the most commonly used word per initial letter;
;; another reduction on a single machine is needed to get the absolutely
;; most commonly used word.)

(define (find-max-mapper kv-pair)
  (list (make-kv-pair (first (kv-key kv-pair))
		      kv-pair)))

(define (find-max-reducer current so-far)
  (if (> (kv-value current) (kv-value so-far))
      current
      so-far))

(define frequent (mapreduce find-max-mapper find-max-reducer
			    (make-kv-pair 'foo 0) wordcounts))

(ss frequent)

(stream-accumulate find-max-reducer (make-kv-pair 'foo 0)
		   (stream-map kv-value frequent))

;; Find the total number of lines of text in all of Shakespeare:

(define will (mapreduce (lambda (kv-pair) (list (make-kv-pair 'line 1)))
			+ 0 "/gutenberg/shakespeare"))


;; This is the desired result from the modification of the above program
;; to find line counts per play.

'((a-midsummer-nights-dream . 2456) (alls-well-that-ends-well . 3231)
  (henry-v . 3643) (loves-labours-lost . 3015) (macbeth . 2900)
  (the-taming-of-the-shrew . 3033) (the-tempest . 2644) (king-john . 3028)
  (the-merry-wives-of-windsor . 3152) (the-winters-tale . 3595)
  (timon-of-athens . 2852) (troilus-and-cressida . 3983)
  (as-you-like-it . 2974) (cymbeline . 4164) (henry-iv-1 . 3355)
  (henry-vi-1 . 3409) (king-lear . 3985) (sonnets . 2626)
  (the-tragedy-of-antony-and-cleopatra . 4175)
  (the-tragedy-of-coriolanus . 4277) (the-two-gentlemen-of-verona . 2559)
  (a-lovers-complaint . 385) (hamlet . 4553) (henry-iv-2 . 3587)
  (henry-vi-2 . 3646) (measure-for-measure . 3119) (othello . 3897)
  (richard-iii . 4541) (twelfth-night . 2801) (henry-vi-3 . 3524)
  (henry-viii . 3765) (much-ado-about-nothing . 2789)
  (romeo-and-juliet . 3620) (the-duke-of-venice . 2968))
