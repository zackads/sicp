#|
ADT for making a key value pair
|#
(define make-kv-pair cons)
(define kv-key car)
(define kv-value cdr)


#|
A non-parallel implementation for mapreduce.
Note that the actual mapreduce will exploit parallelism wherever possible. (e.g. parallelizing the mapper and reducer)
|#
(define (mapreduce mapper reducer base-case data)
	(groupreduce reducer base-case (sort-into-buckets (map mapper data))))

#|
(1) sort-into-buckets:
Takes a list of list. The inner list contains key-value pairs.
It returns a new list of lists where the inner list contains key-value pairs with the same keys

Arguments:
    deeplst: list of lists

Returns:
    list of lists

Example:
sTK> (sort-into-buckets '( ((i . 1) (want . 1) (to . 1)(be . 1))
			   ((i . 1) (want . 1) (to . 1)(see . 1))
			   ((much . 1) (want . 1) (very . doge)) ))

(((i . 1) (i . 1))
 ((want . 1) (want . 1) (want . 1))
 ((to . 1) (to . 1))
 ((be . 1))
 ((see . 1))
 ((much . 1))
 ((very . doge)))

|#

(define (sort-into-buckets deeplst)
  (group (accumulate append '() deeplst))
  )

#|
A Helper function to sort-into-buckets.
Takes a list of key-value pair and groups key-value pair with the same key under one list (bucket).

The output is a list of lists where the inner list is a list of key-value pairs with the same keys

Arguments:
    lst: list of key-value pairs

Returns:
    List of lists
|#
(define (group lst)
  (if (null? lst)
      nil
      (let* ( (key (kv-key (car lst)))
	      (bucket (filter (lambda (kv) (equal? key (kv-key kv))) lst))    ;list of key-values with the same keys
	      (rest   (filter (lambda (kv) (not (equal? key (kv-key kv)))) lst))
	      )
	(cons bucket (group rest)))))





#|
(2) groupreduce:
Takes a reducer, base case and buckets. Buckets is a list of lists where the inner list is a key-value pair with the same keys

Arguments:
    reducer:   Function of 2 arguments
    base-case: Should have the same type as the output of reducer
    buckets:   List of lists

Returns:
    List of key-value pairs

|#
(define (groupreduce reducer base-case buckets)
	(map (lambda (bucket) (reduce-bucket reducer base-case bucket))
             buckets))


#|
Arguments:
    reducer:   Function of 2 arguments
    base-case: Should have the same type as the output of reducer
    bucket:    List of key-value-pairs

Returns:
    A key-value pair

|#
(define (reduce-bucket reducer base-value bucket)
	(make-kv-pair	(kv-key (car bucket))
			(accumulate reducer base-value (map kv-value bucket))))

#|
(3) sort-by-key
Takes a list of key-value pairs and returns a list sorted by key

Arguments:
    strm: list of key-value pairs

Returns:
    List of key-value pairs

|#
(define (sort-by-key lst)
  (if (equal? lst nil)
      nil
      (let* ((smallest (accumulate generic-min (car lst) (cdr lst)))
	     (rest (filter (lambda (kv) (not (equal? kv smallest)))
			   lst)))
	(cons smallest (sort-by-key rest))))
)


(define (generic-min x y)
  (cond ((number? x) (min x y))
	(else "error type unknown")))

#|
 Example input, mapper and reducer for our mapreduce. Duplicated from corresponding EdX courseware.
A list of key-value pairs where the key is the song title and the value is a line from the song
|#
(define song1   '( ((please please me) i saw her standing there)
		   ((please please me) misery)
		   ((please please me) please please me)))

(define song2	 '( ((with the beatles) it wont be long)
		    ((with the beatles) all i have got to do)
		    ((with the beatles) all my loving)))

(define song3 	 '( ((a hard days night) a hard days night)
		    ((a hard days night) i should have known better)
		    ((a hard days night) if i fell)))

(define all-songs (append song1 song2 song3))



(define (mapper input-kv-pair)
  (map (lambda (wd) (make-kv-pair wd 1)) (kv-value input-kv-pair)))


(define (reducer num other-num)
	(+ num other-num))
