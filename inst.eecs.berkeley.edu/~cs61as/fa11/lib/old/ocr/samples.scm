;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; samples.scm
;; - image support file for CS61A Machine Learning Project
;; - Author: Min Xu
;; - based on OCR Classification project by Prof. Dan Klein
;;
;; You DO NOT need to understand this file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;


(load "trainingimages")
(load "traininglabels")

(load "validationimages")
(load "validationlabels")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMAGES -- contains a string of all image samples
;; LABELS -- contains a string of all correct labels
;; Each image: 28 x 28 characters.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define WIDTH 29)
(define HEIGHT 28)
(define IMG-DIM 28)
(define MAX-INDEX (string-length IMAGES))

;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN Labeled Image ADT

(define (make-labeled-image vec label)
  (list 'image vec label))

(define (get-img-label img)
  (if (not (is-img? img))
      (error "Not an image")
      (caddr img)))

(define (is-img? img)
  (and (list? img)
       (eq? (car img) 'image)))

(define (get-xy x y img)
  (if (not (is-img? img))
      (error "Not an image")
      (vector-ref (cadr img) (+ x (* IMG-DIM y)))))

;;;; END labeled image ADT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (char->number char)
  (cond ((eq? char #\0) 0)
	((eq? char #\1) 1)
	((eq? char #\2) 2)
	((eq? char #\3) 3)
	((eq? char #\4) 4)
	((eq? char #\5) 5)
	((eq? char #\6) 6)
	((eq? char #\7) 7)
	((eq? char #\8) 8)
	((eq? char #\9) 9)
	(else (error "NOT a number string" char))))

(define (process-images img-index label-index image-str label-str)
  (if (= img-index MAX-INDEX) 'the-empty-stream
      (cons-stream
       (make-labeled-image (img->vector img-index image-str)
			   (char->number (string-ref label-str label-index)))
       (process-images (+ img-index (* WIDTH HEIGHT)) (+ 2 label-index) image-str label-str))))


(define (img->vector start-index str)
  (let ((result (make-vector (* IMG-DIM IMG-DIM) 0)))
    (define (loop pixel-n result-m)
      (if (= pixel-n (* WIDTH HEIGHT))
	  result
	  (let ((cur-char (string-ref str (+ start-index pixel-n))))
	    (cond ((eq? cur-char #\space) (loop (+ pixel-n 1) (+ result-m 1)))
		  ((eq? cur-char #\+) (vector-set! result result-m 1) (loop (+ pixel-n 1) (+ 1 result-m)))
		  ((eq? cur-char #\#) (vector-set! result result-m 2) (loop (+ pixel-n 1) (+ 1 result-m)))
		  ((eq? cur-char #\newline) (loop (+ pixel-n 1) result-m))))))
    (loop 0 0)))


(define (pretty-print img)
  (if (not (is-img? img))
      (error "Not an IMAGE")
      (begin
	(define (loop n)
	  (if (= n (* IMG-DIM IMG-DIM))
	      (display "")
	      (begin
		(if (= 0 (remainder n IMG-DIM)) (newline))
		(let ((cur-char (get-xy (remainder n IMG-DIM)
					(div n IMG-DIM) img)))
		  (cond ((eq? cur-char 0) (display #\.))
			((eq? cur-char 1) (display #\+))
			((eq? cur-char 2) (display #\#))))
		(loop (+ n 1)))))
	(loop 0))))
	      
(define img-streams (process-images 0 0 IMAGES LABELS))

(define vimg-stream (process-images 0 0 V-IMAGES V-LABELS))
