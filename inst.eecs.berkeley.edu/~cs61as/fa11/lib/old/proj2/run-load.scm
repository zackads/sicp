;;;; Sanity Check Fall 2000
;;;; CS61A Project 2
;;;; Erik Klavon / erik@eriq.org
;;;; run-load.scm
;;;; this file runs another stk process, so that if the submitted code has
;;;; bugs, we can kill that process, rather than hang if we ran our load and
;;;; tests directly. 

;;;; Definitions

; the number we'll count up to before killing the process.
; adjust to suit the length of tests and system load.
(define wait 100000)

; the path and filename of the scheme program to run
(define prog "~cs61a/lib/proj2/load.scm")

;;;; Code

; this procedure checks our job to see if its alive, and kills it if its too
; old. We exit nonzero if we have the kill the process. If it has compleated,
; we exit with the same status as the process
(define (check num job)
  (if (and (process-alive? job) (< num wait))
      (check (+ num 1) job)
      (let ((status (process-exit-status job)))
	(display status)
	(newline)
	(if status
	    (exit status)
	    (begin
	      (process-kill job)
	      (if (> num (- wait 1))
		  (exit 1)))))))


; run prog asynchronously, pipe any output to /dev/null so it
; doesn't muck up our logs
(define job (run-process "stk" "-no-tk" "-file" prog :error "/dev/null" :output "/dev/null" :wait #f))

; check it, kill if its too stale
(check 0 job)

; exit just in case with 0
(exit)




