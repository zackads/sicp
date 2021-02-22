;;; The lazy evaluator with some important procedures traced
;;;
;;; Requres trace.scm, lazy.scm, and by implication, mceval.scm
;;;
;;; For clarity, try untracing actual-value and delay-it.

(load "trace.scm")
(load "lazy.scm")

(trace mc-eval mpp mpp)
(trace mc-apply mpp mpp)
(trace actual-value mpp mpp)
(trace delay-it mpp mpp)
