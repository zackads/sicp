;;; The analyzing evaluator with some important procedures traced
;;;
;;; Requres trace.scm, analyze.scm, and by implication, mceval.scm
;;;

(load "trace.scm")
(load "analyze.scm")

(trace mc-eval mpp mpp)
(trace analyze mpp mpp)
(trace execute-application mpp mpp)
