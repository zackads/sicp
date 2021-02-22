;;; MCE with lots of stuff traced to demonstrate inefficiency
;;; Motivation for analyzing evaluator
;;;
;;; Requires trace.scm and mceval.scm
;;;

(load "trace.scm")
(load "mceval.scm")

(trace mc-eval mpp mpp)
(trace mc-apply mpp mpp)

(trace self-evaluating? mpp mpp)
(trace variable? mpp mpp)
(trace quoted? mpp mpp)
(trace assignment? mpp mpp)
(trace definition? mpp mpp)
(trace if? mpp mpp)
(trace lambda? mpp mpp)
(trace begin? mpp mpp)
(trace cond? mpp mpp)
(trace application? mpp mpp)
