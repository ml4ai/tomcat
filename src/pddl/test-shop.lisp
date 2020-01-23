;; Invocation from command line using SBCL:
;;     sbcl --load test-shop.lisp

(ql:quickload "shop3")
(in-package :shop-user)
(load "sar-individual-domain.pddl")
(load "sar-individual-problem.pddl")
