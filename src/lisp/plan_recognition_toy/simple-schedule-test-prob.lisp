(progn (ql:quickload "shop3")
       (load "../util.lisp")
       (load "simple-schedule-domain.lisp")
       (setf *random-state* (make-random-state t)))

(in-package :shop-user)
(defparameter *day-state* '((human me)
                            (raining)
                            (have-homework)
                            (work-today)))

(defparameter *day* '((monday me)))

(make-problem 'simple-schedule-problem *day-state* *day*)

(find-plans 'simple-schedule-problem :which :all :optimize-cost nil :verbose :plans :plan-tree t)
