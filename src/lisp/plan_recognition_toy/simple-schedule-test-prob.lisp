(progn (ql:quickload "shop3")
       (load "../util.lisp")
       (load "simple-schedule-domain.lisp")
       (setf *random-state* (make-random-state t)))

(in-package :shop-user)
(defparameter *day-state* '((human me)
                            (early-morning)))

(defparameter *day* '((monday me) (monday me) (monday me) (monday me)))

(make-problem 'simple-schedule-problem *day-state* *day*)

(find-plans 'simple-schedule-problem :which :all :optimize-cost nil :verbose :plans :plan-tree nil)
