(progn (ql:quickload "shop3")
       (load "../util.lisp")
       (load "simple-schedule-domain.lisp")
       (setf *random-state* (make-random-state t)))

(in-package :shop-user)

(loop for i in '((monday me) (tuesday me) (wednesday me) (thursday me) (friday me))
      do (loop for j in (cl-user::powerset '((raining) (have-homework) (work-today) (need-groceries) (found-movie)))
               do (progn (make-problem 'simple-schedule-problem (append '((human me)) j) (list i)) 
                         (find-plans 'simple-schedule-problem :which :all :optimize-cost nil :verbose :plans :plan-tree t))))
