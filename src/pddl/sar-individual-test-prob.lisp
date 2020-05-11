(progn (ql:quickload "shop3")
       (ql:quickload "shop3/plan-grapher")
       (load "util.lisp")
       (load "sar-individual-domain.lisp"))

(in-package :shop-user)
(defproblem sar-individual-problem ;; Example initial state and task
            ((building b) 
             (room Lobby) 
             (room r-2) 
             (room r-3) 
             (rescuer t1) 
             (victim v1) 
             (victim v2)          
             (victim v3) 
             (victim v4) 
             (victim v5) 
             (injured v2) 
             (injured v3) 
             (injured v5) 
             (in v1 r-1) 
             (in v2 r-1) 
             (in v3 r-2) 
             (in v4 r-2) 
             (in v5 r-3))
            ((enter-building-and-complete-mission t1 b r-1)))

;; Find plans and graph the all.

(let* ((plan-info (multiple-value-list
                (find-plans 'sar-individual-problem :which :all :optimize-cost t :verbose :plans :plan-tree t)))
       (plan-trees (third plan-info)))
  (cl-user::multi-graph 0 plan-trees))
(cl-user::quit)
