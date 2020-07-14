(progn (ql:quickload "shop3")
       (load "util.lisp")
       (bulk-copy "sar-room-list-original.txt" "sar-room-list.txt")
       (load "sar-individual-domain.lisp")
       (setf *random-state* (make-random-state t)))

(in-package :shop-user)
(defparameter *sar-state* '((room lobby) 
                          (room mens) 
                          (room womens) 
                          (room e-1) 
                          (room e-2) 
                          (room main-Intersection) 
                          (room mid-left-bottom-hallway) 
                          (room far-left-bottom-hallway) 
                          (room lower-left-hallway) 
                          (room upper-left-hallway) 
                          (room r-j) 
                          (room r-201) 
                          (room r-208-a) 
                          (room r-203) 
                          (room r-208-b) 
                          (room r-205) 
                          (room r-210) 
                          (room r-207) 
                          (room lower-center-hallway) 
                          (room upper-center-hallway) 
                          (room mid-right-bottom-hallway) 
                          (room far-right-bottom-hallway) 
                          (room lower-right-hallway) 
                          (room upper-right-hallway) 
                          (room r-209) 
                          (room r-216-a) 
                          (room r-211) 
                          (room r-216-b) 
                          (room r-213) 
                          (room r-218) 
                          (room r-215) 
                          (room r-220) 
                          (rescuer t1)  
                          (in t1 lobby)))

(defparameter *task-type* 'perform-next-task-that-prioritizes-yellow-victims)

(defparameter *sar-task* (list (list *task-type* 't1 'lobby)))

(defparameter *mission-ongoing* t)

(make-problem 'sar-individual-problem *sar-state* *sar-task*)

(with-open-file 
  (outstream "sar-plan-trees.txt" :direction :output :if-exists :supersede) 
  (format outstream "~a~%" 
          (list (append '((complete-mission t1)) (loop while *mission-ongoing* collect 
                (first (let ((current-plan (multiple-value-list 
                                      (find-plans 'sar-individual-problem :which :first :optimize-cost nil :verbose nil :plan-tree t)))) 
                  (setf *sar-state* (state-atoms (first (fourth current-plan)))) 
                  (if (eql '!change-strategy (first (first (first (first current-plan)))))
                    (progn 
                      (setf *task-type* 'perform-next-task-without-victim-priority)
                      (setf *sar-task* (list (list *task-type*
                                                   (second (first *sar-task*))
                                                   (third (first *sar-task*)))))))
                  (if (eql '!move (first (first (first (first current-plan))))) 
                    (setf *sar-task* (list (list *task-type* 
                                                 (second (first (first (first current-plan)))) 
                                                 (fourth (first (first (first current-plan)))))))) 
                (if (eql '!end-mission (first (first (first (first current-plan))))) 
                  (setf *mission-ongoing* nil))
                (make-problem 'sar-individual-problem *sar-state* *sar-task*) 
                (third current-plan))))))))

;;(print (eql '!move (first (first (first (first (multiple-value-list (find-plans 'sar-individual-problem :which :first :optimize-cost nil :verbose nil :plan-tree t))))))))
