;; This contains various utility functions needed for SAR domain

(progn (ql:quickload "shop3")
       (ql:quickload "shop3/plan-grapher"))

(defun multi-graph (id plans)
  (if (null plans)
    'done
    (progn
      (cl-dot:dot-graph (spg:graph-plan-tree (car plans))
                        (format nil "graph~A.pdf" id)
                        :format "pdf")
      (multi-graph (+ id 1) (cdr plans)))))
