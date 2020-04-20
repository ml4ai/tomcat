;; This contains various utility functions needed for SAR domain

(progn (ql:quickload "shop3")
       (ql:quickload "shop3/plan-grapher")
       (ql:quickload "cl-json"))

;; This takes a starting id and a list of plans and graphs each one and assigns
;; it an id (incrementing by 1 from the starting id). The graphs are saved as
;; pdfs in the current directory.
(defun multi-graph (id plans)
  (if (null plans)
    'done
    (progn
      (cl-dot:dot-graph (spg:graph-plan-tree (car plans))
                        (format nil "graph~A.pdf" id)
                        :format "pdf")
      (multi-graph (+ id 1) (cdr plans)))))

;; This is a helper function for looking up keys in the shortest path json
;; file. This returns T if given a symbol and a keyword that have the same
;; name. Would also return T if two symbols with same name or two keywords with
;; same name. 
(defun symbol-equals-keyword (x y)
  (equal (symbol-name x) (symbol-name y)))

;; Load json file of shortest path costs
(defun load-shortest-paths (filename)
  (with-open-file (s (make-pathname :name filename) :direction :input)
    (json:decode-json s)))

;; get cost of shortest path
(defun shortest-path-cost (var loc-x loc-y)
  (rest (assoc-if #'(lambda(x) (symbol-equals-keyword x loc-y)) 
                  (rest (assoc-if #'(lambda(x) (symbol-equals-keyword x loc-x)) 
                                  var)))))
