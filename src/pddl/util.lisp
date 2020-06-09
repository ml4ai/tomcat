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
(defun load-json-database (filename)
  (with-open-file (s (make-pathname :name filename) :direction :input)
    (json:decode-json s)))

;; get cost of shortest path
(defun shortest-path-cost (var loc-x loc-y)
  (rest (assoc-if #'(lambda(x) (symbol-equals-keyword x loc-y)) 
                  (rest (assoc-if #'(lambda(x) (symbol-equals-keyword x loc-x)) 
                                  var)))))

(defun check-for-victim (query)
  (let* ((vic-db (load-json-database "sar_victim_status.json"))
         (v-list (remove-if #'(lambda(x) (not (equalp (cdr (second x)) (symbol-name query)))) vic-db)))
    (loop for i in v-list
          collect (first i))))

(defun is-injured (query)
  (let ((vic-db (load-json-database "sar_victim_status.json")))
    (or (equal "severe" (rest (third (assoc query vic-db)))) (equal "mild" (rest (third (assoc query vic-db)))))))

;; See if list of symbols are equal by name alone (regardless of package)
(defun equall (l1 l2)
  (cond
    ((and (null l1) (null l2)) t)
    ((symbol-equals-keyword (car l1) (car l2)) (equall (cdr l1) (cdr l2)))
    (t nil)))


;; member function that works on lists of lists
(defun memberl (x l)
   (cond
      ((null l) ())
      ((equall (car l) x) l)
      (t (memberl x (cdr l)))))

;; treats lists as sets and checks to see if first argument is subset of the
;; other 
(defun subset-of (l1 l2) 
   (cond 
      ((null l1) t) 
      ((memberl (car l1) l2) (subset-of (cdr l1) l2)) 
      (t ())))

;; checks to see if the first argument contains the same elements as the second
;; argument regardless of order. 
(defun equal-lists (l1 l2)
   (and (subset-of l1 l2) (subset-of l2 l1)))
