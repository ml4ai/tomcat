;; This contains various utility functions needed for SAR domain

(progn (ql:quickload "shop3")
       (ql:quickload "shop3/plan-grapher")
       (ql:quickload "cl-json")
       (ql:quickload "queues")
       (require :queues.simple-queue))

(defun bulk-copy (infile outfile)
  (with-open-file (instream infile :direction :input :element-type '(unsigned-byte 8)
                            :if-does-not-exist nil)
    (when instream
      (with-open-file (outstream outfile :direction :output :element-type '(unsigned-byte 8)
                                 :if-exists :supersede)
        (let ((buffer (make-array 8192 :element-type '(unsigned-byte 8))))
          (loop for bytes-read = (read-sequence buffer instream)
                while (plusp bytes-read)
                do (write-sequence buffer outstream :end bytes-read)))))))

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
    (nshuffle (loop for i in v-list
          collect (first i)))))

(defun severely-injured (query)
  (let ((vic-db (load-json-database "sar_victim_status.json")))
    (equal "severe" (rest (third (assoc query vic-db))))))

(defun still-searching (c)
  (< (random 1.0) 
     (if (= c 0) 0.5 (* .6 (/ 1.0 c)))))

(defun triage-successful (prob)
  (< (random 1.0) prob))

(defun found-victim (prob)
  (< (random 1.0) prob))

(defun triage-cost (victim success)
  (if (severely-injured victim)
    (if success
      15
      (random 15))
    (if success
      8
      (random 8))))

(defun nshuffle (sequence)
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)

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

(defun compare-with-goal (current-state goal)
  (equal-lists current-state (with-open-file 
                               (s (make-pathname :name 
                                                 (string-downcase 
                                                   (symbol-name goal))) :direction :input) 
                               (read s))))

(defun create-room-queue (filename)
  (queue-up-list (with-open-file 
                   (s (make-pathname :name filename) :direction :input) 
                   (read s))))


(defun queue-up-list (lst)
  (let ((q (queues::make-queue ':simple-queue)))
    (loop for x in lst do (queues::qpush q x))
     q))

(defun load-object-from-file (filename)
  (with-open-file
    (s (make-pathname :name filename) :direction :input)
    (read s)))

(defun get-next-room (current-room room-list)
  (first (cdr (member-if #'(lambda (x) (symbol-equals-keyword x current-room)) room-list))))

(defun add-to-room-list (roomname filename)
  (let ((v (with-open-file (instream filename :direction :input
                                     :if-does-not-exist nil)
             (if (not instream)
               (list roomname)
               (append (read instream) (list roomname))))))
    (with-open-file (outstream filename :direction :output
                               :if-exists :supersede)
      (format outstream "~a~%" v))
    t))

(defun remove-from-room-list (roomname filename)
  (let ((v (with-open-file (instream filename :direction :input
                                     :if-does-not-exist nil)
             (if (not instream)
               (list roomname)
               (remove roomname (read instream))))))
    (with-open-file (outstream filename :direction :output
                               :if-exists :supersede)
      (format outstream "~a~%" v))
    t))

(defun split-by-one-space (string)
    "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them.
Found on Common Lisp Cookbook"
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

(defun match-action-headers (act1 act2)
  (equal (car (split-by-one-space act1)) (car (split-by-one-space act2))))

(defun get-applicable-tasks-helper (action tasks)
  (loop for x in tasks
         collect (list (cdr (assoc :header x)) 
                       (loop for y in (cdr (assoc :methods x))
                             when (member-if #'(lambda (x) (match-action-headers x action))
                                             (cdr (assoc :tasks y)))
                             collect (list (cdr (assoc :header y)) (cdr (assoc :tasks y)))))))

(defun get-applicable-tasks (action tasks)
  (remove-if #'(lambda (x) (not (second x))) (get-applicable-tasks-helper action tasks)))

(defun find-leading-actions (action action-set)
  (reverse (member-if #'(lambda (x) (match-action-headers x action)) (reverse action-set))))

(defun powerset (l)
  (if (null l)
      (list nil)
      (let ((prev (powerset (cdr l))))
	(append (mapcar #'(lambda (elt) (cons (car l) elt)) prev)
		prev))))
