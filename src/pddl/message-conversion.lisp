(progn (ql:quickload "cl-json")
(load "util.lisp"))

(defun initialize-current-goal (goalFile)
  (bulk-copy goalFile "sar-individual-current-goal.txt"))

(defun convert-message (message)
  (let ((m (rest (assoc :DATA message))))
    (if 
      (equal "SUCCESSFUL" (rest (assoc :TRIAGE--STATE m)))
      (with-input-from-string (s (format nil "((TRIAGED V-~a))" 
                                        (sxhash (list 
                                                  (rest (assoc :VICTIM--X m)) 
                                                  (rest (assoc :VICTIM--Y m)) 
                                                  (rest (assoc :VICTIM--Z m)))))) (read s)))))

(defun add-event-to-state (message)
  (let ((v (with-open-file (instream "sar-individual-current-goal.txt" :direction :input 
                                     :if-does-not-exist nil) 
             (append (read instream) (convert-message message))))) 
    (with-open-file (outstream "sar-individual-current-goal.txt" :direction :output 
                               :if-exists :supersede)
      (format outstream "~a~%" v))))

(defun listen-for-messages ()
  (loop while t do
        (add-event-to-state (json:decode-json *standard-input*))))
