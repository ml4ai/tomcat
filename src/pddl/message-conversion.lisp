(progn (ql:quickload "cl-json")
(load "util.lisp"))

(defun initialize-current-goal (goalFile)
  (bulk-copy goalFile "sar-individual-current-goal.txt"))

(defun convert-message (message)
  (let ((m (load-json-database message)))
    (if 
      (and (equal "edu.arizona.tomcat.Events.BlockBreakEvent" (rest (assoc :EVENT--TYPE m)))
           (or (equal "minecraft:prismarine" (rest (assoc :BLOCK--MATERIAL m)))
               (equal "minecraft:gold_block" (rest (assoc :BLOCK--MATERIAL m)))))
      (with-input-from-string (s (format nil "((TRIAGED V-test))")) (read s)))))

(defun add-event-to-state (message)
  (let ((v (with-open-file (instream "sar-individual-current-goal.txt" :direction :input 
                                     :if-does-not-exist nil) 
             (append (read instream) (convert-message message))))) 
    (with-open-file (outstream "sar-individual-current-goal.txt" :direction :output 
                               :if-exists :supersede)
      (format outstream "~a~%" v))))
