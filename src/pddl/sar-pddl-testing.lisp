;;;;
;; SAR-PDDL TESTING
;;;;

#+rpg (progn
        (asdf:load-system "shop3")
        (asdf:load-system "shop3/plan-grapher")
        (asdf:load-system :fiveam))

(in-package :shop-user)

(fiveam:def-suite sar-pddl-testing :description "SAR-PDDL Test")
(fiveam:in-suite sar-pddl-testing)

;;;;
;; PDDL SECTION
;;;;

(defun make-domain () (defdomain (sar-individual-domain :type pddl-domain :redefine-ok T) (
    (:types human ;; Everything, including 'human' inherits from the base 'object' type
            victim rescuer - human ;; The rescuer and the victims are humans.
            room ;; Rooms (includes elevators and bathrooms)
            building ;; Building (contains rooms)
    )
   
    (:predicates (in ?h - human ?r - room) ;; Indicates which room a human (rescuer, victim, etc) is inside
                 (inside ?h - rescuer ?b - building) ;; Indicates that the rescuer is in the building
                 (triaged ?v - victim) ;; Indicates that a victim has been triaged
                 (checked-room ?t - rescuer ?r - room) ;; Indicates that a rescuer has checked a room
                 (spotted ?t - rescuer ?v - victim ?r - room) ;; Indicates that a rescuer has spotted the victim in a specific room
                 (examined ?t - rescuer ?v - victim) ;; Indicates that a rescuer has examined a victim
                 (examining ?t - rescuer ?v - victim) ;; Indicates that a rescuer is in the process of examining a victim
                 (injured ?v - victim) ;; Indicates that a victim is injured (to the point of needing triage) 
    )

    (:action check-room ;; Rescuer checks the room, any victims in the room are "spotted" 
      :parameters (?t - rescuer ?r - room)
      :precondition (and (in ?t ?r) (not (checked-room ?t ?r)))
      :effect (and (checked-room ?t ?r) (forall (?v - victim) (when (in ?v ?r) (spotted ?t ?v ?r))))
    )
    
    (:action examine-victim ;; Rescuer begins examining the victim
      :parameters (?t - rescuer ?v - victim)
      :precondition (and (same-room ?t ?v ?r) (spotted ?t ?v ?r) (not (examined ?t ?v)))
      :effect (examining ?t ?v)
    )

    (:action finish-and-goto-next-victim ;; Rescuer finishs examining the current victim and moves to the next
      :parameters (?t - rescuer ?current ?next - victim)
      :precondition (and (same-room ?t ?next ?r) (spotted ?t ?v ?r) (examining ?t ?current) (not (examined ?t ?next)))
      :effect (and (examined ?t ?current) (examining ?t ?next) (not (examining ?t ?current)))
    )

    (:action finish-examinations ;; Rescuer finishs examining the last victim in a room
      :parameters (?t - rescuer ?current - victim)
      :precondition (examining ?t ?current)
      :effect (and (examined ?t ?current) (not (examining ?t ?current)))
    )

    (:action move-to ;; Rescuer moves to another room
      :parameters (?t - rescuer ?source ?destination - room)
      :precondition (and (in ?t ?source) (not (in ?t ?destination)))
      :effect (and (in ?t ?destination) (not (in ?t ?source)))
    )

    (:action triage ;; Rescuer triages a victim, they must be currently examining the victim to do so
      :parameters (?t - rescuer ?v - victim)
      :precondition (and (same-room ?t ?v ?r) (examining ?t ?v) (not (triaged ?v))) 
      :effect (triaged ?v)
    )

    (:action leave-building ;; Rescuer leaves the building
      :parameters (?t - rescuer ?b - building)
      :precondition (inside ?t ?b)
      :effect (not (inside ?t ?b))
    )

    (:action enter-building ;; Rescuer enters the building and is assumed to be in the given room
      :parameters (?t - rescuer ?b - building ?r - room)
      :precondition (not (inside ?t ?b))
      :effect (and (inside ?t ?b) (in ?t ?r))
    )

    (:method (enter-building-and-complete-mission ?t ?b ?r) ;; Method for entering the building and doing the mission
             enter-and-begin
             ()
             (:ordered (:task !enter-building ?t ?b ?r)
                       (:task search-room ?t ?r))

    )

    (:method (search-room ?t ?r) ;; Method for searching a room and determining what needs to be done to move on
             room-checked-victims-found ;; Branch for checking a room with victims inside
             (and (victim ?v) (room ?r2) (in ?t ?r) (same-room ?t ?v ?r) (different ?r ?r2) 
                  (not (checked-room ?t ?r)) (not (checked-room ?t ?r2))) 
             (:ordered (:task !check-room ?t ?r)
                       (:task help-victim ?t ?v)
                       (:task !move-to ?t ?r ?r2)
                       (:task search-room ?t ?r2))

             room-checked-no-victims ;; Branch for checking a room with no victims
             (and (in ?t ?r) (room ?r2) (different ?r ?r2) (not (checked-room ?t ?r)) (not (checked-room ?t ?r2)))
             (:ordered (:task !check-room ?t ?r)
                       (:task !move-to ?t ?r ?r2)
                       (:task search-room ?t ?r2))

             last-room-checked-victims-found ;; Branch for checking the last room with victims inside
             (and (victim ?v) (in ?t ?r) (same-room ?t ?v ?r) (not (checked-room ?t ?r)))
             (:ordered (:task !check-room ?t ?r)
                       (:task help-victim ?t ?v)
                       (:task !leave-building ?t ?b))

             last-room-checked-no-victims ;; Branch for checking the last room with no victims inside
             (and (in ?t ?r) (not (checked-room ?t ?r)))
             (:ordered (:task !check-room ?t ?r)
                       (:task !leave-building ?t ?b))
    )

    (:method (help-victim ?t ?v) ;; Method for examining and triaging victims
             examine-injured-victim-and-move-to-next ;; Branch for examining an injured victim and triaging them
             (and (victim ?v2) (spotted ?t ?v ?r) (different ?v ?v2) (spotted ?t ?v2 ?r) (same-room ?t ?v ?r) (same-room ?t ?v2 ?r) 
                  (injured ?v) (not (examined ?t ?v)) (not (examined ?t ?v2)) (not (triaged ?t ?v)))
             (:ordered (:task !examine-victim ?t ?v)
                       (:task !triage ?t ?v)
                       (:task !finish-and-goto-next-victim ?t ?v ?v2)
                       (:task help-victim ?t ?v2))

             examine-uninjured-victim-and-move-to-next ;; Branch for examining an uninjured victim
             (and (victim ?v2) (spotted ?t ?v ?r) (different ?v ?v2) (spotted ?t ?v2 ?r) (same-room ?t ?v ?r) (same-room ?t ?v2 ?r)
                  (not (examined ?t ?v)) (not (examined ?t ?v2)))
             (:ordered (:task !examine-victim ?t ?v)
                       (:task !finish-and-goto-next-victim ?t ?v ?v2)
                       (:task help-victim ?t ?v2))

             examine-last-injured-victim ;; Branch for examining and triaging the last injured victim in the room
             (and (spotted ?t ?v ?r) (same-room ?t ?v ?r) (injured ?v) (not (examined ?t ?v)) (not (triaged ?t ?v)))
             (:ordered (:task !examine-victim ?t ?v)
                       (:task !triage ?t ?v)
                       (:task !finish-examinations ?t ?v))

             examine-last-uninjured-victim ;; Branch for examining the last uninjured victim in the room
             (and (spotted ?t ?v ?r) (same-room ?t ?v ?r) (not (examined ?t ?v)))
             (:ordered (:task !examine-victim ?t ?v)
                       (:task !finish-examinations ?t ?v))
    )

    (:- (same-room ?t ?v ?r) (and (in ?t ?r) (in ?v ?r))) ;; Axiom that determines that two humans are in the same room, should be used with "different" axoim
    (:- (same ?x ?x) nil) ;; Helper axiom for "different" axoim
    (:- (different ?x ?y) ((not (same ?x ?y)))) ;; Axiom that determines that two variables contain different values. 
  )
))

(defun makeproblem () (defproblem sar-individual-problem ;; Example initial state and task
            ((building b) (room r1) (room r2) (room r3) (rescuer t1) (victim v1) (victim v2) 
                          (victim v3) (victim v4) (victim v5) (injured v2) (injured v3) (injured v5) (in v1 r1) (in v2 r1) (in v3 r2) (in v4 r2) (in v5 r3))
            ((enter-building-and-complete-mission t1 b r1)))
)

;;;;
;; PLAN GENERATION & GRAPH SECTION
;;;;

(defun make-plans () (list (find-plans 'sar-individual-problem :which :all :verbose nil :plan-tree nil)))


(defun make-plan-trees () (let ((plan-trees (nth-value 2
                             (find-plans 'sar-individual-problem 
                                         :which :all 
                                         :verbose :long-plans 
                                         :plan-tree t))))
  (list(cl-dot:print-graph (spg:graph-plan-tree (first plan-trees))))))


(defun multi-graph (id plans)
  (if (null plans)
    'done
    (progn
      (cl-dot:dot-graph (spg:graph-plan-tree (car plans))
                        (format nil "graph~A.pdf" id)
                        :format "pdf")
      (multi-graph (+ id 1) (cdr plans)))))


;;;;
;; TEST SECTION
;;;;

;; For now, we check whether our data exists.

(fiveam:test mk-domain "test defdomain"
             (fiveam:is (not (null (make-domain)))))


(fiveam:test mk-problem "test defproblem"
             (fiveam:is (not (null (makeproblem))))) ;; RENAME: `make-problem' reserved by shop2


(fiveam:test mk-plans "test find-plans"
             (fiveam:is (not (null (make-plans)))))

(fiveam:test mk-plan-trees "test plan-trees"
             (fiveam:is (not (null (make-plan-trees)))))

(fiveam:run! 'sar-pddl-testing)


