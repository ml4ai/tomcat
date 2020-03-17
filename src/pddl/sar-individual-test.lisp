;; Grounded STRIPS-style domains are unsupported within SHOP3.
;; SHOP3 very-loosely parses PDDL domains so as to incorporate PDDL constructs
;; but does not strictly conform to PDDL-syntax.

;; Our Domain Definitions will use SHOP3 Syntax, which provides a preliminary
;; capability to incorporate PDDL domain definitions into a SHOP3 domain.
;; PDDL-domains corresponds to the ADL PDDL-type, and it uses conditional-effects,
;; existential preconditions, universal preconditions.

;; SHOP3 domains contain Operators, Methods, and Axioms, and type:pddl-domain
;; provides support for actions typing, equality, disjunctions, quantifiers,
;; and conditional-effects

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

(in-package :shop-user)
;(shop-trace :all)
(defdomain (sar-individual-domain :type pddl-domain :redefine-ok T) (
    (:types human ;; Everything, including 'human' inherits from the base 'object' type
            victim rescuer - human ;; The rescuer and the victims are humans.
            room ;; Rooms (includes elevators and bathrooms)
            building
    )
   
    (:predicates (in ?h - human ?r - room)
                 (inside ?h - human ?b - building)
                 (triaged ?v - victim)
                 (checked-room ?t - rescuer ?r - room)
                 (spotted ?t - rescuer ?v - victim ?r - room)
                 (examined ?t - rescuer ?v - victim)
                 (examining ?t - rescuer ?v - victim)
                 (injured ?v - victim)
    )

    (:action check-room
      :parameters (?t - rescuer ?r - room)
      :precondition (and (in ?t ?r) (not (checked-room ?t ?r)))
      :effect (and (checked-room ?t ?r) (forall (?v - victim) (when (in ?v ?r) (spotted ?t ?v ?r))))
    )
    
    (:action examine-victim
      :parameters (?t - rescuer ?v - victim)
      :precondition (and (same-room ?t ?v ?r) (spotted ?t ?v ?r) (not (examined ?t ?v)))
      :effect (examining ?t ?v)
    )

    (:action finish-and-goto-next-victim
      :parameters (?t - rescuer ?current ?next - victim)
      :precondition (and (same-room ?t ?next ?r) (spotted ?t ?v ?r) (examining ?t ?current) (not (examined ?t ?next)))
      :effect (and (examined ?t ?current) (examining ?t ?next) (not (examining ?t ?current)))
    )

    (:action finish-examinations 
      :parameters (?t - rescuer ?current - victim)
      :precondition (examining ?t ?current)
      :effect (and (examined ?t ?current) (not (examining ?t ?current)))
    )

    (:action move-to
      :parameters (?t - rescuer ?source ?destination - room)
      :precondition (and (in ?t ?source) (not (in ?t ?destination)))
      :effect (and (in ?t ?destination) (not (in ?t ?source)))
    )

    (:action triage
      :parameters (?t - rescuer ?v - victim)
      :precondition (and (same-room ?t ?v ?r) (examining ?t ?v) (not (triaged ?v))) 
      :effect (triaged ?v)
    )

    (:action leave-building
      :parameters (?t - rescuer ?b - building)
      :precondition (inside ?t ?b)
      :effect ((not (inside ?t ?b)))
    )

    (:action enter-building
      :parameters (?t - rescuer ?b - building ?r - room)
      :precondition (not (inside ?t ?b))
      :effect (and (inside ?t ?b) (in ?t ?r))
    )

    (:method (enter-building-and-begin-mission ?t ?b ?r)
             enter-and-begin
             ()
             (:ordered (:task !enter-building ?t ?b ?r)
                       (:task search-room ?t ?r))

    )

    (:method (search-room ?t ?r) 
             room-checked-victims-found
             (and (victim ?v) (room ?r2) (in ?t ?r) (same-room ?t ?v ?r) (different ?r ?r2) 
                  (not (checked-room ?t ?r)) (not (checked-room ?t ?r2))) 
             (:ordered (:task !check-room ?t ?r)
                       (:task help-victim ?t ?v)
                       (:task !move-to ?t ?r ?r2)
                       (:task search-room ?t ?r2))

             room-checked-no-victims 
             (and (in ?t ?r) (room ?r2) (different ?r ?r2) (not (checked-room ?t ?r)) (not (checked-room ?t ?r2)))
             (:ordered (:task !check-room ?t ?r)
                       (:task !move-to ?t ?r ?r2)
                       (:task search-room ?t ?r2))

             last-room-checked-victims-found
             (and (victim ?v) (in ?t ?r) (same-room ?t ?v ?r) (not (checked-room ?t ?r)))
             (:ordered (:task !check-room ?t ?r)
                       (:task help-victim ?t ?v)
                       (:task !leave-building ?t ?b))

             last-room-checked-no-victims
             (and (in ?t ?r) (not (checked-room ?t ?r)))
             (:ordered (:task !check-room ?t ?r)
                       (:task !leave-building ?t ?b))
    )

    (:method (help-victim ?t ?v)
             examine-injured-victim-and-move-to-next
             (and (victim ?v2) (spotted ?t ?v ?r) (different ?v ?v2) (spotted ?t ?v2 ?r) (same-room ?t ?v ?r) (same-room ?t ?v2 ?r) 
                  (injured ?v) (not (examined ?t ?v)) (not (examined ?t ?v2)) (not (triaged ?t ?v)))
             (:ordered (:task !examine-victim ?t ?v)
                       (:task !triage ?t ?v)
                       (:task !finish-and-goto-next-victim ?t ?v ?v2)
                       (:task help-victim ?t ?v2))

             examine-uninjured-victim-and-move-to-next
             (and (victim ?v2) (spotted ?t ?v ?r) (different ?v ?v2) (spotted ?t ?v2 ?r) (same-room ?t ?v ?r) (same-room ?t ?v2 ?r)
                  (not (examined ?t ?v)) (not (examined ?t ?v2)))
             (:ordered (:task !examine-victim ?t ?v)
                       (:task !finish-and-goto-next-victim ?t ?v ?v2)
                       (:task help-victim ?t ?v2))

             examine-last-injured-victim
             (and (spotted ?t ?v ?r) (same-room ?t ?v ?r) (injured ?v) (not (examined ?t ?v)) (not (triaged ?t ?v)))
             (:ordered (:task !examine-victim ?t ?v)
                       (:task !triage ?t ?v)
                       (:task !finish-examinations ?t ?v))

             examine-last-uninjured-victim
             (and (spotted ?t ?v ?r) (same-room ?t ?v ?r) (not (examined ?t ?v)))
             (:ordered (:task !examine-victim ?t ?v)
                       (:task !finish-examinations ?t ?v))
    )

    (:- (same-room ?t ?v ?r) (and (in ?t ?r) (in ?v ?r)))
    (:- (same ?x ?x) nil)
    (:- (different ?x ?y) ((not (same ?x ?y))))
  )
)

(defproblem sar-individual-problem
            ((building b) (room r1) (room r2) (room r3) (rescuer t1) (victim v1) (victim v2) 
                          (victim v3) (victim v4) (victim v5) (injured v2) (injured v3) (injured v5) (in v1 r1) (in v2 r1) (in v3 r2) (in v4 r2) (in v5 r3))
            ((enter-building-and-begin-mission t1 b r1)))

;; Find plans and graph the first one.

(let ((plan-trees (nth-value 2 
                             (find-plans 'sar-individual-problem
                                         :which :all
                                         :verbose :long-plans
                                         :plan-tree t))))
  (cl-user::multi-graph 1 plan-trees)) 
(cl-user::quit)
