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
                 (checked ?r - room))

    (:action check-room
      :parameters (?t - rescuer ?r - room)
      :precondition (and (in ?t ?r) (not (checked ?r)))
      :effect (checked ?r)
    )

    (:action move-to
      :parameters (?t - rescuer ?source ?destination - room)
      :precondition (and (in ?t ?source) (not (in ?t ?destination)))
      :effect (and (in ?t ?destination) (not (in ?t ?source)))
    )

    (:action triage
      :parameters (?t - rescuer ?v - victim ?r - room)
      :precondition (and (same-room ?t ?v ?r) (not (triaged ?v))) 
      :effect (triaged ?v)
    )

    (:action leave-building
     :parameters (?t - rescuer ?b - building)
     :precondition (inside ?t ?b)
     :effect ((not (inside ?t ?b)))
    )

    (:method (search_and_triage ?t) 
             room-not-checked
             (and (in ?t ?r) (not (checked ?r))) 
             (:ordered (:task :immediate !check-room ?t ?r) 
                       (:task search_and_triage ?t))

             room-checked-victim-found 
             (and (victim ?v) (checked ?r) (same-room ?t ?v ?r) (not (triaged ?v))) 
             (:ordered (:task :immediate !triage ?t ?v ?r)
                       (:task search_and_triage ?t))

             all-clear
             (and (num-of-rooms-checked ?c) (num-of-rooms ?n) (eval (eq '?n '?c)))
             (:ordered (:task !leave-building ?t ?b))

             room-checked-all-triaged
             (and (checked ?r) (in ?t ?r))
             (:ordered (:task :immediate !move-to ?t ?r ?r2)
                       (:task search_and_triage ?t))
             )

    (:- (same-room ?t ?v ?r) (and (in ?t ?r) (in ?v ?r)))
    (:- (same ?x ?x) nil)
    (:- (different ?x ?y) ((not (same ?x ?y))))
    (:- (num-of-rooms-checked ?c) ((setof ?rooms (checked ?rooms) ?ac) (assign ?c (length '?ac))))
    (:- (num-of-rooms ?n) ((setof ?rooms (room ?rooms) ?b) (assign ?n (length '?b))))
  )
)

(defproblem sar-individual-problem
            ((building b) (room r1) (room r2) (rescuer t1) (victim v1) (victim v2) (victim v3) 
                          (victim v4) (inside t1 b) (in t1 r1) (in v1 r1) (in v2 r1) (in v3 r2) (in v4 r2))
            ((search_and_triage t1)))

;; Find plans and graph the first one.

(let ((plan-trees (nth-value 2 
                             (find-plans 'sar-individual-problem
                                         :which :first
                                         :verbose :long-plans
                                         :plan-tree t))))
  (cl-dot:dot-graph (spg:graph-plan-tree (first plan-trees))
                    "graph.pdf"
                    :format "pdf"))
(cl-user::quit)
