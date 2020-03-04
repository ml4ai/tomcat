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
            rooms ;; Rooms (includes elevators and bathrooms)
    )
   
    (:predicates (in ?h - human ?r - rooms)
                 (triaged ?v - victim)
                 (checked ?r - rooms))

    (:action check-room
      :parameters (?t - rescuer ?r - rooms)
      :precondition (and (not (checked ?r)) (in ?t ?r))
      :effect (checked ?r))

    (:action move-to
      :parameters (?t - rescuer ?source - rooms ?destination - rooms)
      :precondition ((not (in ?t ?destination)) (in ?t ?source))
      :effect ((in ?t ?destination) (not (in ?t ?source))))

    (:action triage
      :parameters (?t - rescuer ?v - victim ?r - rooms)
      :precondition (and (in ?v ?r) (not (triaged ?v)) (in ?t ?r)) 
      :effect (triaged ?v)
    )

    (:method (search_and_triage ?t) 
             room-not-checked
             ((not (checked ?r))) 
             (:ordered (:task :immediate !check-room ?t ?r) 
                       (:task search_and_triage ?t))

             room-checked-victim-found 
             (and (victim ?v) (checked ?r) (same-room ?t ?v ?r) (not (triaged ?v))) 
             (:ordered (:task :immediate !triage ?t ?v ?r)
                       (:task search_and_triage ?t))

             room-checked-all-triaged
             ((checked ?r))
             ((:task !move-to ?t ?r ?r2))
             )

    (:- (same-room ?t ?v ?r) (and (in ?t ?r) (in ?v ?r)))
    (:- (same ?x ?x) nil)
    (:- (different ?x ?y) ((not (same ?x ?y))))
    ;(:- (all-rooms-checked) (setof ?rooms (rooms ?rooms) ?building))
  )
)

(defproblem sar-individual-problem
            ((rooms r2) (rooms r1) (rescuer t1) (victim v1) (victim v2) (in t1 r1) (in v1 r1) (in v2 r1))
            ((search_and_triage t1)))

;; Find plans and graph the first one.

(let ((plan-trees (nth-value 2 
                             (find-plans 'sar-individual-problem
                                         :which :all
                                         :verbose :long-plans
                                         :plan-tree t))))
  (cl-dot:dot-graph (spg:graph-plan-tree (first plan-trees))
                    "graph.pdf"
                    :format "pdf"))
(cl-user::quit)
