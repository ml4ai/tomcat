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

(ql:quickload "shop3")
(in-package :shop-user)
(defdomain (sar-individual-domain :type pddl-domain :redefine-ok T) (
    (:types human - object ;; Everything, including 'human' inherits from the base 'object' type
            victim rescuer - human ;; The rescuer and the victims are humans.
            room - object;; Rooms (includes elevators and bathrooms)
    )
    
    (:predicates (in ?h - human ?r - room)
                  (triaged ?v - victim)
                  (checked ?r - room))

    (:action check-room
      :parameters (?t - rescuer ?r - room)
      :precondition (and (not (checked ?r)) (in ?t ?r))
      :effect (checked ?r))

    (:action move-to
      :parameters (?t - rescuer ?source - room ?destination - room)
      :precondition ((not (in ?t ?destination)) (in ?t ?source))
      :effect ((in ?t ?destination) (not (in ?t ?source))))

    (:action triage
      :parameters (?t - rescuer ?v - victim ?r - room)
      :precondition (and (in ?v ?r) (not (triaged ?v)) (in ?t ?r)) 
      :effect (triaged ?v)
    )

    (:method (main) 
             room-not-checked
             ((not (checked ?r))) 
             (:ordered (:task :immediate !check-room ?t ?r) 
                       (:task main))

             room-checked-victim-found 
             (and (checked ?r) (in v1 ?r) (not (triaged ?v))) 
             (:ordered (:task :immediate !triage ?t ?v ?r)
                       (:task main))

             room-checked-victim-triaged
             (and (checked ?r) (in v1 ?r) (triaged ?v))
             ((:task !move-to ?t ?r ?r2))

             room-checked-no-victim
             ((checked ?r))
             ((:task !move-to ?t ?r ?r2))
             )
  )
)

(defproblem sar-individual-problem 
            ((room r2) (room r1) (rescuer t1) (victim v1) (in t1 r1) (in v1 r1))
            ((main)))
  
(find-plans 'sar-individual-problem :verbose :long-plans)
