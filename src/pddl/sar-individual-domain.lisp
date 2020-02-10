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
            victim triager - human ;; The triager and the victims are humans.
            room - object;; Rooms (includes elevators and bathrooms)
    )
    
    (:predicates (in ?h - human ?r - room)
                  (triaged ?v - victim)
                  (checked ?r - room))

    (:action check-room
      :parameters (?t - triager ?r - room)
      :precondition (and (not (checked ?r)) (in ?t ?r))
      :effect (checked ?r))

    (:action move-to
      :parameters (?t - triager ?source - room ?destination - room)
      :precondition ((not (in ?t ?destination)) (in ?t ?source))
      :effect ((in ?t ?destination) (not (in ?t ?source))))

    (:action triage
      :parameters (?t - triager ?v - victim ?r - room)
      :precondition (and (not (triaged ?v)) (in ?t ?r)) 
      :effect (triaged ?v)
    )
    (:pddl-method (main ?t ?r) 
                  (in ?t ?r) 
                  (!check-room ?t ?r))
  )
)

(defproblem sar-individual-problem 
            ((room r1) (room r2) (triager t1) (in t1 r1))
            ((main t1 r1)))
  
(find-plans 'sar-individual-problem :verbose :long-plans)
