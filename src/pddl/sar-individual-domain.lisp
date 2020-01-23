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

(defdomain (sar-individual0 :type pddl-domain :redefine-ok T)
  (
  ;;(:include sar-group #.(asdf:system-relative-pathname "sar-group-domain.lisp"))
   (:types object
           thing
           human - object
           triager victim - human
           specialist - triager
           firefighter excavator - specialist
           agent - specialist
           location
           place - location
           room
           door
           doorway - door
           exit - door)
   
   (:predicates (is ?object - object ?thing - thing)
                (at ?o - object  ?l location)
                (in ?object - object ?place object)
                (triaged ?v - victim)
                (checked ?r - room))

   (:- (and (at ?a ?b)
       (at ?a ?b)
       ()))
   (:CONSTANTS P1 - HUMAN)
   (:op (!move-to ?agent ?location ?destination))
   (:op (!check-room ?agent ?room))
   (:op (!extinguish ?agent ?room))
   (:op (!excavate ?agent ?location ?victim))
   (:op (!triage ?agent ?location ?victim))
   (:op (!open ?agent ?door))
  
   (:method (move-to) ())
   (:method (visiting-all-rooms) ())
   (:method (coordinate-multiple-resources) ())
   (:method (find-all-victims) ())
   (:method (clear-multiple-blocks) ())
  )
)