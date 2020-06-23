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
       (load "util.lisp")
       (defvar *s-paths* (load-json-database "sar_shortest_path_lengths.json")))

(in-package :shop-user)

;;(shop-trace :states :operators)
(defdomain (sar-individual-domain :type pddl-domain :redefine-ok T) (
    (:types human ;; Everything, including 'human' inherits from the base 'object' type
            victim rescuer - human ;; The rescuer and the victims are humans.
            room ;; Rooms (includes elevators and bathrooms)
            building ;; Building (contains rooms)
            goal
    )
   
    (:predicates (in ?h - human ?r - room) ;; Indicates which room a human (rescuer, victim, etc) is inside
                 (inside ?h - rescuer ?b - building) ;; Indicates that the rescuer is in the building
                 (rescued ?v - victim) ;; Indicates that a victim has been triaged
                 (searched ?t - rescuer ?r - room) ;; Indicates that a rescuer has checked a room
                 (searching ?t - rescuer ?r - room)
                 (spotted ?t - rescuer ?v - victim ?r - room) ;; Indicates that a rescuer has spotted the victim in a specific room
                 (rescuing ?t - rescuer ?v - victim) ;; Indicates that a rescuer is in the process of examining a victim
                 (severe-injuries ?v - victim)
                 (victims-cleared ?t - rescuer ?r - room)
    )

    (:action start-searching ;; Rescuer checks the room, any victims in the room are "spotted" 
      :parameters (?t - rescuer ?r - room)
      :precondition (and (in ?t ?r) (not (searched ?t ?r)))
      :effect (searching ?t ?r) 
    )

    (:action finish-searching
      :parameters (?t - rescuer ?r - room)
      :precondition (and (in ?t ?r) (searching ?t ?r))
      :effect (and (searched ?t ?r) (not (searching ?t ?r)))
    )
    
    (:action start-rescuing-victim ;;
      :parameters (?t - rescuer ?v - victim)
      :precondition (and (spotted ?t ?v ?r) (not (rescued ?v)) (not (rescuing ?t ?v)))
      :effect (rescuing ?t ?v)
    )

    (:action finish-rescuing-victim ;; Rescuer finishs examining the current victim and moves to the next
      :parameters (?t - rescuer ?v - victim)
      :precondition (and (spotted ?t ?v ?r) (rescuing ?t ?v))
      :effect (and (rescued ?v) (not (rescuing ?t ?v)))
    )

    (:action move ;; Rescuer moves to another room
      :parameters (?t - rescuer ?source ?destination - room)
      :precondition (and (in ?t ?source) (not (in ?t ?destination)))
      :effect (and (in ?t ?destination) (not (in ?t ?source)))
      :cost (+ 1 (cl-user::shortest-path-cost cl-user::*s-paths* '?source '?destination))
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

    (:operator (!!assert ?fact) 
               (())
               (())
               (?fact)
               0
    )

    (:operator (!!delete ?fact) 
               (())
               (?fact)
               (())
               0
    )


    (:method (perform-next-mission-task ?t ?r) ;; Method for examining and triaging victims
             search-A ;; Search strategy where agent stops searching to treat any victims currently found
             (and (strategy A) (not searched ?t ?r) 
                  (or (not (spotted ?t ?v ?r)) (and (spotted ?t ?v ?r) (not (rescued ?v)))))
             (:task search)

             search-B ;; Search strategy where agent only stops to treat gold victims.
             (and (strategy B) (not searched ?t ?r) 
                  (or (not (spotted ?t ?v ?r)) (and (spotted ?t ?v ?r) (not (rescued ?v)))))
             (:task search)


    )

    (:- (same ?x ?x) nil) ;; Helper axiom for "different" axoim
    (:- (different ?x ?y) ((not (same ?x ?y)))) ;; Axiom that determines that two variables contain different values. 
  )
)
