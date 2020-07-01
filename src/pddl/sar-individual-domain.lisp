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
       (load "util.lisp"))

(in-package :shop-user)

(shop-trace :all)
(defdomain (sar-individual-domain :type pddl-domain :redefine-ok T) (
    (:types human ;; Everything, including 'human' inherits from the base 'object' type
            victim rescuer - human ;; The rescuer and the victims are humans.
            room ;; Rooms (includes elevators and bathrooms)
            building ;; Building (contains rooms)
            goal
            strategy
    )
   
    (:predicates (in ?h - human ?r - room) ;; Indicates which room a human (rescuer, victim, etc) is inside
                 (triaged ?v - victim) ;; Indicates that a victim has been triaged
                 (searched ?t - rescuer ?r - room) ;; Indicates that a rescuer has checked a room
                 (searching ?t - rescuer ?r - room)
                 (spotted ?t - rescuer ?v - victim ?r - room) ;; Indicates that a rescuer has spotted the victim in a specific room
                 (triaging ?t - rescuer ?v - victim) ;; Indicates that a rescuer is in the process of examining a victim
                 (severe-injuries ?v - victim)
                 (victims-cleared ?t - rescuer ?r - room)
    )

    (:action start-searching ;; Rescuer checks the room, any victims in the room are "spotted" 
      :parameters (?t - rescuer ?r - room)
      :precondition (and (in ?t ?r) (not (searched ?t ?r)) 
                         (assign ?vs (cl-user::check-for-victim '?r .5)))
      :effect (and (searching ?t ?r) 
                   (forall ?vs
                           (and (victim ?v) (spotted ?t ?v ?r) 
                                (when (eval (cl-user::severely-injured ?v)) (severe-injuries ?v))))) 
    )

    (:action continue-searching ;; Rescuer checks the room, any victims in the room are "spotted" 
      :parameters (?t - rescuer ?r - room)
      :precondition (and (in ?t ?r) (searching ?t ?r) (not (searched ?t ?r)) 
                         (assign ?vs (cl-user::check-for-victim '?r .5)))
      :effect (forall ?vs 
                      (and (victim ?v) (spotted ?t ?v ?r) 
                           (when (eval (cl-user::severely-injured ?v)) (severe-injuries ?v)))) 
    )

    (:action done-searching
      :parameters (?t - rescuer ?r - room)
      :precondition (and (in ?t ?r) (searching ?t ?r))
      :effect (and (searched ?t ?r) (not (searching ?t ?r)))
    )
    
    (:action start-triaging-victim ;;
      :parameters (?t - rescuer ?v - victim)
      :precondition (and (spotted ?t ?v ?r) (not (triaged ?v)) (not (triaging ?t ?v)))
      :effect (triaging ?t ?v)
    )

    (:action finish-triaging-victim ;; Rescuer finishs examining the current victim and moves to the next
      :parameters (?t - rescuer ?v - victim)
      :precondition (and (spotted ?t ?v ?r) (triaging ?t ?v))
      :effect (and (triaged ?v) (not (triaging ?t ?v)))
    )

    (:action stop-triaging-victim ;; Rescuer finishs examining the current victim and moves to the next
      :parameters (?t - rescuer ?v - victim)
      :precondition (and (spotted ?t ?v ?r) (triaging ?t ?v))
      :effect (not (triaging ?t ?v))
    )


    (:action move ;; Rescuer moves to another room
      :parameters (?t - rescuer ?source ?destination - room)
      :precondition (and (in ?t ?source) (not (in ?t ?destination)))
      :effect (and (in ?t ?destination) (not (in ?t ?source)))
    )

    (:action leave ;; Rescuer leaves the building
      :parameters (?t - rescuer ?r - room)
      :precondition (in ?t ?r)
      :effect (not (in ?t ?r))
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
             (and (not (triaging ?t ?_)) (strategy A) (not (searched ?t ?r)) 
                  (or (not (spotted ?t ?v ?r)) (and (spotted ?t ?v ?r) (triaged ?v))))
             (:task search ?t ?r)

             search-B ;; Search strategy where agent only stops to treat gold victims.
             (and (not (triaging ?t ?_)) (strategy B) (not (searched ?t ?r)) 
                  (or (not (spotted ?t ?v ?r)) (and (spotted ?t ?v ?r) 
                                                    (or (triaged ?v) (not (severe-injuries ?v))))))
             (:task search ?t ?r)

             triage-A
             (and (strategy A) (or (triaging ?t ?v) (and (spotted ?t ?v ?r) (not (triaged ?v)))))
             (:task triage ?t ?v)

             triage-B
             (and (strategy B) (or (triaging ?t ?v) (and (spotted ?t ?v ?r) (not (triaged ?v)) (severe-injuries ?v))))
             (:task triage ?t ?v)

             move
             (next-location ?r2)
             (:task !move ?t ?r ?r2)

             leave
             ()
             (:task !leave ?t ?r)
    )

    (:method (search ?t ?r)
             start-searching
             (and (not (searching ?t ?r)))
             (:task !start-searching ?t ?r)

             continue-searching
             (and (searching ?t ?r) (how-many-found ?t ?r ?c) (eval (cl-user::still-searching '?c)))
             (:task !continue-searching ?t ?r)

             finish-searching
             (searching ?t ?r)
             (:ordered (!continue-searching ?t ?r)
                       (!done-searching ?t ?r))
    )

    (:method (triage ?t ?v)
             start-triaging
             (and (not (triaging ?t ?v)))
             (:task !start-triaging-victim ?t ?v)

             finish-triaging
             (and (triaging ?t ?v) (eval (cl-user::triage-succesful .9)))
             (:task !finish-triaging-victim ?t ?v)

             stop-triaging
             (triaging ?t ?v)
             (:task !stop-triaging-victim ?t ?v)

    )

    (:- (same ?x ?x) nil) ;; Helper axiom for "different" axoim
    (:- (different ?x ?y) ((not (same ?x ?y)))) ;; Axiom that determines that two variables contain different values. 
    (:- (how-many-found ?t ?r ?c) (and (setof ?v (spotted ?t ?r ?v) ?vs) (assign ?c (eval (length '?vs))))) 
  )
)
