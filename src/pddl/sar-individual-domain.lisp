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
       (setf s-paths (load-json-database "sar_shortest_path_lengths.json")))

(in-package :shop-user)

(defun trace-query-hook (type item additional-information state-atoms)
                  (print type)
                  (print item)
                  (print state-atoms))

(shop-trace :operators)
(defdomain (sar-individual-domain :type pddl-domain :redefine-ok T) (
    (:types human ;; Everything, including 'human' inherits from the base 'object' type
            victim rescuer - human ;; The rescuer and the victims are humans.
            room ;; Rooms (includes elevators and bathrooms)
            building ;; Building (contains rooms)
    )
   
    (:predicates (in ?h - human ?r - room) ;; Indicates which room a human (rescuer, victim, etc) is inside
                 (inside ?h - rescuer ?b - building) ;; Indicates that the rescuer is in the building
                 (triaged ?v - victim) ;; Indicates that a victim has been triaged
                 (searched ?t - rescuer ?r - room) ;; Indicates that a rescuer has checked a room
                 (searching ?t - rescuer ?r - room)
                 (spotted ?t - rescuer ?v - victim ?r - room) ;; Indicates that a rescuer has spotted the victim in a specific room
                 (examined ?t - rescuer ?v - victim) ;; Indicates that a rescuer has examined a victim
                 (examining ?t - rescuer ?v - victim) ;; Indicates that a rescuer is in the process of examining a victim
                 (injured ?v - victim) ;; Indicates that a victim is injured (to the point of needing triage) 
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
    
    (:action start-examining-victim ;; Rescuer begins examining the victim
      :parameters (?t - rescuer ?v - victim)
      :precondition (and (spotted ?t ?v ?r) (not (examined ?t ?v)))
      :effect (examining ?t ?v)
    )

    (:action finish-examining-victim ;; Rescuer finishs examining the current victim and moves to the next
      :parameters (?t - rescuer ?v - victim)
      :precondition (and (spotted ?t ?v ?r) (examining ?t ?v))
      :effect (and (examined ?t ?v) (not (examining ?t ?v)))
    )

    (:action move ;; Rescuer moves to another room
      :parameters (?t - rescuer ?source ?destination - room)
      :precondition (and (in ?t ?source) (not (in ?t ?destination)))
      :effect (and (in ?t ?destination) (not (in ?t ?source)))
      :cost (+ 1 (cl-user::shortest-path-cost cl-user::s-paths '?source '?destination))
    )

    (:action triage ;; Rescuer triages a victim, they must be currently examining the victim to do so
      :parameters (?t - rescuer ?v - victim)
      :precondition (and (in ?t ?r) (examining ?t ?v) (not (triaged ?v))) 
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


    (:method (enter-building-and-complete-mission ?t ?b ?r) ;; Method for entering the building and doing the mission
             enter-and-begin
             ()
             (:ordered (:task !enter-building ?t ?b ?r)
                       (:task explore ?t ?r))

    )

    (:method (explore ?t ?r) ;; Method for searching a room and determining what needs to be done to move on
             room-unsearched ;; Branch for checking a room with victims inside
             (and (in ?t ?r) (not (searched ?t ?r))) 
             (:ordered (:task search-room ?t ?r)
                       (:task explore ?t ?r))

             victims-found-in-room
             (and (in ?t ?r) (not (victims-cleared ?t ?r)))
             (:ordered (:task help-victims ?t ?r)
                       (:task explore ?t ?r))

             room-searched-and-cleared ;; Branch for checking a room with no victims
             (and (in ?t ?r) (room ?r2) (different ?r ?r2) (not (searched ?t ?r2)))
             (:ordered (:task !move ?t ?r ?r2)
                       (:task explore ?t ?r2))

             all-rooms-searched ;; Branch for checking the last room with victims inside
             (in ?t ?r)
             (:ordered (:task !leave-building ?t ?b))

    )

    (:method (search-room ?t ?r)
             initial-victim-found
             (and (in ?t ?r) (assign* ?v (cl-user::check-for-victim '?r)) (not (spotted ?t ?v ?r)) (not (searching ?t ?r)))
             (:ordered (:task !start-searching ?t ?r)
                       (:task !!assert (victim ?v))
                       (:task !!assert (spotted ?t ?v ?r))
                       (:task search-room ?t ?r))

             another-victim-found
             (and (in ?t ?r) (assign* ?v (cl-user::check-for-victim '?r)) (not (spotted ?t ?v ?r)))
             (:ordered (:task !!assert (victim ?v))
                       (:task !!assert (spotted ?t ?v ?r))
                       (:task search-room ?t ?r))

             all-victims-found
             (and (in ?t ?r) (spotted ?t ?v ?r))
             (:ordered (:task !finish-searching ?t ?r))

             no-victims-found
             (in ?t ?r)
             (:ordered (:task !start-searching ?t ?r)
                       (:task !finish-searching ?t ?r)
                       (:task !!assert (victims-cleared ?t ?r)))
    )

    (:method (help-victims ?t ?r) ;; Method for examining and triaging victims
             examine-injured-victim ;; Branch for examining an injured victim and triaging them
             (and (in ?t ?r) (spotted ?t ?v ?r) (eval (cl-user::is-injured '?v)) (not (examined ?t ?v)))
             (:ordered (:task !start-examining-victim ?t ?v)
                       (:task !triage ?t ?v)
                       (:task !finish-examining-victim ?t ?v)
                       (:task help-victims ?t ?r))

             examine-uninjured-victim ;; Branch for examining an uninjured victim
             (and (in ?t ?r) (spotted ?t ?v ?r) (not (examined ?t ?v)))
             (:ordered (:task !start-examining-victim ?t ?v)
                       (:task !finish-examining-victim ?t ?v)
                       (:task help-victims ?t ?r))

             no-more-victims-to-examine ;; Branch for examining and triaging the last injured victim in the room
             (in ?t ?r)
             (:ordered (:task !!assert (victims-cleared ?t ?r)))

    )

    (:- (same ?x ?x) nil) ;; Helper axiom for "different" axoim
    (:- (different ?x ?y) ((not (same ?x ?y)))) ;; Axiom that determines that two variables contain different values. 
  )
)
