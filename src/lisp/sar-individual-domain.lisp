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
       (defparameter *roomlist* (load-object-from-file "sar-room-list.txt"))
       (defvar *s-paths* (load-json-database "sar_shortest_path_lengths.json"))
       (defparameter *time-passed* 0)
       (defvar *number-of-rooms* (length *roomlist*)))

(in-package :shop-user)

;;(shop-trace :methods)

(defun plan-found-hook (state which plan cost depth)
                 (setf cl-user::*time-passed* (+ cl-user::*time-passed* cost)))

(defdomain (sar-individual-domain :type pddl-domain :redefine-ok T) (
    (:types human ;; Everything, including 'human' inherits from the base 'object' type
            victim rescuer - human ;; The rescuer and the victims are humans.
            room ;; Rooms (includes elevators and bathrooms)
            building ;; Building (contains rooms)
            goal
    )
   
    (:predicates (in ?h - human ?r - room) ;; Indicates which room a human (rescuer, victim, etc) is inside
                 (triaged ?v - victim) ;; Indicates that a victim has been triaged
                 (searched ?t - rescuer ?r - room) ;; Indicates that a rescuer has checked a room
                 (searching ?t - rescuer ?r - room)
                 (spotted ?t - rescuer ?v - victim ?r - room) ;; Indicates that a rescuer has spotted the victim in a specific room
                 (triaging ?t - rescuer ?v - victim) ;; Indicates that a rescuer is in the process of examining a victim
                 (severe-injuries ?v - victim)
                 (victims-cleared ?t - rescuer ?r - room)
                 (marked-for-revisit ?r - room)
    )

    (:action start-searching ;; Rescuer checks the room, any victims in the room are "spotted" 
      :parameters (?t - rescuer ?r - room)
      :precondition (and (in ?t ?r) (not (searched ?t ?r))) 
      :effect (and (searching ?t ?r)) 
      :cost (+ 5 (random 10))
    )

    (:action continue-searching ;; Rescuer checks the room, any victims in the room are "spotted" 
      :parameters (?t - rescuer ?r - room)
      :precondition (and (in ?t ?r) (searching ?t ?r) (not (searched ?t ?r))) 
      :effect ()
      :cost (+ 5 (random 10))
    )

    (:action done-searching
      :parameters (?t - rescuer ?r - room)
      :precondition (and (in ?t ?r) (searching ?t ?r))
      :effect (and (searched ?t ?r) (not (searching ?t ?r)))
      :cost 0
    )
    
    (:action start-triaging-victim ;;
      :parameters (?t - rescuer ?v - victim)
      :precondition (and (spotted ?t ?v ?r) (not (triaged ?v)) (not (triaging ?t ?v)))
      :effect (triaging ?t ?v)
      :cost 0
    )

    (:action finish-triaging-victim ;; Rescuer finishs examining the current victim and moves to the next
      :parameters (?t - rescuer ?v - victim)
      :precondition (and (spotted ?t ?v ?r) (triaging ?t ?v))
      :effect (and (triaged ?v) (not (triaging ?t ?v)))
      :cost (cl-user::triage-cost '?v t)
    )

    (:action stop-triaging-victim ;; Rescuer finishs examining the current victim and moves to the next
      :parameters (?t - rescuer ?v - victim)
      :precondition (and (spotted ?t ?v ?r) (triaging ?t ?v))
      :effect (not (triaging ?t ?v))
      :cost (cl-user::triage-cost '?v nil)
    )


    (:action move ;; Rescuer moves to another room
      :parameters (?t - rescuer ?source ?destination - room)
      :precondition (and (in ?t ?source) (not (in ?t ?destination)))
      :effect (and (in ?t ?destination) (not (in ?t ?source)))
      :cost (ceiling (/ (+ 1 (cl-user::shortest-path-cost cl-user::*s-paths* '?source '?destination)) 5.612))
    )

    (:action end-mission ;; Rescuer leaves the building
      :parameters (?t - rescuer ?r - room)
      :precondition (in ?t ?r)
      :effect (not (in ?t ?r))
      :cost 0
    )

    (:operator (!!mark-for-revisit ?r)
               (())
               (())
               ((marked-for-revisit ?r))
               0
    )

    (:operator (!change-priority)
               (())
               (())
               ((priority-changed))
               0
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

    (:method (perform-next-task-without-victim-priority ?t ?r) ;; Method for examining and triaging victims
             search ;; Search strategy where agent stops searching to treat any victims currently found
             (and (eval (< cl-user::*time-passed* 900)) (not (triaging ?t ?_)) (not (searched ?t ?r)) 
                  (or (not (spotted ?t ?v ?r)) (and (spotted ?t ?v ?r) (triaged ?v))))
             (:task search ?t ?r)

             triage
             (and (eval (< cl-user::*time-passed* 900)) 
                  (or (triaging ?t ?v) 
                      (and (spotted ?t ?v ?r) (not (triaged ?v)) 
                           (not (and (severe-injuries ?v) (eval (>= cl-user::*time-passed* 540)))))))
             (:task triage ?t ?v)

             move
             (and (eval (< cl-user::*time-passed* 900)) (assign ?room (cl-user::get-next-room '?r cl-user::*roomlist*)) 
                  (room ?r2) (different ?r ?r2) (eval (cl-user::symbol-equals-keyword '?room '?r2))
                  (eval (cl-user::remove-from-room-list '?r "sar-room-list.txt"))
                  (eval (setf cl-user::*roomlist* (cl-user::load-object-from-file "sar-room-list.txt"))))
             (:task !move ?t ?r ?r2)

             end-mission
             ()
             (:task !end-mission ?t ?r)
    )



    (:method (perform-next-task-that-prioritizes-yellow-victims ?t ?r) ;; Method for examining and triaging victims
             change-priority
             (and (eval (< cl-user::*time-passed* 900)) (not (priority-changed)) 
                  (or (all-rooms-searched) (eval (> cl-user::*time-passed* 525))))
             (:task !change-priority)

             search ;; Search strategy where agent only stops to treat gold victims.
             (and (eval (< cl-user::*time-passed* 900)) (not (triaging ?t ?_)) (not (searched ?t ?r)) 
                  (or (not (spotted ?t ?v ?r)) (and (spotted ?t ?v ?r) 
                                                    (or (triaged ?v) (not (severe-injuries ?v))))))
             (:task search ?t ?r)

             triage
             (and (eval (< cl-user::*time-passed* 900)) 
                  (or (triaging ?t ?v) 
                      (and (spotted ?t ?v ?r) (not (triaged ?v)) (severe-injuries ?v) (eval (< cl-user::*time-passed* 540)))))
             (:task triage ?t ?v)

             move-and-mark-for-revisit
             (and (eval (< cl-user::*time-passed* 900)) (assign ?room (cl-user::get-next-room '?r cl-user::*roomlist*)) 
                  (room ?r2) (different ?r ?r2) (eval (cl-user::symbol-equals-keyword '?room '?r2))
                  (spotted ?t ?v ?r) (not (severe-injuries ?v))
                  (eval (cl-user::remove-from-room-list '?r "sar-room-list.txt"))
                  (eval (cl-user::add-to-room-list '?r "sar-room-list.txt"))
                  (eval (setf cl-user::*roomlist* (cl-user::load-object-from-file "sar-room-list.txt"))))
             ((:task !move ?t ?r ?r2)
              (:task !!mark-for-revisit ?r))

             move
             (and (eval (< cl-user::*time-passed* 900)) (assign ?room (cl-user::get-next-room '?r cl-user::*roomlist*)) 
                  (room ?r2) (different ?r ?r2) (eval (cl-user::symbol-equals-keyword '?room '?r2))
                  (eval (cl-user::remove-from-room-list '?r "sar-room-list.txt"))
                  (eval (setf cl-user::*roomlist* (cl-user::load-object-from-file "sar-room-list.txt"))))
             (:task !move ?t ?r ?r2)

             end-mission
             ()
             (:task !end-mission ?t ?r)
    )

    (:method (search ?t ?r)
             start-searching
             (and (not (searching ?t ?r)))
             ((:task !start-searching ?t ?r)
             (:task util-percept-victims ?t ?r))

             continue-searching
             (and (searching ?t ?r) (how-many-found ?t ?r ?c) (eval (cl-user::still-searching '?c)))
             ((:task !continue-searching ?t ?r)
              (:task util-percept-victims ?t ?r))

             finish-searching
             (searching ?t ?r)
             (:ordered (:task !continue-searching ?t ?r)
                       (:task util-percept-victims ?t ?r)
                       (:task !done-searching ?t ?r))
    )

    (:method (triage ?t ?v)
             start-triaging
             (and (not (triaging ?t ?v)))
             (:task !start-triaging-victim ?t ?v)

             finish-triaging
             (and (triaging ?t ?v) (eval (cl-user::triage-successful .9)) (not (and (severe-injuries ?v) (eval (>= cl-user::*time-passed* 540)))))
             (:task !finish-triaging-victim ?t ?v)

             stop-triaging
             (triaging ?t ?v)
             (:task !stop-triaging-victim ?t ?v)

    )

    (:method (util-percept-victims ?t ?r) ;;Utility method
             spotted-severely-injured-victim
             (and (in ?t ?r) (assign* ?v (cl-user::check-for-victim '?r)) 
                  (eval (cl-user::severely-injured '?v)) (not (spotted ?t ?v ?r)) (eval (cl-user::found-victim .5)))
             (:ordered (:task !!assert (victim ?v))
                       (:task !!assert (spotted ?t ?v ?r))
                       (:task !!assert (severe-injuries ?v))
                       (:task util-percept-victims ?t ?r))

             spotted-injured-victim
             (and (in ?t ?r) (assign* ?v (cl-user::check-for-victim '?r)) (not (eval (cl-user::severely-injured '?v))) (not (spotted ?t ?v ?r))
                  (eval (cl-user::found-victim .5)))
             (:ordered (:task !!assert (victim ?v))
                       (:task !!assert (spotted ?t ?v ?r))
                       (:task util-percept-victims ?t ?r))

             no-victims-spotted
             ()
             ()
    )

    (:- (same ?x ?x) nil) ;; Helper axiom for "different" axoim
    (:- (different ?x ?y) ((not (same ?x ?y)))) ;; Axiom that determines that two variables contain different values. 
    (:- (how-many-found ?t ?r ?c) (and (setof ?v (spotted ?t ?r ?v) ?vs) (assign ?c (eval (length '?vs))))) 
    (:- (all-rooms-searched) (and (setof ?r (searched ?t ?r) ?rs) (eval (= (length '?rs) cl-user::*number-of-rooms*))))
  )
)
