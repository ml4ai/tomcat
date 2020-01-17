;; This file defines the domain for an urban search-and-rescue mission in the
;; context of a collapsed office building, with a single human whose goal is to
;; triage as many victims of the collapse as possible within a given time
;; limit. Note that the :method definitions are not part of standard PDDL, but are
;; taken from SHOP3's modified version of PDDL, to support HTN planning.
;; We also use object fluents from PDDL 3.1.

(define (domain sar-individual)

  (:requirements :strips :typing :equality :durative-actions :object-fluents)

  ;; Here we define a type hierarchy
  (:types human - object ;; Everything, including 'human' inherits from the base 'object' type
          triager victim - human ;; The triager and the victims are humans.
          room ;; Rooms (includes elevators and bathrooms)
  )

  (:functions (room-of ?h - human) - room)
  (:predicates (triaged ?v - victim)
               (checked ?r - room)
               (in ?h - human ?r - room)) 


  (:action check-room
    :parameters (?t - triager ?r - room)
    :precondition (and (not (checked ?r)) (in ?t ?r)) ;; TODO
    :effect (checked ?r))

  (:action triage
    :parameters (?t - triager ?v - victim)
    :precondition (and (not (triaged ?v))) (= (room-of ?t) (room-of ?v)))
    :effect (triaged ?v))
