;; This file defines the domain for an urban search-and-rescue mission in the
;; context of a collapsed office building, with a single human whose goal is to
;; triage as many victims of the collapse as possible within a given time
;; limit. Note that the :method definitions are not part of standard PDDL, but are
;; taken from SHOP3's modified version of PDDL, to support HTN planning.

(define (domain SAR)

  (:requirements :strips :typing :equality :durative-actions :object-fluents)

  ;; Here we define a type hierarchy
  (:types human - object ;; Everything, including 'human' inherits from the base 'object' type
          triager victim - human ;; The triager and the victims are humans.
          location region - object
          room hallway - region ;; Rooms and hallways are classified as discrete 'regions'
  )

  (:functions (room-of ?x - human) - room)

  ;; Here we define our boolean predicates.
  (:predicates (in ?x - human ?y - room) ;; Is human x in room y? 
               (at ?x - triager ?y - location)) ;; Is triager x at location y?  And so on...

  (:durative-action (triage ?x - victim)
    :parameters )
  (:action put-out-fire
    :parameters (?agent - firefighter ?place - location)
    :precondition (and (at fire place) (at agent place))
    :effect (not (at fire place)))

  (:durative-action clear-blockage
    :parameters (?agent - excavator ?victim - victim ?place - location ?time)
    :duration (= ?duration (time))
    :condition (and (at victim place) (at agent place))
    :effect (and ((at end (not (at victim location))))))

  (:durative-action triage-resource
    :parameters (?agent ?time)
    :duration (= ?duration (time)) 
    :condition (and ()) 
    :effect (and ((at end (triage-resource agent duration)))))

  (:action move-to-prim
    :parameters (?agent - agent ?from - location ?to - location)
    :preconditions (and (at agent from))
    :effect (and (not (at agent from))(at agent to)))

  (:action find-victims
    :parameters (?from - region ?to - region)
    :preconditions (and)
    :effect (and))

  (:method clear-multiple-blocks 
    (loop for block in 'blocks do (clear-blockage block)))

;; forall (?room - room)
;;     forall (?region - region)
;;          (find-victims) 
;;              (if (> total-victims 1) (triage-resource) (simul-triage))

  (:method (move-to))
  (:method (visiting-all-rooms))
  (:method (coordinate-multiple-resources))
  (:method (in-region (find-all-victims)))
)
