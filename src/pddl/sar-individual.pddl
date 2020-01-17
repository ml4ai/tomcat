;; This file defines the domain for an urban search-and-rescue mission in the
;; context of a collapsed office building, with a single human whose goal is to
;; triage as many victims of the collapse as possible within a given time
;; limit. Note that the :method definitions are not part of standard PDDL, but are
;; taken from SHOP3's modified version of PDDL, to support HTN planning.

(define (domain SAR)

  ;; We require the ability to handle typing durative actions and durative (in
  ;; addition to STRIPS).
  (:requirements :strips :typing :durative-actions)

  (:types victim room)
  (:predicates (path-clear ?path - path)
               (is ?object - object ?thing - object)
               (at ?object - object ?place - location)
               (in ?object - object ?thing - object))

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
