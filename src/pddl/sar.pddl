;; This file defines the domain for an urban search-and-rescue mission in the
;; context of a collapsed office building, with 
;; four human team members, each of which is assigned a specific role.

(define (domain SAR)

  ;; We require the ability to handle typing durative actions and durative (in
  ;; addition to STRIPS).
  (:requirements :strips :typing :durative-actions)

  (:types agent 
          firefighter  ;; Only firefighters can put out fires.
          excavator    ;; Only excavators can clear blockages. 
          victim       ;; Victims that need medical attention 
          multi-victim ;; Victims that require two triagers to triage
          path         ;; A path from one location to another
          region       ;; A region of the building
          direction    ;; Direction in which to move
          hallway      ;; A hallway 
          room         ;; A room in the office building
          landmark     ;; A landmark for orienting the human
          fire         ;; A fire that has broken out in some location
  )

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

  (:action simul-triage
    :parameters ()
    :precondition (and (is multi-victim) ())
    :effect (and ()))


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
