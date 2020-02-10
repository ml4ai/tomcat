(ql:quickload "shop3")
(in-package :shop-user)
; Simple example of a SAR mission with two rooms

(defdomain simple-sar (
   (:op (!move-to ?agent ?location ?destination)
    :add ((in ?agent ?destination))
    :delete ((in ?agent ?location))
    :precond (in ?agent ?location))
   (:op (!triage ?victim ?location)
    :add NIL
    :delete ((in ?victim ?location))
    :precond (in ?victim ?location))

   (:method (visit-rooms ?agent ?room1 ?room2)
    (in ?agent ?room1)
    ((search-room ?room1) (!move-to ?agent ?room1 ?room2))
    (in ?agent ?room2)
    ((search-room ?room2)(!move-to ?agent ?room2 ?room1)))
   (:method (search-room ?room)
    (in victim1 ?room)
    ((!triage victim1 ?room))
    (in victim2 ?room)
    ((!triage victim2 ?room)))
  )
)

(defproblem sar-example1 simple-sar
  ((in victim1 room1) (in victim2 room2) (in triager room1))
  ((visit-rooms triager room1 room2)))

(find-plans 'sar-example1 :verbose :long-plans)
