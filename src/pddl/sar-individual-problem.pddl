(define (problem sar-individual-mission)
 (:domain sar-individual)
 (:objects room1 room2 room3 - room
           victim1 victim2 victim3 - victim
           triager1 - triager)
 (:init (= (room-of triager1) room1)
 (:goal (forall (?r - room) (checked ?r)))
)
