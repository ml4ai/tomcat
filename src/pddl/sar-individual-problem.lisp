;; Our Domain Definitions will use SHOP3 Syntax

(defproblem sar-individual-problem sar-individual
  ((:objects room1 room2 - room)
   (:init victim1 victim2 - victim)
   (:goal (forall (?r - room) (checked ?r)))
  )
)
