
#|Created: 19 October 2020  

   Purpose: To test run with Shop 3.
   For now: reference to shop3 manual in comments. Delete later.
|#


(progn (ql:quickload "shop3")
       )
(in-package :shop-user) ;pg 5
;; for debugging, try (shop trace :tasks) only, then find plans again. pg 10
(shop-trace :tasks :states :plans) ;pg 9 methods axioms operators goals all effects

(defdomain (mine-wood :type pddl-domain) (
    (:types 
      ingredients - object
      wood - ingredients
    )

    (:predicates (has-wood ?iw - ingredients)
    )

  ;;; mine wood. Very simple for now.
    (:action get-wood
      :parameters ((wood ?w - ingredients)
                   (has-wood ?iw - ingredients))
      :precondition ()  
      :effect (increase (has-wood ?iw) ?1)
    )
    )
); end defdomain

(defproblem mine-wood-problem
            ((ingredients wood)
             (has-wood iw))
             (= ((has-wood iw) 4))
);end problem

(find-plans 'mine-wood-problem :which :all :verbose :plans :plan-tree t);1,2,3,t, nil


