
#|Created: 19 October 2020  

   Purpose: To test run with Shop 3.
|#


(progn (ql:quickload "shop3"))

(in-package :shop-user)
(shop-trace :tasks :states :plans)

(defdomain (mine-wood :type pddl-domain :redefine-okay T :action-costs) ()
  (:types (wood - ingredients)
    )

  (:predicates (has-wood ?w - ingredients)
    );end predicates

  ;;; mine wood. Very simple for now.
  (:action mine-wood
    :parameters ((wood ?w - ingredients)
                 (has-wood ?w - ingredients)
    :precondition ()   ; none. There is conditional effect below.
    :effect (increase (has-wood ?w) ?1)
            ); end action mine-wood                               
); end defdomain

(defproblem mine-wood-problem
  ((ingredients wood)
   (has-wood ?w))
  (= ((has-wood w) ?4))
  );end problem
  (find-plans 'mine-wood-problem :which :all :verbose :plans :plan-tree nil)


