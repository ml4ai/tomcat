;;;; Domain crafts wooden tools only.  
;;;; Updated 31 October 2020

(progn (ql:quickload "shop3"))

(in-package :shop-user)
(shop-trace ;:goals ;uncomment to see unify/binding or debugging
            :tasks ;uncomment to see tasks, primitive and compound
            :states 
            :plans)
(defdomain (craft-wood-tools.lisp :type pddl-domain :redefine-ok T) (
    (:types
      tool ingredients - object
      wood planks sticks - ingredients
      wood-axe wood-pickaxe wood-hoe wood-shovel - tool
    ); end of types

    (:predicates
      (has-wood ?w - ingredients)
      (has-planks ?p - ingredients)
      (has-sticks ?s - ingredients)
      (has-wood-axe ?wa - tool)
      (has-wood-pickaxe ?wpa - tool)
      (has-wood-hoe ?wh - tool)
      (has-wood-shovel ?ws - tool)
    ); end predicates


;;; Primitive tasks used for methods
    (:action mine-wood
      :parameters (?w - ingredients)
      :precondition (not (has-wood ?w))
      :effect (has-wood ?w)
    ); end action mine-wood

    (:action craft-planks
      :parameters (?w ?p - ingredients)
      :precondition (and (has-wood ?w)
                         (not (has-planks ?p)))
      :effect (has-planks ?p)
    ); end action craft-planks

    (:action craft-sticks
      :parameters (?p ?s - ingredients)
      :precondition (and (has-planks ?p)
                         (not (has-sticks ?s)))
      :effect (has-sticks ?s)
    ); end craft-sticks

    (:action craft-wood-axe
      :parameters (?p ?s - ingredients ?wa - tool)
      :precondition (and (has-sticks ?s)
                         (has-planks ?p))
      :effect (has-wood-axe ?wa)
    );end craft-wood-axe

    (:action craft-wood-pickaxe
      :parameters (?p ?s - ingredients ?wpa - tool)
      :precondition (and (has-sticks ?s)
                         (has-planks ?p)
                         (not (has-wood-pickaxe ?wpa)))
      :effect (has-wood-pickaxe ?wpa)
    );end craft-wood-pickaxe

    (:action craft-wood-hoe
      :parameters (?p ?s - ingredients ?wh - tool)
      :precondition (and (has-sticks ?s)
                         (has-planks ?p)
                         (not (has-wood-hoe ?wh)))
      :effect (has-wood-hoe ?wh)
    );end craft-wood-hoe

    (:action craft-wood-shovel
      :parameters (?p ?s - ingredients ?ws - tool)
      :precondition (and (has-sticks ?s)
                         (has-planks ?p)
                         )
      :effect (has-wood-shovel ?ws)
    );end craft-wood-shovel


;;; Tasks and methods for crafting tools
    (:method (craft-wood-axe ?wa)
       mine-wood
       (not (has-wood ?w))
       (:ordered (:task !mine-wood ?w)
                 (:task craft-wood-axe ?wa))

       craft-planks
       (and (has-wood ?w) (not (has-planks ?p)))
       (:ordered (:task !craft-planks ?w ?p)
                 (:task craft-wood-axe ?wa))

       craft-sticks
       (and (has-planks ?p) (not (has-sticks ?s)))
       (:ordered (:task !craft-sticks ?p ?s)
                 (:task craft-wood-axe ?wa))

       craft-wood-axe
       (and (has-planks ?p) (has-sticks ?s)
            (not (has-wood-axe ?wa)))
       (:ordered (:task !craft-wood-axe ?p ?s ?wa)
                 (:task craft-wood-axe ?wa))

       mission-done
       ()
       ()
    );end method craft-wood-axe

    (:method (craft-wood-pickaxe ?wpa)
       mine-wood
       (not (has-wood ?w))
       (:ordered (:task !mine-wood ?w)
                 (:task craft-wood-pickaxe ?wpa))

       craft-planks
       (and (has-wood ?w) (not (has-planks ?p)))
       (:ordered (:task !craft-planks ?w ?p)
                 (:task craft-wood-pickaxe ?wpa))

       craft-sticks
       (and (has-planks ?p) (not (has-sticks ?s)))
       (:ordered (:task !craft-sticks ?p ?s)
                 (:task craft-wood-pickaxe ?wpa))

       craft-wood-pickaxe
       (and (has-planks ?p) (has-sticks ?s)
            (not (has-wood-pickaxe ?wpa)))
       (:ordered (:task !craft-wood-pickaxe ?p ?s ?wpa)
                 (:task craft-wood-pickaxe ?wpa))

       mission-done
       ()
       ()
    );end method craft-wood-pickaxe

    (:method (craft-wood-hoe ?wh)
       mine-wood
       (not (has-wood ?w))
       (:ordered (:task !mine-wood ?w)
                 (:task craft-wood-hoe ?wh))

       craft-planks
       (and (has-wood ?w) (not (has-planks ?p)))
       (:ordered (:task !craft-planks ?w ?p)
                 (:task craft-wood-hoe ?wh))

       craft-sticks
       (and (has-planks ?p) (not (has-sticks ?s)))
       (:ordered (:task !craft-sticks ?p ?s)
                 (:task craft-wood-hoe ?wh))

       craft-wood-hoe
       (and (has-planks ?p) (has-sticks ?s) 
            (not (has-wood-hoe ?wh)))
       (:ordered (:task !craft-wood-hoe ?p ?s ?wh)
                 (:task craft-wood-hoe ?wh))

       mission-done
       ()
       ()
    );end method craft-wood-hoe

    (:method (craft-wood-shovel ?ws)
       mine-wood
       (not (has-wood ?w))
       (:ordered (:task !mine-wood ?w)
                 (:task craft-wood-shovel ?ws))

       craft-planks
       (and (has-wood ?w) (not (has-planks ?p)))
       (:ordered (:task !craft-planks ?w ?p)
                 (:task craft-wood-shovel ?ws))

       craft-sticks
       (and (has-planks ?p) (not (has-sticks ?s)))
       (:ordered (:task !craft-sticks ?p ?s)
                 (:task craft-wood-shovel ?ws))

       craft-wood-shovel
       (and (has-planks ?p) (has-sticks ?s)
            (not (has-wood-shovel ?ws)))
       (:ordered (:task !craft-wood-shovel ?p ?s ?ws)
                 (:task craft-wood-shovel ?ws))

       mission-done
       ()
       ()
    );end method craft-wood-shovel
  )   
); end defdomain


;;; To craft a wooden tool, redefine the problem as described
    ; in commentary. A single ';' refers to following line only.
;(defproblem craft-wood-<enter tool desired>-problem
; add relevant predicates. To start with nothing, use (NIL)
(defproblem craft-wood-shovel-problem
          ((wood w) (wood-pickaxe wpa)
           (NIL)) 
        ; ((craft-wood-<enter tool desired> <enter variable desired>)))
          ((craft-wood-pickaxe wpa)))

(find-plans 'craft-wood-shovel-problem :which :all :verbose :plans :plan-tree t)


