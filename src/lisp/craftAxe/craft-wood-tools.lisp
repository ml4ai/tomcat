;;;; This domain currently crafts wooden tools. Domain and problem are defined 
   ; in this same file. (See below))

;; TO RUN PROGRAM: on terminal, enter command:
 ;                              sbcl --load craft-wood-tools.lisp --quit

;;; Notes to self: 
   ; No arithmetic in shop3, increment like in knights-tour example (n1 n2 n3)
   ; Numeric fluents
   ; Updated 26 Oct 2020
;;; The code runs the plan now, and I will build up from here to include
   ; building of other tools and weapons from stone and iron.

(progn (ql:quickload "shop3"))

(in-package :shop-user)
(shop-trace :tasks :states :plans)
(defdomain (mine-wood.lisp :type pddl-domain :redefine-ok T) (
    (:types
      tool ingredients - object
      wood planks sticks begin - ingredients
      wood-axe wood-pickaxe wood-hoe wood-shovel - tool
    ); end of types

    (:predicates
      (to-begin ?b - ingredients)
      (has-wood ?w - ingredients)
      (has-planks ?p - ingredients)
      (has-sticks ?s - ingredients)
      (has-wood-axe ?wa - tool)
      (has-wood-pickaxe ?wpa - tool)
      (has-wood-hoe ?wh - tool)
      (has-wood-shovel ?ws - tool)
    ); end predicates

    (:action mine-wood
      :parameters (?w - ingredients)
      :precondition (not (has-wood ?w))
      :effect (has-wood ?w)
    ); end action mine-wood

    (:action craft-planks
      :parameters (?w ?p - ingredients)
      :precondition (has-wood ?w)
      :effect (has-planks ?p)
    ); end action craft-planks

    (:action craft-sticks
      :parameters (?p ?s - ingredients)
      :precondition (has-planks ?p)
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
                         (has-planks ?p))
      :effect (has-wood-pickaxe ?wpa)
    );end craft-wood-pickaxe

    (:action craft-wood-hoe
      :parameters (?p ?s - ingredients ?wh - tool)
      :precondition (and (has-sticks ?s)
                         (has-planks ?p))
      :effect (has-wood-pickaxe ?wh)
    );end craft-wood-hoe

    (:action craft-wood-shovel
      :parameters (?p ?s - ingredients ?ws - tool)
      :precondition (and (has-sticks ?s)
                         (has-planks ?p))
      :effect (has-wood-shovel ?ws)
    );end craft-wood-shovel


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

       mission-done
       ()
       ()
    );end method craft-axe

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

       mission-done
       ()
       ()
    );end method craft-pickaxe

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

       mission-done
       ()
       ()
    );end method craft-hoe

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

       mission-done
       ()
       ()
    );end method craft-pickaxe
  )   
); end defdomain



;;; To craft a wooden tool, redefine the problem as described
    ; in commentary. A single ';' refers to following line only.
;(defproblem craft-wood-<enter tool desired>-problem
(defproblem craft-wood-shovel-problem
          ((wood w)
           (not (has-wood w))) 
        ; ((craft-wood-<enter tool desired> <enter variable desired>)))
          ((craft-wood-shovel ws)))

(find-plans 'craft-wood-shovel-problem :which :all :verbose :plans :plan-tree t)




