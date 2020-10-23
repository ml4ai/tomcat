;;;; This domain currently crafts a wood axe. Domain and problem are defined 
   ; in this same file. (See below))

;;; Notes to self: 
   ; No arithmetic in shop3, increment like in knights-tour example (n1 n2 n3)
   ; Numeric fluents
   ; Updated 23 Oct 2020
;;; The code runs the plan now, but...
    ; I need to finish some sort of action in the beginning so that
       ; mine-wood is not skipped.


(progn (ql:quickload "shop3"))

(in-package :shop-user)
(shop-trace :tasks :states :plans)
(defdomain (mine-wood.lisp :type pddl-domain :redefine-ok T) (
    (:types
      tool ingredients - object
      wood planks sticks begin - ingredients
      wood-axe - tool
    ); end of types

    (:predicates
      (to-begin ?b - ingredients)
      (has-wood ?w - ingredients)
      (has-planks ?p - ingredients)
      (has-sticks ?s - ingredients)
      (has-wood-axe ?wa - tool)
;      (has-wood-pickaxe ?wpx - tool)
    ); end predicates

;    (:action begin-mission
;      :parameters (?b - ingredients)
;      :precondition (to-begin ?b)
;      :effect ()
;      );end action begin-mission

    (:action mine-wood
      :parameters (?b ?w - ingredients)
      :precondition (to-begin ?b)
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


    (:method (craft-wood-axe ?wa)
 ;      begin-mission
 ;      (not (to-begin ?b))
 ;      (:ordered (:task !to-begin ?b)
 ;                (:task craft-axe ?wa))      
       mine-wood
       (not (has-wood ?w))
       (:ordered (:task !mine-wood ?w)
                 (:task craft-axe ?wa))

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
  )   
); end defdomain


(defproblem craft-wood-axe-problem
          ;  ((begin b)
          ;   (to-begin b)) 
          ;   (has-wood w) (has-planks p) (has-sticks s) (has-wood-axe wa))
          ((wood w)
           (has-wood w)) 
          ((craft-wood-axe wa)))

(find-plans 'craft-wood-axe-problem :which :all :verbose :plans :plan-tree t)




