
(progn (ql:quickload "shop3"))

(in-package :shop-user)
(shop-trace ;:tasks 
            :states 
            :plans
            ;:goals ;uncomment for unifyer/binder or debugging
            :methods
            )
(defdomain (craft-stone-tools.lisp :type pddl-domain :redefine-ok T) (
    (:types
      tool ingredients - object
      wood planks sticks stone - ingredients
      stone-axe stone-pickaxe stone-hoe stone-shovel - tool 
      wood-axe wood-pickaxe wood-hoe wood-shovel - tool
    ); end of types

    (:predicates
      (has-wood ?w - ingredients)
      (has-stone ?st - ingredients)
      (has-planks ?p - ingredients)
      (has-sticks ?s - ingredients)
      (has-wood-axe ?wa - tool)
      (has-wood-pickaxe ?wpa - tool)
      (has-wood-hoe ?wh - tool)
      (has-wood-shovel ?ws - tool)
      (has-stone-axe ?sta - tool)
      (has-stone-pickaxe ?stpa - tool)
      (has-stone-hoe ?sth - tool)
      (has-stone-shovel ?sts - tool)
    ); end predicates

;;; mining tasks used for primitive tasks and methods    
    (:action mine-wood
      :parameters (?w - ingredients)
      :precondition (not (has-wood ?w))
      :effect (has-wood ?w)
    ); end action mine-wood

    (:action mine-stone
      :parameters (?st - ingredients
                   ?wpa - tool)
      :precondition (and (not (has-stone ?st))
                         (has-wood-pickaxe ?wpa))
      :effect (has-stone ?st)
    ); end action mine-stone


;;; primitive tasks used for methods
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


;;; crafting wood items    
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
                         (has-planks ?p))
      :effect (has-wood-shovel ?ws)
    );end craft-wood-shovel











;;; Craft Stone Items
    (:action craft-stone-axe
      :parameters (?st ?p ?s - ingredients ?sta - tool)
      :precondition (and (has-planks ?p) (has-sticks ?s)
                         (has-stone-axe ?sta)
                         (not (has-stone-axe ?sta)))
      :effect (has-stone-axe ?sta)
    );end craft-stone-axe

    (:action craft-stone-pickaxe
      :parameters (?st ?p ?s - ingredients ?stpa - tool)
      :precondition (and (has-planks ?p) (has-sticks ?s)
                         (has-stone-pickaxe ?stpa)
                         (not (has-stone-pickaxe ?stpa)))
      :effect (has-stone-pickaxe ?stpa)
    );end craft-stone-pickaxe

    (:action craft-stone-hoe
      :parameters (?st ?p ?s - ingredients ?sth - tool)
      :precondition (and (has-planks ?p) (has-sticks ?s)
                         (has-stone-hoe ?sth)
                         (not (has-stone-hoe ?sth)))
      :effect (has-stone-hoe ?sth)
    );end craft-stone-hoe

    (:action craft-stone-shovel
      :parameters (?st ?p ?s - ingredients ?sts - tool)
      :precondition (and (has-planks ?p) (has-sticks ?s)
                         (has-stone-shovel ?sts)
                         (not (has-stone-shovel ?sts)))
      :effect (has-stone-shovel ?sts)
    );end craft-stone-shovel


;;; Methods

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



    (:method (mine-stone ?st)
       mine-wood
       (not (has-wood ?w))
       (:ordered (:task !mine-wood ?w)
                 (:task mine-stone ?st))

       craft-planks
       (and (has-wood ?w) (not (has-planks ?p)))
       (:ordered (:task !craft-planks ?w ?p)
                 (:task mine-stone ?st))

       craft-sticks
       (and (has-planks ?p) (not (has-sticks ?s)))
       (:ordered (:task !craft-sticks ?p ?s)
                 (:task mine-stone ?st))

       craft-wood-pickaxe
       (and (has-planks ?p) (has-sticks ?s)
            (not (has-wood-pickaxe ?wpa)))
       (:ordered (:task !craft-wood-pickaxe ?p ?s ?wpa)
                 (:task mine-stone ?st))

       mine-stone
       (and (not (has-stone ?st)) (has-wood-pickaxe ?wpa))
       (:ordered (:task !mine-stone ?st ?wpa)
                 (:task mine-stone ?st))

       mission-done
       ()
       ()
    );end method mine-stone

    
    (:method (craft-stone-axe ?sta)
       mine-wood
       (not (has-wood ?w))
       (:ordered (:task !mine-wood ?w)
                 (:task craft-stone-axe ?sta))

       craft-planks
       (and (has-wood ?w) (not (has-planks ?p)))
       (:ordered (:task !craft-planks ?w ?p)
                 (:task craft-stone-axe ?sta))

       craft-sticks
       (and (has-planks ?p) (not (has-sticks ?s)))
       (:ordered (:task !craft-sticks ?p ?s)
                 (:task craft-stone-axe ?sta))

       craft-wood-pickaxe
       (and (has-planks ?p) (has-sticks ?s)
            (not (has-wood-pickaxe ?wpa)))
       (:ordered (:task !craft-wood-pickaxe ?p ?s ?wpa)
                 (:task craft-wood-pickaxe ?wpa))

       mine-stone
       (and (has-wood-pickaxe ?wpa) (not (have-stone ?st)))
       (:ordered (:task !mine-stone ?st)
                 (:task craft-stone-axe ?sta))
       
       craft-stone-axe
       (and (has-planks ?p) (has-sticks ?s) 
            (has-wood-pickaxe ?wpa) (has-stone ?st)
            (not (has-stone-axe ?sta)))
       (:ordered (:task !craft-stone-axe ?p ?s ?wpa ?st ?sta)
                 (:task craft-stone-axe ?sta))

       mission-done
       ()
       ()
    );end method craft-stone-axe

    
    (:method (craft-stone-pickaxe ?stpa)
       mine-wood
       (not (has-wood ?w))
       (:ordered (:task !mine-wood ?w)
                 (:task craft-stone-pickaxe ?stpa))

       craft-planks
       (and (has-wood ?w) (not (has-planks ?p)))
       (:ordered (:task !craft-planks ?w ?p)
                 (:task craft-stone-pickaxe ?stpa))

       craft-sticks
       (and (has-planks ?p) (not (has-sticks ?s)))
       (:ordered (:task !craft-sticks ?p ?s)
                 (:task craft-stone-pickaxe ?stpa))

       craft-wood-pickaxe 
       (and (has-planks ?p) (has-sticks ?s) 
            (not (has-wood-pickaxe ?wpa)))
       (:task craft-wood-pickaxe ?wpa)
       
       mine-stone
       (and (has-wood-pickaxe ?wpa) (not (have-stone ?st)))
       (:ordered (:task !mine-stone ?st)
                 (:task craft-stone-pickaxe ?stpa))
            
       mission-done
       ()
       ()
    );end method craft-stone-pickaxe

    (:method (craft-stone-hoe ?sth)
       mine-wood
       (not (has-wood ?w))
       (:ordered (:task !mine-wood ?w)
                 (:task craft-stone-hoe ?sth))

       craft-planks
       (and (has-wood ?w) (not (has-planks ?p)))
       (:ordered (:task !craft-planks ?w ?p)
                 (:task craft-stone-hoe ?sth))

       craft-sticks
       (and (has-planks ?p) (not (has-sticks ?s)))
       (:ordered (:task !craft-sticks ?p ?s)
                 (:task craft-stone-hoe ?sth))

       craft-wood-pickaxe 
       (and (has-planks ?p) (has-sticks ?s) 
            (not (has-wood-pickaxe ?wpa)))
       (:task craft-wood-pickaxe ?wpa)
       
       mine-stone
       (and (has-wood-pickaxe ?wpa) (not (have-stone ?st)))
       (:ordered (:task !mine-stone ?st)
                 (:task craft-stone-hoe ?sth))
            
       mission-done
       ()
       ()
    );end method craft-stone-hoe


    (:method (craft-stone-shovel ?sts)
       mine-wood
       (not (has-wood ?w))
       (:ordered (:task !mine-wood ?w)
                 (:task craft-stone-shovel ?sts))

       craft-planks
       (and (has-wood ?w) (not (has-planks ?p)))
       (:ordered (:task !craft-planks ?w ?p)
                 (:task craft-stone-shovel ?sts))

       craft-sticks
       (and (has-planks ?p) (not (has-sticks ?s)))
       (:ordered (:task !craft-sticks ?p ?s)
                 (:task craft-stone-shovel ?sts))

       craft-wood-pickaxe 
       (and (has-planks ?p) (has-sticks ?s) 
            (not (has-wood-pickaxe ?wpa)))
       (:task craft-wood-pickaxe ?wpa)
       
       mine-stone
       (and (has-wood-pickaxe ?wpa) (not (have-stone ?st)))
       (:ordered (:task !mine-stone ?st)
                 (:task craft-stone-shovel ?sts))
            
       mission-done
       ()
       ()
    );end method craft-stone-shovel
  )   
); end defdomain



;;; To craft a wood tool, redefine the problem as described
    ; in commentary. A single ';' refers to following line only.
;(defproblem craft-wood-<enter tool desired>-problem
(defproblem craft-stone-shovel-problem
          ((wood w) (wood-pickaxe wpa) (stone st)
           (NIL))
        ; ((craft-wood-<enter tool desired> <enter variable desired>)))
          ((mine-stone st)))

(find-plans 'craft-stone-shovel-problem :which :all :verbose :plans :plan-tree t)




