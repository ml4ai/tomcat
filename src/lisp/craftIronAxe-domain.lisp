
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CraftIronAxe.lisp
;; Salena Torres Ashton     
;; Created 26 September 2020
;; INFO 699 and ToMCAT Planning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
 This domain crafts wooden, stone and iron axes. 
 The domain does not currently increase inventory numerically after 
    mining or crafting. It does not decrease inventory after crafting.
    the domain assumes that mining or crafting of item completely fills
    and depletes inventory of said item's ingredients.
 Tools used are not depleted.


 To Do:  * Code inventory numerically
         * PDDL/ SHOP3 syntax
  
 To make more domain independent, actions now do not have specific
    crafting names or materials/supplies for ingredients.
 Older code I'm not ready to delete is currently commented out.
 Note on names:
      'wood'   = any raw-mined wood 
      'wooden' = any crafted wood for blade type
|#

(define (domain craft-iron-axe)
  (:requirements :strips :typing)
  (:types
    object fuel tool blade 
    ) ;;end types

  (:predicates 
    ;; keep highest type level for future refactoring?
    (has-obj ?o - object)        ;; ingredients, supplies, mined ore
    (has-ingot ?o - object)      ;; to craft blade. Must be diff than handle
    (has-fuel ?f - fuel)         ;; ingredients, items or fuel used to burn
    (has-tool? ?t - tool)        ;; crafted tool, furnace
    (has-blade ?b - blade)       ;; need to craft tool
    (choose-blade ?b - blade)    ;; higher-level choice
    (placed-sticks ?o - object)  ;; to craft handle
    (placed-blade ?b - blade)    ;; to craft blade
    (placed-fuel ?fuel - fuel)   ;; to prepare for smelting
    (placed-ore ?ore - obj)      ;; to prepae for smelting
    (placed-ingot ?obj -obj)     ;; to prepare for crafting a blade
    (smelt-ore ?o -object ?f - fuel) ;; to smelt ores
    ) ;; end predicates


  ;; Most-primitive actions
  (:action mine-wood
          :parameters (?wood - object)
          :precondition ()  ;; nothing needed to mine wood
          :effects (and (has-obj ?wood))
          )

  (:action mine-fuel
          ;; fuel can be coal, kelp, bamboo, any wood or wooden item
          :parameters (?fuel - object)
          :precondition ()  ;; nothing needed to mine wood
          :effects (and (has-obj ?fuel))
          )


  ;; Less-primitive actions
  (:action craft-planks ;; for now, makes planks and wooden blades
          :parameters (?wood ?planks - object ?wooden - blade )
          :precondition (and (has-obj ?wood))
          :effect (and (has-obj ?planks)
                       (has-blade ?wooden)
                       (not (has-obj ?wood)))
          )

  (:action craft-sticks ;; depletes planks for object, not for blades
          :parameters (?planks ?sticks - object)
          :precondition (and (has-obj ?planks))
          :effect (and (has-obj ?sticks)
                       (not (has-obj ?planks)))
          )

  (:action mine-stone
          :parameters (?stone - blade ?pickaxe - tool)
          :precondition (and (has-tool ?pickaxe))
          :effect (and (has-blade ?stone))
          )

  (:action mine-iron-ore
          :parameters (?ore - object ?pickaxe - tool)
          :precondition (and (has-tool ?stone-pickaxe))
          :effect (and (has-obj ?iron-ore))
          )



  ;; Higher Actions
  ;; Does not specify spatial placing of ingredients. This is just me
  ;;    working through the logic of having and using items.
  (:action place-sticks
          :parameters (?sticks - object)
          :precondition (and (has-obj ?sticks))
          :effect (and (placed-sticks ?sticks)
                       (not (has-obj ?sticks)))
          )
 


;; Investigate: I orginally had these next two actions, then commented them
    ;; out so it would be less had-coded. Now I'm wondering once again.

  (:action place-planks
          :parameters (wooden - blade)
          :precondition (and (has-blade wooden))
          :effect (and (placed-blade wooden)
                       (not (has-blade wooden)))
          )

  (:action place-m-stone
          :parameters (m-stone - blade)
          :precondition (and (has-blade m-stone))
          :effect (and (placed-blade m-stone)
                       (not (has-blade m-stone)))
          )

 I commented out the material-specific blades above.
    However, you can't mine ore with wooden pickaxe, so
    I might add this back in. For now, I'm keeping below:
|#

  (:action place-blade
          :parameters (?b - blade)
          :precondition (and (has-blade ?b))
          :effect (and (placed-blade ?b
                       (not (has-blade ?b)))
          )

  ;; to prepare for smelting ore and many other uses
  (:action place-fuel
           :parameters (?fuel - fuel ?furnace - tool)
           :precondition (and (has-fuel ?fuel)
                              (has-furnace ?furnace))
           :effect (and (placed-fuel ?fuel)
                       (not (has-fuel ?fuel)))
           ;; does not deplete furnace
           )


  ;; to prepare for smelting ore
  (:action place-iron-ore
           :parameters (?ore - object ?furnace - tool)
           :precondition (and (has-ore ?ore)
                              (has-furnace ?furnace))
           :effect (and (placed-ore ?ore)
                        (not (has-ore ?ore)))
           ;; does not deplete furnace
           )

  ;; smelt ore in order to craft ingots
  (:action smelt-ore
           :parameters (?fuel ?ore ?o - object)
           :precondition (and (has-ore ?ore)
                              (has-fuel ?fuel))
           :effect (and (has-ingot ?o)
                        (not (has-ore ?ore)
                             (has-fuel ?fuel)))
           ;; does not deplete furnace
           )

  ;; to craft an iron tool or weapon
  (:action place-ingot
           :parameters (?ingot - object)
           :precondition (and (has-ingot ?ingot))
           :effect (and (placed-ingot ?ingot)
                        (not (has-ingot ?ingot)))
           )


  ;; Methods
  ;; to craft a wooden or stone axe
  (:method (craft-axe ?sticks ?b ?axe)
           placed-sticks
           (and (placed-sticks ?sticks))

           placed-blade
           (and (placed-blade ?b))

           has-tool
           (and (has-tool ?axe))
  );;end craft-axe method

  ;; separate from stone pick axe due to preconditions
      ;; wooden's inability to mine iron ore
  (:method (craft-wooden-pickaxe ?sticks ?b ?tool)
           placed-sticks
           (and (placed-sticks ?sticks))

           placed-blade
           (and (placed-blade ?b))

           has-tool
           (and (has-tool ?wooden-pickaxe))
  );;end craft-wooden-pickaxe method


  ;; Needed to mine iron ore
  (:method (craft-stone-pickaxe ?sticks ?b ?tool)
           placed-sticks
           (and (placed-sticks ?sticks))

           placed-blade
           (and (placed-blade ?b))

           has-tool
           (and (has-tool ?stone-pickaxe))
  );;end craft-stone-pickaxe method


  (:method (craft-iron-axe ?sticks ?o ?iron-axe)
;; Salena's to-do: 
     ;; lift constraints to help decision of higher level task
     ;; continue working on LISP syntax
     ;; syntax for this particular problem
;; The first subtask, placed-ingot, will prevent this method from taking effect
;;      unless (placed-ingot ?obj) is true.
;; You cannot create an iron axe without an ingot, which you cannot smelt
;;      having a stone pickaxe.

;; Do I need to explicitly state?:
    (:preconditions (and (placed-ingot ?obj)
                       (placed-sticks ?sticks)
                  )
        )

           placed-ingot
           (and (placed-ingot ?o))

           placed-sticks
           (and (placed-sticks ?sticks))

           has-tool
           (and (has-tool ?iron-axe))
  );; end craft-iron-axe method


);; end domain



