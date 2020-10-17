;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; high-task-CraftAxe-domain.lisp
;; Salena Torres Ashton     
;; Created 5 October 2020
;; ToMCAT Planning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
 This domain crafts wooden, stone and iron axes. 
 The domain does not require mining any wood or stone, but requires mining 
    iron ore and fuel. Because you cannot mine wood, planks and sticks are
    assumed to be in agent's inventory and do not deplete.

Problem must be initialized for agent: (has-obj sticks planks stone)
   These items are mentioned because agent still needs to place them on
   the crafting table.

Tools and furnace used are not depleted.
|#

(define (domain high-task-CraftAxe-domain) 
  (:requirements :strips :typing)
  (:types
    object fuel tool blade 
    ) ;;end types

  (:predicates 
    ;; keep highest type level for future refactoring?
    (has-obj ?o - object)        ;; ingredients, supplies, mined ore
    (has-ingot ?o - object)      ;; to craft blade. 
    (has-fuel ?f - fuel)         ;; ingredients, items or fuel used to burn
    (has-tool? ?t - tool)        ;; crafted tool, furnace
    (has-blade ?b - blade)       ;; need to craft tool
    (choose-blade ?b - blade)    ;; higher-level choice
    (placed-blade ?b - blade)    ;; to craft blade
    (placed-fuel ?fuel - fuel)   ;; to prepare for smelting
    (placed-ore ?ore - obj)      ;; to prepae for smelting
    (placed-ingot ?obj -obj)     ;; to prepare for crafting a blade
    (smelt-ore ?o -object ?f - fuel) ;; to smelt ores
    ) ;; end predicates

;; fuel and iron ore are the only materials you need to mine
  (:action mine-fuel
          ;; fuel can be coal, kelp, bamboo, any wood or wooden item
          :parameters (?fuel - object)
          :precondition ()  ;; nothing needed to mine wood
          :effects (and (has-obj ?fuel))
          )

  (:action mine-iron-ore
          :parameters (?ore - object ?pickaxe - tool)
          :precondition (and (has-tool ?stone-pickaxe))
          :effect (and (has-obj ?iron-ore))
          )

  ;; Does not specify spatial placing of ingredients. This is just me
  ;;    working through the logic of having and using items.
  (:action place-sticks
          :parameters (?sticks - object)
          :precondition (and (has-obj ?sticks))
          :effect (and (placed-sticks ?sticks))
          )

;; Investigate: I originally had these next two actions, then commented them
    ;; out so it would be less had-coded. Now I'm wondering once again.
  (:action place-planks
          :parameters (wooden - blade)
          :precondition (and (has-blade wooden))
          :effect (and (placed-blade wooden))
          )

  ;; In this domain, stone is simply called 'stone' since there is 
      ;; no need to mine stone. Other domains call this 'm-stone'.
  (:action place-stone 
          :parameters (stone - blade)
          :precondition (and (has-blade stone))
          :effect (and (placed-blade stone))
          )

  ;; use for wooden, stone or iron. I might delete this.
  (:action place-blade
          :parameters (?b - blade)
          :precondition (and (has-blade ?b))
          :effect (and (placed-blade ?b))
          )

  ;; to prepare for smelting ore and many other uses
  (:action place-fuel
           :parameters (?fuel - fuel ?furnace - tool)
           :precondition (and (has-fuel ?fuel)
                              (has-furnace ?furnace))
           :effect (and (placed-fuel ?fuel))
           )

  ;; to prepare for smelting ore
  (:action place-iron-ore
           :parameters (?ore - object ?furnace - tool)
           :precondition (and (has-ore ?ore)
                              (has-furnace ?furnace))
           :effect (and (placed-ore ?ore))
           )

  ;; smelt ore in order to craft ingots
  (:action smelt-ore
           :parameters (?fuel ?ore ?o - object)
           :precondition (and (has-ore ?ore)
                              (has-fuel ?fuel))
           :effect (and (has-ingot ?o))
           )

  ;; to craft an iron tool or weapon
  (:action place-ingot
           :parameters (?ingot - object)
           :precondition (and (has-ingot ?ingot))
           :effect (and (placed-ingot ?ingot))
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

  ;; Needed to mine iron ore. Since stone is in inventory, there
      ;; no need to craft a wooden pick axe.
  (:method (craft-stone-pickaxe ?sticks ?b ?tool)
           placed-sticks
           (and (placed-sticks ?sticks))

           placed-blade
           (and (placed-blade ?b))

           has-tool
           (and (has-tool ?stone-pickaxe))
  );;end craft-stone-pickaxe method

  (:method (craft-iron-axe ?sticks ?o ?iron-axe)
;; To "fail faster", do I need to explicitly state?:
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

