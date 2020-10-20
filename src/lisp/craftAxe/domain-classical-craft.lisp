
#| Differences:

This domain enables the crafting of a wood, stone, or iron axe, hoe, or pickaxe 
    using boolean-valued actions.

This domain does not track inventory when ingredients or tools are used. Further
    development of craftAxe will be in numeric and other planning domains.

 to do: Syntax for SHOP3


(progn (ql:quickload "shop3"))

(in-package :shop-user)

(defdomain (domain-classical-craft :type pddl-domain :redefine-okay T) ()
  (:types (ingredients tool weapon - object)
          (wood stone ore ingot flint - ingredients)
            (planks sticks - wood)

          (wood-axe stone-axe iron-axe
              wood-pickaxe stone-pickaxe iron-pickaxe
              wood-hoe stone-hoe iron-hoe
              flintSteel furnace - tool)

          ;; flintSteal mispelled this way is used only as a weapon
          (wood-sword stone-sword iron-sword flintSteal - weapon))
  

  (:predicates (has-wood ?w 
                has-stone ?s
                has-ore ?o
                has-ingot ?i
                has-fuel ?f 
                has-flint ?fl - ingredients)
               
               (has-planks ?p
                has-sticks ?st -wood)

               (has-wood-pickaxe ?wpx
                has-stone-pickaxe ?spx 
                has-iron-pickaxe ?ipx
                has-wood-axe ?wa 
                has-stone-axe ?sa
                has-iron-axe ?ia
                has-wood-hoe ?wh
                has-stone-hoe ?sh
                has-iron-hoe ?ih
                has-furnace - ?furnace 
                has-flint-steel ?flsteel- tool)
               
               (has-wood-sword ?wsw
                has-stone-sword ?stsw
                has-iron-sword ?isw
                has-flint-steal ?w - weapon))

;;; Mining Actions
  (:action mine-wood
    :parameters (has-wood ?w - ingredients)
    :precondition ()   
    :effect (has-wood ?w))


  (:action mine-stone
    :parameters (has-stone ?s - ingredients
                 has-wood-pickaxe ?wpx
                 has-stone-pickaxe ?wpx - tool)
    :precondition (or (has-wood-pickaxe ?wpx)
                      (has-stone-pickaxe ?spx)
    :effect (has-stone ?s))


  (:action mine-flint
    :parameters (has-flint ?fl - ingredients
                 has-wood-pickaxe ?wpx 
                 has-stone-pickaxe ?spx - tool)
    :precondition (or (has-wood-pickaxe ?wpx)
                      (has-stone-pickaxe ?spx)
    :effect (has-flint ?fl))
  

  (:action mine-ore
    :parameters (has-ore ?o - ingredients
                 has-stone-pickaxe ?spx - tool)
    :precondition (has-stone-pickaxe ?spx)
    :effects (has-ore ?o))


  (:action mine-fuel
    :parameters (has-fuel ?f - ingredients)
    :precondition ()
    :effects (has-fuel ?f))


;;; Crafting ingredients needed for crafting tools or weapons
  (:action craft-planks
    :parameters (has-wood ?w - ingredient
                has-planks ?p - wood)
    :precondition (has-wood ?w)
    :effect (has-planks ?p))


  (:action craft-sticks
    :parameters (has-planks ?p - wood
                has-sticks ?st - wood)
    :precondition (has-planks ?p)
    :effect (has-sticks ?st))


  ;; builds a furnace for smelting metal ores
  (:action build-furnace
    :parameters (has-furnace ?furnace - tool
                 has-stone ?s - ingredients)
    :precondition (has-stone ?s)
    :effect (has-furnace ?furnace))

  ;; craft ingots from iron ore
  (:action craft-ingot
    :parameters (has-furnace ?furnace - tool 
                 has-fuel ?f
                 has-ore ?o - ingredient) 
    :precondition (and (has-furnace ?furnace)
                       (has-fuel ?f)
                       (has-ore ?o))
    :effect (has-ingot ?i))


;;; Crafting Axes
  (:action craft-wood-axe
    :parameters (has-sticks ?st 
                 has-planks ?p - wood 
                 has-wood-axe ?wa - tool)
    :precondition (and (has-sticks ?st)
                       (has-planks ?p))
    :effect (has-wood-axe ?wa))


  (:action craft-stone-axe
    :parameters (has-sticks ?st 
                 has-stone ?s - ingredients 
                 has-stone-axe ?sa - tool)
    :precondition (and (has-sticks ?st)
                       (has-stone ?s))
    :effect (has-stone-axe ?sa))


  (:action craft-iron--axe
    :parameters (has-sticks ?st 
                 has-ingot ?i - ingredients 
                 has-iron-axe ?ia - tool)
    :precondition (and (has-sticks ?st)
                       (has-ingot ?i))
    :effect (has-iron-axe ?ia))



;;; Crafting Pickaxes
  (:action craft-wood-pickaxe
    :parameters (has-sticks ?st 
                 has-planks ?p - wood
                 has-wood-pickaxe ?wpx - tool)
    :precondition (and (has-sticks ?st)
                       (has-planks ?p)
    :effect (has-wood-pickaxe ?wpx)))  


  (:action craft-stone-pickaxe
    :parameters (has-sticks ?st - wood
                 has-stone ?s - ingredients
                 has-stone-pickaxe ?spx - tool)
    :precondition (and (has-sticks ?st)
                       (has-stone ?s)
    :effect (has-stone-pickaxe ?spx)))  
  

  (:action craft-iron-pickaxe
    :parameters (has-sticks ?st - wood
                 has-iron ?i - ingredients
                 has-iron-pickaxe ?ipx - tool)
    :precondition (and (has-sticks ?st)
                       (has-ingot ?i)
    :effect (has-iron-pickaxe ?ipx)))  
  
  
  
;;; Crafting hoes 
  (:action craft-wood-hoe
    :parameters (has-sticks ?st
                 has-planks ?p - wood
                 has-wood-hoe ?wh - tool)
    :precondition (and (has-sticks ?st)
                       (has-planks ?p)
    :effect (has-wood-hoe ?wh)))  
  
  (:action craft-stone-hoe
    :parameters (has-sticks ?st - wood
                 has-stone ?s- ingredients
                 has-stone-hoe ?sh - tool)
    :precondition (and (has-sticks ?st)
                       (has-stone ?s)
    :effect (has-stone-hoe ?sh)))  
  
  (:action craft-iron-hoe
    :parameters (has-sticks ?st - wood
                 has-ingot ?i- ingredients
                 has-iron-hoe ?ih - tool)
    :precondition (and (has-sticks ?st)
                       (has-ingot ?i)
    :effect (has-iron-hoe ?ih)))  
  
 
;;; Crafting Swords
  (:action craft-wood-sword
    :parameters (has-sticks ?st
                 has-planks ?p - wood
                 has-wood-sword ?wsw - weapon)
    :precondition (and (has-sticks ?st)
                       (has-planks ?p)
    :effect (has-wood-sword ?wsw)))  
  

  (:action craft-stone-sword
    :parameters (has-sticks ?st - wood
                 has-stone ?s - ingredients
                 has-stone-sword ?ssw - weapon)
    :precondition (and (has-sticks ?st)
                       (has-stone ?s)
    :effect (has-stone-sword ?ssw)))  
  
  (:action craft-iron-sword
    :parameters (has-sticks ?st - wood
                 has-ingot ?i - ingredients
                 has-iron-sword ?isw - weapon)
    :precondition (and (has-sticks ?st)
                       (has-ingot ?i)
    :effect (has-iron-sword ?isw)))  
  

;;; Craft flint and steel. This action is repeated twice because flint and
     ; steel can be used for a tool or weapon. 
     ; As a tool, I spell it flintSteel.
     ; As a weapon, I spell it flintSteal. 
     ; ??? Should I make this distinction or let the agent do so?  

  (:action craft-flint-steel
    :parameters (has-flint-steel ?flsteel - tool
                 has-flint ?fl
                 has-ingot ?i - ingredient)
    :precondition(and (has-flint ?fl)
                      (has-ingot ?i))
    :effect (has-flint-steel ?flsteel))

  (:action craft-flint-steal
    :parameters (has-flint-steal ?flsteal - weapon 
                 has-flint ?fl
                 has-ingot ?i - ingredient)
    :precondition(and (has-flint ?fl)
                      (has-ingot ?i))
    :effect (has-flint-steal ?flsteal))

); end defdomain
