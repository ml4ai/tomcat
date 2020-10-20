
#| Differences:
    Use of conditional preconditions, effects, and/or goals
    Soft Goals and preferences in preconditions and/or goals

This domain enables the crafting of an axe with a blade made from wood,
   stone or iron. This domain has the greatest amount of low-level tasks
   that respect the physics of the Minecraft domain. Additional files will
   introduce heuristics or otherwise relaxed problems. 

This domain does not currently deplete inventory when ingredients or tools
  are used. That update will be in a future domain.

 to do: Syntax for SHOP3
        preferences and weapons?
        Cost functions or penalties
        Added constraints

For now, the crafting of a bow will need to be enough. When I get this
  working, the arrow can be developed. Easier to assume a bow comes with arrow
  for two reasons: It's the only item in this domain that requires three
  different ingredients and the only item to require feathers.|#

(progn (ql:quickload "shop3"))

(in-package :shop-user)

(defdomain (domain-classical-craft :type pddl-domain :redefine-okay T) ()
  (:types (ingredients tool weapon - object)
          (wood stone ore ingot flint strings cobwebs - ingredients)
            (planks sticks - wood)

          ;; flintSteel spelled this way is used only for a tool
          (axe hoe pickaxe shovel flintSteel furnace - tool)

          ;; flintSteal mispelled this way is used only as a weapon
          ;; for now, crafting a bow yields a bow and arrow.
          (sword bow flintSteal - weapon))

  (:predicates (has-wood ?w 
                has-stone ?s
                has-ore ?o
                has-ingot ?i
                has-fuel ?f - ingredients)
               
               (has-planks ?p
                has-sticks ?st -wood)

               (has-wood-pickaxe ?wpx
                has-stone-pickaxe ?spx 
                has-wood-axe ?wa 
                has-stone-axe ?sa
                has-furnace - ?furnace - tool)
               
               (has-weapon ?w - weapon))


  (:action mine-wood
    :parameters (has-wood ?w - ingredients)
    :precondition ()   
    :effect (has-wood ?w))


  (:action mine-stone
    :parameters (has-stone ?s - ingredients
                 has-wood-pickaxe ?wpx- tool)
    :precondition (or (has-wood-pickaxe ?wpx)
                      (has-stone-pickaxe ?spx)
    :effect (has-stone ?s))
  

  (:action mine-ore
    :parameters (has-ore ?o - ingredients
                 has-stone-pickaxe ?spx - tool)
    :precondition (has-stone-pickaxe ?spx)
    :effects (has-ore ?o))


  (:action mine-fuel
    :parameters (has-fuel ?f - ingredients)
    :precondition ()
    :effects (has-fuel ?f))


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
    :effect (has-furnace ?furnace)

  ;; craft ingots from iron ore
  (:action craft-ingot
    :parameters (has-furnace ?furnace - tool 
                 has-fuel ?f
                 has-ore ?o - ingredient) 
    :precondition (and (has-furnace ?furnace)
                       (has-fuel ?f)
                       (has-ore ?o))
    :effect (has-ingot ?i
    )

  ;;; Craft flint and steel. This action is repeated twice because flint and
     ;;; steel can be used for a tool or weapon. 
     ;;; As a tool, I spell it flintSteel.
     ;;; As a weapon, I spell it flintSteal. 
     ;;; ??? Should I make this distinction or let the agent do so?  
  (:action craft-flintSteel
    :parameters (has-tool ?flint - tool
                 has-ingredient ?flint ?ingot - ingredient)
    :precondition(and (has-ingredient ?flint)
                      (has-ingredient ?ingot))
    :effect (has-tool ?flint)
    )

  (:action craft-flintSteal
    :parameters (has-weapon ?flint - weapon
                 has-ingredient ?flint ?ingot - ingredient)
    :precondition(and (has-ingredient ?flint)
                      (has-ingredient ?ingot))
    :effect (has-weapon ?flint)
    )

  (:action craft-wood-axe
    :parameters (has-sticks ?st 
                 has-planks ?p - wood 
                 has-wood-axe ?wa - tool)
    :precondition (and (has-sticks ?st)
                       (has-planks ?p))
    :effect (has-wood-axe ?wa)))

  (:action craft-stone-axe
    :parameters (has-sticks ?st 
                 has-stone ?s - ingredients 
                 has-stone-axe ?sa - tool)
    :precondition (and (has-sticks ?st)
                       (has-stone)?s)
    :effect (has-stone-axe ?sa)))


  ;;; crafts hoes with blades made of wood, stone or iron
  (:action craft-hoe
    :parameters (has-ingredient ?sticks ?planks - wood 
                 has-ingredient ?stone - stone 
                 has-ingredient ?ingot - metal
                 has-tool ?tool - tool)
    :precondition (and (has-ingredient ?sticks)
                       (exists (or (?planks - ingredient)
                                   (?stone - stone)
                                   (?ingot - metal))))
    :effect (forall (?sticks - ingredient)
                    (and (when (has-ingredient ?planks)
                               (has-tool ?wood-hoe))
                         (when (has-ingredient ?stone)
                               (has-tool ?stone-hoe))
                         (when (has-ingredient ?ingot)
                               (has-tool ?iron-hoe))))
    )

  ;;; crafts shovel with blades made of wood, stone or iron
  (:action craft-shovel
    :parameters (has-ingredient ?sticks ?planks - wood 
                 has-ingredient ?stone - stone 
                 has-ingredient ?ingot - metal
                 has-tool ?tool - tool)
    :precondition (and (has-ingredient ?sticks)
                       (exists (or (?planks - ingredient)
                                   (?stone - stone)
                                   (?ingot - metal))))
    :effect (forall (?sticks - ingredient)
                    (and (when (has-ingredient ?planks)
                               (has-tool ?wood-shovel))
                         (when (has-ingredient ?stone)
                               (has-tool ?stone-shovel))
                         (when (has-ingredient ?ingot)
                               (has-tool ?iron-shovel))))
    )
  
  (:action craft-wood-pickaxe
    :parameters (has-sticks ?st 
                 has-planks ?p - wood
                 has-wood-pickaxe ?wpx - tool)
    :precondition (and (has-sticks ?st)
                       (has-planks ?p)
    :effect (has-wood-pickaxe ?wpx)))  
  ); end defdomain
  


