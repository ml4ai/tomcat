
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

(defdomain (craftAxe-2 :type pddl-domain :redefine-okay T) ()
  (:types (ingredients tool weapon junk - object)
          (wood metal stone animal - ingredients)
            (logs planks sticks - wood)
          (stone flint - stone) ; can subtype have same name as parent type?
            (ore ingot - metal)
            (strings cobwebs - animal)

          ;; flintSteel spelled this way is used only for a tool
          (axe hoe pickaxe shovel flintSteel furnace - tool)

          ;; flintSteal mispelled this way is used only as a weapon
          ;; for now, crafting a bow yields a bow and arrow.
          (sword bow flintSteal - weapon) 

          ;; Junk items introduced since crafting is similar to tools/weapons
          ;; Items not needed for mission, later for uncertainty
          (fishingRod gate loom - junk) 
    )

  (:predicates (has-ingredient ?h - ingredients)
               ;; Later, when agent places numbered items on table, the number
               ;; will indicate what they plan to craft. All items in this
               ;; domain require two different ingredient types. The difference
               ;; between is-crafted and has-tool/has-weapon is that the former
               ;; doesn't tell you agent intent. The latter tells you exactly
               ;; what the agent has in hand.
               (is-crafted ?c1 ?c2 - ingredients) 
               (has-tool ?t - tool)
               (has-pickaxe ?px - pickaxe)
               (has-weapon ?w - weapon)
               ;; These junk items have similar recipes to tools and weapons. 
               (is-useless ?j - junk)
    )

  ;;; mine raw ingredients wood, fuel, stone, flint, and iron ore simplified so that 
    ;;; any type pickaxe will mine iron ore
  (:action mine-something
    :parameters (has-ingredient ?m - ingredient)
    :precondition ()   ; none. There is conditional effect below.
    :effect (and (when (not has-pickaxe ?px)
                       (and (has-ingredient wood)
                            (has-ingredient fuel))); calling wood fuel for simplicity
                 (when (has-pickaxe ?px) 
                       (and (has-ingredient stone)
                            (has-ingredient flint)))
                 (when (and (has-pickaxe ?px) 
                           (has-ingredient stone)
                       (has-ingredient ore))))
    );; end action mine-something

  
  ;;; simplified action that crafts sticks and wood with one action
  (:action craft-wood
    :parameters (has-ingredient ?logs ?planks ?sticks - wood)
    :precondition (has-ingredient ?logs)
    :effect (and (has-ingredient ?planks)
                 (has-ingredient ?sticks)
    )

  ;; builds a furnace for smelting metal ores
  (:action build-furnace
    :parameters (has-tool furnace - tool
                 has-ingredients ?stone - stone)
    :precondition (has-ingredients ?stone)
    :effect (has-tool furnace)

  ;; craft ingots from iron ore
  (:action craft-ingot
    :parameters (has-tool furnace - tool 
                 has-ingredient ?fuel ?ore - ingredient) 
    :precondition (and (has-tool furnace)
                       (has-ingredient ?fuel ?ore))
    :effect (has-ingredient ?ore)
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

  ;;; crafts axes with blades made of wood, stone or iron
  (:action craft-axe
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
                               (has-tool ?wood-axe))
                         (when (has-ingredient ?stone)
                               (has-tool ?stone-axe))
                         (when (has-ingredient ?ingot)
                               (has-tool ?iron-axe))))
    )

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
  
  ;;; crafts pickaxes with blades made of wood, stone or iron
  ;;; pickaxes are needed to mine stone and ore, so that is why this action has
     ;;; a more specific subtype
  (:action craft-pickaxe
    :parameters (has-ingredient ?sticks ?planks - wood 
                 has-ingredient ?stone - stone 
                 has-ingredient ?ingot - metal
                 has-pickaxe ?px - pickaxe) 
    :precondition (and (has-ingredient ?sticks)
                       (exists (or (?planks - wood)
                                   (?stone - stone)
                                   (?ingot - metal))))
    :effect (forall (?sticks - ingredient)
                    (and (when (has-ingredient ?planks)
                               (has-pickaxe ?wood-pickaxe))
                         (when (has-ingredient ?stone)
                               (has-pickaxe ?stone-pickaxe))
                         (when (has-ingredient ?ingot)
                               (has-pickaxe ?iron-pickaxe))))
    )  
  ); end defdomain
  


