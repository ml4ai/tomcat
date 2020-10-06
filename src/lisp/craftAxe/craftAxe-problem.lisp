#|
 craftIronAxe-problem.lisp
 Salena Torres Ashton
 Created 28 September 2020
 INFO 699 and ToMCAT Planning

These are three problems. 3 different init and same goal: craft iron axe
Delete block comments to run each problem (or put in separate file)

|#





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (problem craft-iron-axe)
  (:domain CraftIronAxe-domain.lisp)
  (:objects
    sticks planks wood ore ingot- object
    wooden-axe stone-axe iron-axe wood-pickaxe stone-pickaxe furnace- tool
    ;; currently the only fuel in the domain is 'fuel' but if I further
    ;;    develop this, it can burn wooden objects of the same name as
    ;;    wooden blades and tools, but not typed as tools, etc.
    wooden wood planks sticks kelp coal fuel- fuel
    wooden stone - blade
    )


  (:init
    () ;; has neither objects nor tools. Start from scratch.
    )

  (:goal (and
    (has-tool iron-axe)
    ))
) ;;end problem definition



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|

(define (problem craft-iron-axe)
  (:domain CraftIronAxe-domain.lisp)
  (:objects
    sticks planks wood ore ingot- object
    wooden-axe stone-axe iron-axe wood-pickaxe stone-pickaxe furnace- tool
    ;; currently the only fuel in the domain is 'fuel' but if I further
    ;;    develop this, it can burn wooden objects of the same name as
    ;;    wooden blades and tools, but not typed as tools, etc.
    wooden wood planks sticks kelp coal fuel- fuel
    wooden stone - blade
    )


  (:init (and
    (has-tool wooden-axe)
    ))

  (:goal (and
    (has-tool iron-axe)
    ))
) ;;end problem definition



|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|

(define (problem craft-iron-axe)
  (:domain CraftIronAxe-domain.lisp)
  (:objects
    sticks planks wood ore ingot- object
    wooden-axe stone-axe iron-axe wood-pickaxe stone-pickaxe furnace- tool
    ;; currently the only fuel in the domain is 'fuel' but if I further
    ;;    develop this, it can burn wooden objects of the same name as
    ;;    wooden blades and tools, but not typed as tools, etc.
    wooden wood planks sticks kelp coal fuel- fuel
    wooden stone - blade
    )


  (:init (and
    (has-tool stone-pickaxe) 
    (not (has-tool stone-pickaxe)
    ))
) ;;end problem definition

