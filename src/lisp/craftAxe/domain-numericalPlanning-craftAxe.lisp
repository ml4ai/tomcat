
#| Numerical Planning for same domain and problem of craftAxe
   This file does not run in SHOP3. To run a file through SHOP3, please
       look at craft-stone-tools.lisp and craft-wood-tools.lisp files.
   I would like to keep this file for the numerical planning aspect of 
       the domain so I can later refactor, if needed.

   Created: 16 October 2020  

   Purpose: To craft an axe made from wood, stone, or iron. This domain 
            uses numerical planning to increase and decrease ingredients
            and inventory for crafting items. 

            Once an axe is crafted, it will not deplete.

            Numerical costs of crafting an axe reflects current 
            Minecraft recipes.
 
  Updates and Temporary Notes:
  16 Oct: domain crafts only wooden axe and wooden pickaxe.
          needs shop3 syntax!
  19 Oct: added wooden sword
|#

 


(progn (ql:quickload "shop3"))

(in-package :shop-user)

(defdomain (craftAxe-2 :type pddl-domain :redefine-okay T :action-costs) ()
  (:types (ingredients tool weapon- object)
          (sticks planks wood - ingredients)
          (wood-axe wood-pickaxe - tool)
          (wood-sword - weapon)
    )

  (:predicates (wood ?w planks ?p sticks ?s - ingredients)
               (wood-axe ?wa wood-pickaxe ?wp - tool)
               (wood-sword ?ws - weapon)
    );end predicates

  (:functions (inventory-wood ?iw - ingredients)
              (inventory-planks ?ip - ingredients)
              (inventory-sticks ?is - ingredients)
; Do I need this after all?              (total-cost)
              ); end functions



  ;;; mine wood. Very simple for now.
  (:action mine-wood
    :parameters (wood ?w - ingredient)
    :precondition ()   ; none. There is conditional effect below.
    :effect (increase (inventory-wood ?iw) ?1)
            ); end action mine-wood                               


  ;;; crafting planks from wood
  (:action craft-planks
    :parameters ((wood ?w planks ?p - ingredient)
                 (inventory-wood ?w inventory-planks ?ip - ingredients))
    :precondition (>= (inventory-wood ?iw) ?1)
    :effect (and (decrease (inventory-wood ?iw) ?1)
                 (increase (inventory-planks ?iw) ?4)
                 ); end action craft-planks


  ;;; crafting sticks from planks
  (:action craft-sticks
    :parameters (inventory-planks ?ip inventory-sticks ?is - ingredients)
    :precondition (>= (inventory-planks ?ip) ?2)
    :effect (and (increase (inventory-sticks ?is) ?4)
                 (decrease (inventory-planks ?ip) ?2))
    ); end action craft-sticks

  
  ;;; crafting a wooden axe
  (:action craft-axe
    :parameters (wood-axe ?wa - tool
                inventory-planks ?ip inventory-sticks ?is - ingredients)
    :precondition (and (>= (inventory-planks ?ip) ?3)
                       (>= (inventory-sticks ?is) ?2))
    :effect (and (decrease (inventory-planks ?ip) ?3)
                 (decrease (inventory-sticks ?is) ?2)
                 (= (wood-axe ?wa) ?1)
            ); end action craft-wood-axe


  ;;; crafting a wooden pickaxe
  (:action craft-pickaxe
    :parameters (wood-pickaxe ?wp - tool
                inventory-planks ?ip inventory-sticks ?is - ingredients)
    :precondition (and (>= (inventory-planks ?ip) ?3)
                       (>= (inventory-sticks ?is) ?2))
    :effect (and (decrease (inventory-planks ?ip) ?3)
                 (decrease (inventory-sticks ?is) ?2)
                 (= (wood-pickaxe ?wp) ?1))
    ); end action craft-wood-pickaxe


  ;; crafting a wooden sword
  (:action craft-sword
    :parameters (wood-sword ?ws - weapon
                inventory-planks ?ip inventory-sticks ?is - ingredients)
    :precondition (and (>= (inventory-planks ?ip) ?2)
                       (>= (inventory-sticks ?is) ?1))
    :effect (and (decrease (inventory-planks ?ip) ?2)
                 (decrease (inventory-sticks ?is) ?1)
                 (= (wood-sword ?ws) ?1))
    ); end action craft-wood-sword

);end defdomain
                       
    
