#| Numerical planning for same domain and problem of craftAxe
   Created: 16 October 2020

   Purpose: to craft an axe or pickaxe made from wood, stone, or iron.

            Once a tool is crafted, it will not deplete.

            Numerical inventory costs of crafting a tool reflects
            current Minecraft recipe costs.
  
  Updates and temporary Notes:
  16 Oct: domain crafts only wooden axe and wooden pickaxe. Needs
          shop3 syntax!
  19 Oct: crafts wooden sword. Uncomment to change goal (below)
|#

(define (problem craft-tool)
  (:domain domain-numericalPlanning-craftAxe.lisp)
  (:objects inventory-wood 
            inventory-planks 
            inventory-sticks - ingredients
            wood-axe wood-pickaxe - tool)
  
  ;; start state and goal reflect wood axe for now
  (:init (= (inventory-wood iw) 0)
         (= (inventory-planks ip) 0)
         (= (inventory-sticks is) 0)
         (= (wood-axe wa) 0)
         (= (wood-pickaxe wp) 0))

;;;; uncomment to craft an axe and a pickaxe  
  (:goal (and (>= (wood-axe wa) 1)
              (>= (wood-pickaxe wp) 1)))

;;;; uncomment to craft a sword
  (:goal (>= (wood-sword ws) 1))
  ); end define problem craft-tool












