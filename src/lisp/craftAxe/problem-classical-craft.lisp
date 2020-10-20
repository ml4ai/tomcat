;;;; craft with classical planning: wood, stone or iron axe
                                 ;  or any pickaxe

;;; To Do:   
         ; Syntax for SHOP3
           
(define (problem craft-wood-axe
  (:domain domain-classical-craft.lisp)

  ;; no need for pickaxe or weapon parameters
  (:objects st p - wood
            wa - tool)
  (:init  ()    )
  (:goal (has-wood-axe wa))
  );end problem craft-wood-axe
  
  ;;comment out if you want to use problem below
  ); end problem craft-wood-axe

;-------------------------------------------------------------------  

#|
(define (problem craft-stone-pickaxe
  (:domain domain-classical-craft.lisp)

  (:objects sticks planks - wood
            stone - ingredients
            spx - tool)

  (:init ()  )

  (:goal (has-stone-pickaxe spx))
  ); end problem craft-stone-pickaxe

  ;;comment out if you want to use problem above
  ); end problem craft-wood-axe
|#
