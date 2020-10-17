;;;; craft with classical planning: wood, stone or iron axe
                                 ;  or any pickaxe

;;; To Do:   
         ; Syntax for SHOP3
           
(define (problem craft-wood-axe
  (:domain problem-craft-tool-weapon-domain.lisp)

  ;; no need for pickaxe or weapon parameters
  (:objects has-ingredient ?h - ingredients
            is-crafted ?c1 ?c2 - ingredients
            has-tool ?t - tool) 
  (:init  ()    )
  (:goal (has-tool wood-axe))
  ); end problem craft-wood-axe
;-------------------------------------------------------------------  
           
           
(define (problem craft-stone-axe ; from scratch
  (:domain problem-craft-tool-weapon-domain.lisp)

  (:objects has-ingredient ?wood ?stone ?sticks ?planks - ingredients
            is-crafted ?c1 ?c2 - ingredients
            has-tool ?t - tool
            has-pickaxe ?px - pickaxe) 
  (:init  ()    )
  (:goal (has-tool stone-axe))
  ); end problem craft-stone-axe

;-------------------------------------------------------------------  
           
(define (problem craft-iron-axe ;from scratch
  (:domain problem-craft-tool-weapon-domain.lisp)

  (:objects has-ingredient ?wood ?stone ?sticks ?planks ?ingot - ingredients
            is-crafted ?c1 ?c2 - ingredients
            has-tool ?t - tool
            has-pickaxe ?px - pickaxe) 
  (:init  ()    )
  (:goal (has-tool iron-axe))
  ); end problem craft-iron-axe

;-------------------------------------------------------------------  
(define (problem craft-pickaxe ;from scratch
  (:domain problem-craft-tool-weapon-domain.lisp)

  (:objects has-ingredient ?wood ?stone ?sticks ?planks ?ingot - ingredients
            is-crafted ?c1 ?c2 - ingredients
            has-tool ?t - tool
            has-pickaxe ?px - pickaxe
            )
  (:init  ()    )
  (:goal (has-pickaxe pickaxe))
  ); end problem craft-pickaxe

;--------------------------------------------------------------------  
