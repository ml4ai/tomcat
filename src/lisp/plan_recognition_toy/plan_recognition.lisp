(prog (ql:quickload "shop3") 
      (ql:quickload "cl-json")
      (load "../util.lisp"))

(load-lisp-domain (domain-file)
                  (load domain-file))

(load-json-domain (domain-file))

(compute-posterior-prob (cats action state domain prior)
                        )

(compute-likelihood-prior (c action state domain prior)
                          )
