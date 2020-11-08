(progn (ql:quickload "shop3")
       (load "util.lisp"))

(in-package :shop-user)
(shop-trace :tasks :states :plans)
(defdomain (simple-sar-domain :type pddl-domain :redefine-ok T) (
    (:types
      human
      victim rescuer - human
      room
    )

    (:predicates
      (in ?h - human ?r - room)
      (yellow ?v - victim)
      (green ?v - victim)
      (triaged ?v - victim)
      (searched ?r - room)
      (located ?v - victim ?r - room)
    )

    (:action enter-room 
      :parameters (?t - rescuer ?r - room)
      :precondition (not (in ?t ?r))
      :effect (in ?t ?r)
    )

    (:action search-room
      :parameters (?t - rescuer ?r - room)
      :precondition (and (in ?t ?r) (not (searched ?r)))
      :effect (and (searched ?r) (forall (?v - victim) (when (in ?v ?r) (located ?v ?r))))
    )

    (:action triage
     :parameters (?t - rescuer ?v - victim)
     :precondition (not (triaged ?v))
     :effect (triaged ?v)
    )

    (:action exit-room
      :parameters (?t - rescuer ?r - room)
      :precondition (in ?t ?r)
      :effect (not (in ?t ?r))
    )

    (:method (yellow-first-strategy ?t)
      enter-room
      (and (room ?r) (not (in ?t ?_)) (not (searched ?r)))
      (:ordered (:task !enter-room ?t ?r)
                (:task yellow-first-strategy ?t))

      search-room
      (and (in ?t ?r) (not (searched ?r)))
      (:ordered (:task !search-room ?t ?r)
                (:task yellow-first-strategy ?t))

      triage-yellow
      (and (in ?t ?r) (located ?v ?r) (yellow ?v) (not (triaged ?v)))
      (:ordered (:task !triage ?t ?v)
                (:task yellow-first-strategy ?t))

      exit-room
      (and (in ?t ?r))
      (:ordered (:task !exit-room ?t ?r)
                (:task yellow-first-strategy ?t))

      mission-done
      ()
      ()
    )

    (:method (opportunistic ?t)
      enter-room
      (and (room ?r) (not (in ?t ?_)) (not (searched ?r)))
      (:ordered (:task !enter-room ?t ?r)
                (:task opportunistic ?t))

      search-room
      (and (in ?t ?r) (not (searched ?r)))
      (:ordered (:task !search-room ?t ?r)
                (:task opportunistic ?t))

      triage-yellow
      (and (in ?t ?r) (located ?v ?r) (victim ?v) (not (triaged ?v)))
      (:ordered (:task !triage ?t ?v)
                (:task opportunistic ?t))

      exit-room
      (and (in ?t ?r))
      (:ordered (:task !exit-room ?t ?r)
                (:task opportunistic ?t))

      mission-done
      ()
      ()
    )
  )
)

(defproblem simple-sar-problem
            ((room r1) (room r2) (rescuer t1) (victim v1) (victim v2)
                          (victim v3) (victim v4) (victim v5) (victim v6) (victim v7) (victim v8) (victim v9) (victim v10)
                          (in v1 r1) (in v2 r1) (in v3 r1) (in v4 r1) (in v5 r1) (in v6 r2) (in v7 r2) (in v8 r2) (in v9 r2) (in v10 r2)
                          (yellow v1) (green v2) (yellow v3) (green v4) (yellow v5) (yellow v6) (yellow v7) (green v8) (green v9) (green v10))
            ((yellow-first-strategy t1))) ;;comment out for opportunistic/remove comment for yellow-first
            ;;((opportunistic t1))) ;;comment out for yellow-first/remove comment for opportunistic

(find-plans 'simple-sar-problem :which :all :verbose :plans :plan-tree t)
