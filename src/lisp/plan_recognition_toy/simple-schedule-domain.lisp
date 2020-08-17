(ql:quickload "shop3")

(in-package :shop-user)
;;(shop-trace :all)
(defdomain (simple-schedule-domain :type pddl-domain :redefine-ok T) (
            (:types human)
            (:predicates (raining)
                         (work-today)
                         (store-today)
                         (homework-today)
                         (movie-today)
                         (went-to-school)
                         (went-to-work)
                         (did-chores)
                         (did-homework)
                         (stayed-for-tutoring)
                         (ran)
                         (played-videogames)
                         (went-to-store)
                         (watched-movie)
                         (early-morning)
                         (late-morning)
                         (afternoon)
                         (evening)
            )

            (:action go-to-school
                     :parameters (?t - human)
                     :precondition ()
                     :effect (went-to-school)
            )

            (:action go-to-work
                     :parameters (?t - human)
                     :precondition ()
                     :effect (went-to-work)
            )

            (:action do-chores
                     :parameters (?t - human)
                     :precondition ()
                     :effect (did-chores)
            )

            (:action do-homework
                     :parameters (?t - human)
                     :precondition ()
                     :effect (did-homework)
            )

            (:action stay-for-tutoring
                     :parameters (?t - human)
                     :precondition ()
                     :effect (stayed-for-tutoring)
            )

            (:action go-running
                     :parameters (?t - human)
                     :precondition ()
                     :effect (ran)
            )

            (:action play-videogames
                     :parameters (?t - human)
                     :precondition ()
                     :effect (played-videogames)
            )

            (:action go-to-store
                     :parameters (?t - human)
                     :precondition ()
                     :effect (went-to-store)
            )

            (:action watch-movie
                     :parameters (?t - human)
                     :precondition ()
                     :effect (watched-movie)
            )

            (:operator (!!assert ?fact)
               (())
               (())
               (?fact)
               0
            )  

            (:operator (!!delete ?fact)
               (())
               (?fact)
               (())
               0
            )

            (:method (monday ?t)
                     work-rain-homework-early
                     (and (early-morning) (work-today) (raining) (homework-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     work-rain-homework-late
                     (and (late-morning) (work-today) (raining) (homework-today))
                     (:ordered (:task !go-to-work ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     work-rain-homework-afternoon
                     (and (afternoon) (work-today) (raining) (homework-today))
                     (:ordered (:task !do-chores ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     work-rain-homework-evening
                     (and (evening) (work-today) (raining) (homework-today))
                     (:ordered (:task !do-homework ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     rain-homework-early
                     (and (early-morning) (raining) (homework-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     rain-homework-late
                     (and (late-morning) (raining) (homework-today))
                     (:ordered (:task !stay-for-tutoring ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     rain-homework-afternoon
                     (and (afternoon) (raining) (homework-today))
                     (:ordered (:task !do-chores ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     rain-homework-evening
                     (and (evening) (raining) (homework-today))
                     (:ordered (:task !do-homework ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     work-homework-early
                     (and (early-morning) (work-today) (homework-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     work-homework-late
                     (and (late-morning) (work-today) (have-homework))
                     (:ordered (:task !go-to-work ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     work-homework-afternoon
                     (and (afternoon) (work-today) (have-homework))
                     (:ordered (:task !go-running ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     work-homework-evening
                     (and (evening) (work-today) (have-homework))
                     (:ordered (:task !do-homework ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     homework-early
                     (and (early-morning) (homework-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     homework-late
                     (and (late-morning) (homework-today))
                     (:ordered (:task !stay-for-tutoring ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     homework-afternoon
                     (and (afternoon) (homework-today))
                     (:ordered (:task !go-running ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     homework-evening
                     (and (evening) (homework-today))
                     (:ordered (:task !do-homework ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     work-rain-early
                     (and (early-morning) (work-today) (raining))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     work-rain-late
                     (and (late-morning) (work-today) (raining))
                     (:ordered (:task !go-to-work ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     work-rain-afternoon
                     (and (afternoon) (work-today) (raining))
                     (:ordered (:task !do-chores ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     work-rain-evening
                     (and (evening) (work-today) (raining))
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     rain-early
                     (and (early-morning) (raining))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     rain-late
                     (and (late-morning) (raining))
                     (:ordered (:task !stay-for-tutoring ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     rain-afternoon
                     (and (afternoon) (raining))
                     (:ordered (:task !do-chores ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     rain-evening
                     (and (evening) (raining))
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     work-early
                     (and (early-morning) (work-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     work-late
                     (and (late-morning) (work-today))
                     (:ordered (:task !stay-for-tutoring ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     work-afternoon
                     (and (afternoon) (work-today))
                     (:ordered (:task !go-running ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     work-evening
                     (and (evening) (work-today))
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     no-work-early
                     (early-morning)
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     no-work-late
                     (late-morning)
                     (:ordered (:task !stay-for-tutoring ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     no-work-afternoon
                     (afternoon)
                     (:ordered (:task !go-running ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     no-work
                     (evening)
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

            )

            (:method (tuesday ?t)
                     work-rain-homework-store-early
                     (and (early-morning) (store-today) (work-today) (raining) (homework-today))
                     (:ordered (:task !go-to-work ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     work-rain-homework-store-late
                     (and (late-morning) (store-today) (work-today) (raining) (homework-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     work-rain-homework-store-afternoon
                     (and (afternoon) (store-today) (work-today) (raining) (homework-today))
                     (:ordered (:task !go-to-store ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     work-rain-homework-store-evening
                     (and (evening) (store-today) (work-today) (raining) (homework-today))
                     (:ordered (:task !do-homework ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     work-rain-homework-early
                     (and (early-morning) (work-today) (raining) (homework-today))
                     (:ordered (:task !go-to-work ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     work-rain-homework-late
                     (and (late-morning) (work-today) (raining) (homework-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     work-rain-homework-afternoon
                     (and (afternoon) (work-today) (raining) (homework-today))
                     (:ordered (:task !do-homework ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     work-rain-homework-evening
                     (and (evening) (work-today) (raining) (homework-today))
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     rain-homework-store-early
                     (and (early-morning) (store-today) (raining) (homework-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     rain-homework-store-late
                     (and (late-morning) (store-today) (raining) (homework-today))
                     (:ordered (:task !stay-for-tutoring ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     rain-homework-store-afternoon
                     (and (afternoon) (store-today) (raining) (homework-today))
                     (:ordered (:task !go-to-store ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     rain-homework-store-evening
                     (and (evening) (store-today) (raining) (homework-today))
                     (:ordered (:task !do-homework ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     rain-homework-early
                     (and (early-morning) (raining) (homework-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     rain-homework-late
                     (and (late-morning) (raining) (homework-today))
                     (:ordered (:task !stay-for-tutoring ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     rain-homework-afternoon
                     (and (afternoon) (raining) (homework-today))
                     (:ordered (:task !do-homework ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     rain-homework-evening
                     (and (evening) (raining) (homework-today))
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     work-homework-store-early
                     (and (early-morning) (store-today) (work-today) (homework-today))
                     (:ordered (:task !go-to-work ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     work-homework-store-late
                     (and (late-morning) (store-today) (work-today) (homework-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     work-homework-store-afternoon
                     (and (afternoon) (store-today) (work-today) (homework-today))
                     (:ordered (:task !go-to-store ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     work-homework-store-evening
                     (and (evening) (store-today) (work-today) (homework-today))
                     (:ordered (:task !do-homework ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     work-homework-early
                     (and (early-morning) (work-today) (homework-today))
                     (:ordered (:task !go-to-work ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     work-homework-late
                     (and (late-morning) (work-today) (homework-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     work-homework-afternoon
                     (and (afternoon) (work-today) (homework-today))
                     (:ordered (:task !do-homework ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     work-homework-evening
                     (and (evening) (work-today) (homework-today))
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     homework-store-early
                     (and (early-morning) (store-today) (homework-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     homework-store-late
                     (and (late-morning) (store-today) (homework-today))
                     (:ordered (:task !stay-for-tutoring ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     homework-store-afternoon
                     (and (afternoon) (store-today) (homework-today))
                     (:ordered (:task !go-to-store ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     homework-store-evening
                     (and (evening) (store-today) (homework-today))
                     (:ordered (:task !do-homework ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     homework-early
                     (and (early-morning) (homework-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     homework-late
                     (and (late-morning) (homework-today))
                     (:ordered (:task !stay-for-tutoring ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     homework-afternoon
                     (and (afternoon) (homework-today))
                     (:ordered (:task !do-homework ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     homework-evening
                     (and (evening) (homework-today))
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     work-rain-store-early
                     (and (early-morning) (store-today) (work-today) (raining))
                     (:ordered (:task !go-to-work ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     work-rain-store-late
                     (and (late-morning) (store-today) (work-today) (raining))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     work-rain-store-afternoon
                     (and (afternoon) (store-today) (work-today) (raining))
                     (:ordered (:task !go-to-store ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     work-rain-store-evening
                     (and (evening) (store-today) (work-today) (raining))
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     work-rain-early
                     (and (early-morning) (work-today) (raining))
                     (:ordered (:task !go-to-work ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     work-rain-late
                     (and (late-morning) (work-today) (raining))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     work-rain-afternoon
                     (and (afternoon) (work-today) (raining))
                     (:ordered (:task !do-chores ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     work-rain-evening
                     (and (evening) (work-today) (raining))
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     rain-store-early
                     (and (early-morning) (store-today) (raining))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     rain-store-late
                     (and (late-morning) (store-today) (raining))
                     (:ordered (:task !stay-for-tutoring ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     rain-store-afternoon
                     (and (afternoon) (store-today) (raining))
                     (:ordered (:task !go-to-store ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     rain-store-evening
                     (and (evening) (store-today) (raining))
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     rain-early
                     (and (early-morning) (raining))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     rain-late
                     (and (late-morning) (raining))
                     (:ordered (:task !stay-for-tutoring ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     rain-afternoon
                     (and (afternoon) (raining))
                     (:ordered (:task !do-chores ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     rain-evening
                     (and (evening) (raining))
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     work-store-early
                     (and (early-morning) (store-today) (work-today))
                     (:ordered (:task !go-to-work ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     work-store-late
                     (and (late-morning) (store-today) (work-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     work-store-afternoon
                     (and (afternoon) (store-today) (work-today))
                     (:ordered (:task !go-to-store ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     work-store-evening
                     (and (evening) (store-today) (work-today))
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     work-early
                     (and (early-morning) (work-today))
                     (:ordered (:task !go-to-work ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     work-late
                     (and (late-morning) (work-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     work-afternoon
                     (and (afternoon) (work-today))
                     (:ordered (:task !go-running ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     work-evening
                     (and (evening) (work-today))
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     no-work-store-early
                     (and (early-morning) (store-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     no-work-store-late
                     (and (late-morning) (store-today))
                     (:ordered (:task !stay-for-tutoring ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     no-work-store-afternoon
                     (and (afternoon) (store-today))
                     (:ordered (:task !go-to-store ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     no-work-store-evening
                     (and (evening) (store-today))
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     no-work-early
                     (early-morning)
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     no-work-late
                     (late-morning)
                     (:ordered (:task !stay-for-tutoring ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     no-work-afternoon
                     (afternoon)
                     (:ordered (:task !go-running ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     no-work-evening
                     (evening)
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

            )
 
            (:method (wednesday ?t)
                     work-rain-homework-early
                     (and (early-morning) (work-today) (raining) (homework-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     work-rain-homework-late
                     (and (late-morning) (work-today) (raining) (homework-today))
                     (:ordered (:task !do-homework ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     work-rain-homework-afternoon
                     (and (afternoon) (work-today) (raining) (homework-today))
                     (:ordered (:task !go-to-work ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     work-rain-homework-evening
                     (and (evening) (work-today) (raining) (homework-today))
                     (:ordered (:task !do-chores ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     rain-homework-early
                     (and (early-morning) (raining) (homework-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     rain-homework-late
                     (and (late-morning) (raining) (homework-today))
                     (:ordered (:task !do-homework ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     rain-homework-afternoon
                     (and (afternoon) (raining) (homework-today))
                     (:ordered (:task !do-chores ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     rain-homework-evening
                     (and (evening) (raining) (homework-today))
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     work-homework-early
                     (and (early-morning) (work-today) (homework-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     work-homework-late
                     (and (late-morning) (work-today) (have-homework))
                     (:ordered (:task !do-homework ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     work-homework-afternoon
                     (and (afternoon) (work-today) (have-homework))
                     (:ordered (:task !go-to-work ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     work-homework-evening
                     (and (evening) (work-today) (have-homework))
                     (:ordered (:task !do-chores ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     homework-early
                     (and (early-morning) (homework-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     homework-late
                     (and (late-morning) (homework-today))
                     (:ordered (:task !do-homework ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     homework-afternoon
                     (and (afternoon) (homework-today))
                     (:ordered (:task !go-running ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     homework-evening
                     (and (evening) (homework-today))
                     (:ordered (:task !do-chores ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     work-rain-early
                     (and (early-morning) (work-today) (raining))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     work-rain-late
                     (and (late-morning) (work-today) (raining))
                     (:ordered (:task !stay-for-tutoring ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     work-rain-afternoon
                     (and (afternoon) (work-today) (raining))
                     (:ordered (:task !go-to-work ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     work-rain-evening
                     (and (evening) (work-today) (raining))
                     (:ordered (:task !do-chores ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     rain-early
                     (and (early-morning) (raining))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     rain-late
                     (and (late-morning) (raining))
                     (:ordered (:task !stay-for-tutoring ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     rain-afternoon
                     (and (afternoon) (raining))
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     rain-evening
                     (and (evening) (raining))
                     (:ordered (:task !do-chores ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     work-early
                     (and (early-morning) (work-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     work-late
                     (and (late-morning) (work-today))
                     (:ordered (:task !go-to-work ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     work-afternoon
                     (and (afternoon) (work-today))
                     (:ordered (:task !go-running ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     work-evening
                     (and (evening) (work-today))
                     (:ordered (:task !do-chores ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     no-work-early
                     (early-morning)
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     no-work-late
                     (late-morning)
                     (:ordered (:task !stay-for-tutoring ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     no-work-afternoon
                     (afternoon)
                     (:ordered (:task !go-running ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     no-work
                     (evening)
                     (:ordered (:task !do-chores ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))
 
            )

            (:method (thursday ?t)
                     work-rain-homework-early
                     (and (early-morning) (work-today) (raining) (homework-today))
                     (:ordered (:task !go-to-work ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     work-rain-homework-late
                     (and (late-morning) (work-today) (raining) (homework-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     work-rain-homework-afternoon
                     (and (afternoon) (work-today) (raining) (homework-today))
                     (:ordered (:task !do-homework ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     work-rain-homework-evening
                     (and (evening) (work-today) (raining) (homework-today))
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     rain-homework-movie-early
                     (and (early-morning) (movie-today) (raining) (homework-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     rain-homework-movie-late
                     (and (late-morning) (movie-today) (raining) (homework-today))
                     (:ordered (:task !do-homework ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     rain-homework-movie-afternoon
                     (and (afternoon) (movie-today) (raining) (homework-today))
                     (:ordered (:task !do-chores ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     rain-homework-movie-evening
                     (and (evening) (movie-today) (raining) (homework-today))
                     (:ordered (:task !watch-movie ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     rain-homework-early
                     (and (early-morning) (raining) (homework-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     rain-homework-late
                     (and (late-morning) (raining) (homework-today))
                     (:ordered (:task !do-homework ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     rain-homework-afternoon
                     (and (afternoon) (raining) (homework-today))
                     (:ordered (:task !do-chores ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     rain-homework-evening
                     (and (evening) (raining) (homework-today))
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     work-homework-early
                     (and (early-morning) (work-today) (homework-today))
                     (:ordered (:task !go-to-work ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     work-homework-late
                     (and (late-morning) (work-today) (homework-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     work-homework-afternoon
                     (and (afternoon) (work-today) (homework-today))
                     (:ordered (:task !do-homework ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     work-homework-evening
                     (and (evening) (work-today) (homework-today))
                     (:ordered (:task !go-running ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     homework-movie-early
                     (and (early-morning) (movie-today) (homework-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     homework-movie-late
                     (and (late-morning) (movie-today) (homework-today))
                     (:ordered (:task !do-homework ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     homework-movie-afternoon
                     (and (afternoon) (movie-today) (homework-today))
                     (:ordered (:task !go-running ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     homework-movie-evening
                     (and (evening) (movie-today) (homework-today))
                     (:ordered (:task !watch-movie ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     homework-early
                     (and (early-morning) (homework-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     homework-late
                     (and (late-morning) (homework-today))
                     (:ordered (:task !do-homework ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     homework-afternoon
                     (and (afternoon) (homework-today))
                     (:ordered (:task !go-running ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     homework-evening
                     (and (evening) (homework-today))
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     work-rain-early
                     (and (early-morning) (work-today) (raining))
                     (:ordered (:task !go-to-work ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     work-rain-late
                     (and (late-morning) (work-today) (raining))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     work-rain-afternoon
                     (and (afternoon) (work-today) (raining))
                     (:ordered (:task !stay-for-tutoring ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     work-rain-evening
                     (and (evening) (work-today) (raining))
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     rain-movie-early
                     (and (early-morning) (movie-today) (raining))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     rain-movie-late
                     (and (late-morning) (movie-today) (raining))
                     (:ordered (:task !stay-for-tutoring ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     rain-movie-afternoon
                     (and (afternoon) (movie-today) (raining))
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     rain-movie-evening
                     (and (evening) (movie-today) (raining))
                     (:ordered (:task !watch-movie ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     rain-early
                     (and (early-morning) (raining))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     rain-late
                     (and (late-morning) (raining))
                     (:ordered (:task !stay-for-tutoring ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     rain-afternoon
                     (and (afternoon) (raining))
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     rain-evening
                     (and (evening) (raining))
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     work-early
                     (and (early-morning) (work-today))
                     (:ordered (:task !go-to-work ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     work-late
                     (and (late-morning) (work-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     work-afternoon
                     (and (afternoon) (work-today))
                     (:ordered (:task !go-running ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     work-evening
                     (and (evening) (work-today))
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     no-work-movie-early
                     (and (early-morning) (movie-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     no-work-movie-late
                     (and (late-morning) (movie-today))
                     (:ordered (:task !stay-for-tutoring ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     no-work-movie-afternoon
                     (and (afternoon) (movie-today))
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     no-work-movie-evening
                     (and (evening) (movie-today))
                     (:ordered (:task !watch-movie ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     no-work-early
                     (early-morning)
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     no-work-late
                     (late-morning)
                     (:ordered (:task !stay-for-tutoring ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     no-work-afternoon
                     (afternoon)
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     no-work-evening
                     (evening)
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))


            )

            (:method (friday ?t)
                     work-rain-early
                     (and (early-morning) (work-today) (raining))
                     (:ordered (:task !do-chores ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     work-rain-late
                     (and (late-morning) (work-today) (raining))
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     work-rain-afternoon
                     (and (afternoon) (work-today) (raining))
                     (:ordered (:task !go-to-work ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     work-rain-evening
                     (and (evening) (work-today) (raining))
                     (:ordered (:task !go-to-work ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     rain-early
                     (and (early-morning) (raining))
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     rain-late
                     (and (late-morning) (raining))
                     (:ordered (:task !stay-for-tutoring ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     rain-afternoon
                     (and (afternoon) (raining))
                     (:ordered (:task !do-chores ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     rain-evening
                     (and (evening) (raining))
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     work-early
                     (and (early-morning) (work-today))
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     work-late
                     (and (late-morning) (work-today))
                     (:ordered (:task !go-running ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     work-afternoon
                     (and (afternoon) (work-today))
                     (:ordered (:task !go-to-work ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     work-evening
                     (and (evening) (work-today))
                     (:ordered (:task !go-to-work ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))

                     no-work-early
                     (early-morning)
                     (:ordered (:task !go-to-school ?t)
                               (:task !!delete (early-morning))
                               (:task !!assert (late-morning)))

                     no-work-late
                     (late-morning)
                     (:ordered (:task !go-running ?t)
                               (:task !!delete (late-morning))
                               (:task !!assert (afternoon)))

                     no-work-afternoon
                     (afternoon)
                     (:ordered (:task !do-chores ?t)
                               (:task !!delete (afternoon))
                               (:task !!assert (evening)))

                     no-work
                     (evening)
                     (:ordered (:task !play-videogames ?t)
                               (:task !!delete (evening))
                               (:task !!assert (early-morning)))
 
            )
 
  )
)
