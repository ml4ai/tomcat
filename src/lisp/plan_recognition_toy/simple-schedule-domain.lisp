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
                     (:task !go-to-school ?t)

                     work-rain-homework-late
                     (and (late-morning) (work-today) (raining) (homework-today))
                     (:task !go-to-work ?t)

                     work-rain-homework-afternoon
                     (and (afternoon) (work-today) (raining) (homework-today))
                     (:task !do-chores ?t)

                     work-rain-homework-evening
                     (and (evening) (work-today) (raining) (homework-today))
                     (:task !do-homework ?t)

                     rain-homework-early
                     (and (early-morning) (raining) (homework-today))
                     (:task !go-to-school ?t)

                     rain-homework-late
                     (and (late-morning) (raining) (homework-today))
                     (:task !stay-for-tutoring ?t)

                     rain-homework-afternoon
                     (and (afternoon) (raining) (homework-today))
                     (:task !do-chores ?t)

                     rain-homework-evening
                     (and (evening) (raining) (homework-today))
                     (:task !do-homework ?t)

                     work-homework-early
                     (and (early-morning) (work-today) (homework-today))
                     (:task !go-to-school ?t)

                     work-homework-late
                     (and (late-morning) (work-today) (have-homework))
                     (:task !go-to-work ?t)

                     work-homework-afternoon
                     (and (afternoon) (work-today) (have-homework))
                     (:task !go-running ?t)

                     work-homework-evening
                     (and (evening) (work-today) (have-homework))
                     (:task !do-homework ?t)

                     homework-early
                     (and (early-morning) (homework-today))
                     (:task !go-to-school ?t)

                     homework-late
                     (and (late-morning) (homework-today))
                     (:task !stay-for-tutoring ?t)

                     homework-afternoon
                     (and (afternoon) (homework-today))
                     (:task !go-running ?t)

                     homework-evening
                     (and (evening) (homework-today))
                     (:task !do-homework ?t)

                     work-rain
                     (and (work-today) (raining))
                     (:ordered (:task !go-to-school ?t)
                               (:task !go-to-work ?t)
                               (:task !do-chores ?t)
                               (:task !play-videogames ?t))

                     rain
                     (raining)
                     (:ordered (:task !go-to-school ?t)
                               (:task !stay-for-tutoring ?t)
                               (:task !do-chores ?t)
                               (:task !play-videogames ?t))

                     work
                     (work-today)
                     (:ordered (:task !go-to-school ?t)
                               (:task !stay-for-tutoring ?t)
                               (:task !go-running ?t)
                               (:task !play-videogames ?t))

                     no-work
                     ()
                     (:ordered (:task !go-to-school ?t)
                               (:task !stay-for-tutoring ?t)
                               (:task !go-running ?t)
                               (:task !play-videogames ?t))

            )

            (:method (tuesday ?t)
                     work-rain-homework-store
                     (and (store-today) (work-today) (raining) (homework-today))
                     (:ordered (:task !go-to-work ?t)
                               (:task !go-to-school ?t)
                               (:task !go-to-store ?t)
                               (:task !do-homework ?t))

                     work-rain-homework
                     (and (work-today) (raining) (homework-today))
                     (:ordered (:task !go-to-work ?t)
                               (:task !go-to-school ?t)
                               (:task !do-homework ?t)
                               (:task !play-videogames ?t))

                     rain-homework-store
                     (and (store-today) (raining) (homework-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !stay-for-tutoring ?t)
                               (:task !go-to-store ?t)
                               (:task !do-homework ?t))

                     rain-homework
                     (and (raining) (homework-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !stay-for-tutoring ?t)
                               (:task !do-homework ?t)
                               (:task !play-videogames ?t))

                     work-homework-store
                     (and (store-today) (work-today) (homework-today))
                     (:ordered (:task !go-to-work ?t)
                               (:task !go-to-school ?t)
                               (:task !go-to-store ?t)
                               (:task !do-homework ?t))

                     work-homework
                     (and (work-today) (homework-today))
                     (:ordered (:task !go-to-work ?t)
                               (:task !go-to-school ?t)
                               (:task !do-homework ?t)
                               (:task !play-videogames ?t))

                     homework-store
                     (and (store-today) (homework-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !stay-for-tutoring ?t)
                               (:task !go-to-store ?t)
                               (:task !do-homework ?t))

                     homework
                     (homework-today)
                     (:ordered (:task !go-to-school ?t)
                               (:task !stay-for-tutoring ?t)
                               (:task !do-homework ?t)
                               (:task !play-videogames ?t))

                     work-rain-store
                     (and (store-today) (work-today) (raining))
                     (:ordered (:task !go-to-work ?t)
                               (:task !go-to-school ?t)
                               (:task !go-to-store ?t)
                               (:task !play-videogames ?t))

                     work-rain
                     (and (work-today) (raining))
                     (:ordered (:task !go-to-work ?t)
                               (:task !go-to-school ?t)
                               (:task !do-chores ?t)
                               (:task !play-videogames ?t))

                     rain-store
                     (and (store-today) (raining))
                     (:ordered (:task !go-to-school ?t)
                               (:task !stay-for-tutoring ?t)
                               (:task !go-to-store ?t)
                               (:task !play-videogames ?t))

                     rain
                     (raining)
                     (:ordered (:task !go-to-school ?t)
                               (:task !stay-for-tutoring ?t)
                               (:task !do-chores ?t)
                               (:task !play-videogames ?t))

                     work-store
                     (and (store-today) (work-today))
                     (:ordered (:task !go-to-work ?t)
                               (:task !go-to-school ?t)
                               (:task !go-to-store ?t)
                               (:task !play-videogames ?t))

                     work
                     (work-today)
                     (:ordered (:task !go-to-work ?t)
                               (:task !go-to-school ?t)
                               (:task !go-running ?t)
                               (:task !play-videogames ?t))

                     no-work-store
                     (store-today)
                     (:ordered (:task !go-to-school ?t)
                               (:task !stay-for-tutoring ?t)
                               (:task !go-to-store ?t)
                               (:task !play-videogames ?t))

                     no-work
                     ()
                     (:ordered (:task !go-to-school ?t)
                               (:task !stay-for-tutoring ?t)
                               (:task !go-running ?t)
                               (:task !play-videogames ?t))
            )
 
            (:method (wednesday ?t)
                     work-rain-homework
                     (and (work-today) (raining) (homework-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !do-homework ?t)
                               (:task !go-to-work ?t)
                               (:task !do-chores ?t))

                     rain-homework
                     (and (raining) (homework-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !do-homework ?t)
                               (:task !do-chores ?t)
                               (:task !play-videogames ?t))

                     work-homework
                     (and (work-today) (homework-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !do-homework ?t)
                               (:task !go-to-work ?t)
                               (:task !do-chores ?t))

                     homework
                     (homework-today)
                     (:ordered (:task !go-to-school ?t)
                               (:task !do-homework ?t)
                               (:task !go-running ?t)
                               (:task !do-chores ?t))

                     work-rain
                     (and (work-today) (raining))
                     (:ordered (:task !go-to-school ?t)
                               (:task !stay-for-tutoring ?t)
                               (:task !go-to-work ?t)
                               (:task !do-chores ?t))

                     rain
                     (raining)
                     (:ordered (:task !go-to-school ?t)
                               (:task !stay-for-tutoring ?t)
                               (:task !play-videogames ?t)
                               (:task !do-chores ?t))

                     work
                     (work-today)
                     (:ordered (:task !go-to-school ?t)
                               (:task !go-to-work ?t)
                               (:task !go-running ?t)
                               (:task !do-chores ?t))

                     no-work
                     ()
                     (:ordered (:task !go-to-school ?t)
                               (:task !stay-for-tutoring ?t)
                               (:task !go-running ?t)
                               (:task !do-chores ?t))
            )

            (:method (thursday ?t)
                     work-rain-homework
                     (and (work-today) (raining) (homework-today))
                     (:ordered (:task !go-to-work ?t)
                               (:task !go-to-school ?t)
                               (:task !do-homework ?t)
                               (:task !play-videogames ?t))

                     rain-homework-movie
                     (and (movie-today) (raining) (homework-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !do-homework ?t)
                               (:task !do-chores ?t)
                               (:task !watch-movie ?t))

                     rain-homework
                     (and (raining) (homework-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !do-homework ?t)
                               (:task !do-chores ?t)
                               (:task !play-videogames ?t))

                     work-homework
                     (and (work-today) (homework-today))
                     (:ordered (:task !go-to-work ?t)
                               (:task !go-to-school ?t)
                               (:task !do-homework ?t)
                               (:task !go-running ?t))

                     homework-movie
                     (and (movie-today) (homework-today))
                     (:ordered (:task !go-to-school ?t)
                               (:task !do-homework ?t)
                               (:task !go-running ?t)
                               (:task !watch-movie ?t))

                     homework
                     (homework-today)
                     (:ordered (:task !go-to-school ?t)
                               (:task !do-homework ?t)
                               (:task !go-running ?t)
                               (:task !play-videogames ?t))

                     work-rain
                     (and (work-today) (raining))
                     (:ordered (:task !go-to-work ?t)
                               (:task !go-to-school ?t)
                               (:task !stay-for-tutoring ?t)
                               (:task !play-videogames ?t))

                     rain-movie
                     (and (movie-today) (raining))
                     (:ordered (:task !go-to-school ?t)
                               (:task !stay-for-tutoring ?t)
                               (:task !play-videogames ?t)
                               (:task !watch-movie ?t))

                     rain
                     (raining)
                     (:ordered (:task !go-to-school ?t)
                               (:task !stay-for-tutoring ?t)
                               (:task !play-videogames ?t)
                               (:task !play-videogames ?t))

                     work
                     (work-today)
                     (:ordered (:task !go-to-work ?t)
                               (:task !go-to-school ?t)
                               (:task !go-running ?t)
                               (:task !play-videogames ?t))

                     no-work-movie
                     (movie-today)
                     (:ordered (:task !go-to-school ?t)
                               (:task !stay-for-tutoring ?t)
                               (:task !play-videogames ?t)
                               (:task !watch-movie ?t))

                     no-work
                     ()
                     (:ordered (:task !go-to-school ?t)
                               (:task !stay-for-tutoring ?t)
                               (:task !play-videogames ?t)
                               (:task !play-videogames ?t))
 
            )

            (:method (friday ?t)
                     rain-work
                     (and (work-today) (raining))
                     (:ordered (:task !do-chores ?t)
                               (:task !play-videogames ?t)
                               (:task !go-to-work ?t)
                               (:task !go-to-work ?t))

                     rain
                     (raining)
                     (:ordered (:task !go-to-school ?t)
                               (:task !stay-for-tutoring ?t)
                               (:task !do-chores ?t)
                               (:task !play-videogames ?t))

                     work
                     (work-today)
                     (:ordered (:task !play-videogames ?t)
                               (:task !go-running ?t)
                               (:task !go-to-work ?t)
                               (:task !go-to-work ?t))

                     no-work
                     ()
                     (:ordered (:task !go-to-school ?t)
                               (:task !go-running ?t)
                               (:task !do-chores ?t)
                               (:task !play-videogames ?t))
            )
 
  )
)
