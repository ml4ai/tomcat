(ql:quickload "shop3")

(in-package :shop-user)
(shop-trace :tasks :states :plans)
(defdomain (simple-schedule-domain :type pddl-domain :redefine-ok T) (
            (:types 
              human
            )
            (:predicates 
              (raining)
              (work-today)
              (need-groceries)
              (have-homework)
              (found-movie)
              (went-to-school)
              (went-to-work)
              (did-chores)
              (did-homework)
              (stayed-for-tutoring)
              (ran)
              (played-videogames)
              (went-to-store)
              (watched-movie)
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
                     :effect (and (did-homework) (not (have-homework)))
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
                     :effect (and (went-to-store) (not (need-groceries)))
            )

            (:action watch-movie
                     :parameters (?t - human)
                     :precondition ()
                     :effect (and (watched-movie) (not (found-movies)))
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
                     work-rain-homework
                     (and (work-today) (raining) (have-homework))
                     (:ordered (:task !go-to-school ?t)
                               (:task !go-to-work ?t)
                               (:task !do-chores ?t)
                               (:task !do-homework ?t))

                     rain-homework
                     (and (raining) (have-homework))
                     (:ordered (:task !go-to-school ?t)
                               (:task !stay-for-tutoring ?t)
                               (:task !do-chores ?t)
                               (:task !do-homework ?t))

                     work-homework
                     (and (work-today) (have-homework))
                     (:ordered (:task !go-to-school ?t)
                               (:task !go-to-work ?t)
                               (:task !go-running ?t)
                               (:task !do-homework ?t))

                     homework
                     (have-homework)
                     (:ordered (:task !go-to-school ?t)
                               (:task !stay-for-tutoring ?t)
                               (:task !go-running ?t)
                               (:task !do-homework ?t))

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
                     (and (need-groceries) (work-today) (raining) (have-homework))
                     (:ordered (:task !go-to-work ?t)
                               (:task !go-to-school ?t)
                               (:task !go-to-store ?t)
                               (:task !do-homework ?t))

                     work-rain-homework
                     (and (work-today) (raining) (have-homework))
                     (:ordered (:task !go-to-work ?t)
                               (:task !go-to-school ?t)
                               (:task !do-homework ?t)
                               (:task !play-videogames ?t))

                     rain-homework-store
                     (and (need-groceries) (raining) (have-homework))
                     (:ordered (:task !go-to-school ?t)
                               (:task !stay-for-tutoring ?t)
                               (:task !go-to-store ?t)
                               (:task !do-homework ?t))

                     rain-homework
                     (and (raining) (have-homework))
                     (:ordered (:task !go-to-school ?t)
                               (:task !stay-for-tutoring ?t)
                               (:task !do-homework ?t)
                               (:task !play-videogames ?t))

                     work-homework-store
                     (and (need-groceries) (work-today) (have-homework))
                     (:ordered (:task !go-to-work ?t)
                               (:task !go-to-school ?t)
                               (:task !go-to-store ?t)
                               (:task !do-homework ?t))

                     work-homework
                     (and (work-today) (have-homework))
                     (:ordered (:task !go-to-work ?t)
                               (:task !go-to-school ?t)
                               (:task !do-homework ?t)
                               (:task !play-videogames ?t))

                     homework-store
                     (and (need-groceries) (have-homework))
                     (:ordered (:task !go-to-school ?t)
                               (:task !stay-for-tutoring ?t)
                               (:task !go-to-store ?t)
                               (:task !do-homework ?t))

                     homework
                     (have-homework)
                     (:ordered (:task !go-to-school ?t)
                               (:task !stay-for-tutoring ?t)
                               (:task !do-homework ?t)
                               (:task !play-videogames ?t))

                     work-rain-store
                     (and (need-groceries) (work-today) (raining))
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
                     (and (need-groceries) (raining))
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
                     (and (need-groceries) (work-today))
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
                     (need-groceries)
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
                     (and (work-today) (raining) (have-homework))
                     (:ordered (:task !go-to-school ?t)
                               (:task !do-homework ?t)
                               (:task !go-to-work ?t)
                               (:task !do-chores ?t))

                     rain-homework
                     (and (raining) (have-homework))
                     (:ordered (:task !go-to-school ?t)
                               (:task !do-homework ?t)
                               (:task !do-chores ?t)
                               (:task !play-videogames ?t))

                     work-homework
                     (and (work-today) (have-homework))
                     (:ordered (:task !go-to-school ?t)
                               (:task !do-homework ?t)
                               (:task !go-to-work ?t)
                               (:task !do-chores ?t))

                     homework
                     (have-homework)
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
                     (and (work-today) (raining) (have-homework))
                     (:ordered (:task !go-to-work ?t)
                               (:task !go-to-school ?t)
                               (:task !do-homework ?t)
                               (:task !play-videogames ?t))

                     rain-homework-movie
                     (and (found-movie) (raining) (have-homework))
                     (:ordered (:task !go-to-school ?t)
                               (:task !do-homework ?t)
                               (:task !do-chores ?t)
                               (:task !watch-movie ?t))

                     rain-homework
                     (and (raining) (have-homework))
                     (:ordered (:task !go-to-school ?t)
                               (:task !do-homework ?t)
                               (:task !do-chores ?t)
                               (:task !play-videogames ?t))

                     work-homework
                     (and (work-today) (have-homework))
                     (:ordered (:task !go-to-work ?t)
                               (:task !go-to-school ?t)
                               (:task !do-homework ?t)
                               (:task !go-running ?t))

                     homework-movie
                     (and (found-movie) (have-homework))
                     (:ordered (:task !go-to-school ?t)
                               (:task !do-homework ?t)
                               (:task !go-running ?t)
                               (:task !watch-movie ?t))

                     homework
                     (have-homework)
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
                     (and (found-movie) (raining))
                     (:ordered (:task !go-to-school ?t)
                               (:task !stay-for-tutoring ?t)
                               (:task !play-videogames ?t)
                               (:task !watch-movie ?t))

                     rain
                     (raining)
                     (:ordered (:task !go-to-school ?t)
                               (:task !stay-for-tutoring ?t)
                               (:task !play-videogames ?t))

                     work
                     (work-today)
                     (:ordered (:task !go-to-work ?t)
                               (:task !go-to-school ?t)
                               (:task !go-running ?t)
                               (:task !play-videogames ?t))

                     no-work-movie
                     (found-movie)
                     (:ordered (:task !go-to-school ?t)
                               (:task !stay-for-tutoring ?t)
                               (:task !play-videogames ?t)
                               (:task !watch-movie ?t))

                     no-work
                     ()
                     (:ordered (:task !go-to-school ?t)
                               (:task !stay-for-tutoring ?t)
                               (:task !play-videogames ?t))
            )

            (:method (friday ?t)
                     rain-work
                     (and (work-today) (raining))
                     (:ordered (:task !do-chores ?t)
                               (:task !play-videogames ?t)
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
