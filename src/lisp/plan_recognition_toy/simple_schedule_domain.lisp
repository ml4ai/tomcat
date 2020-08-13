(ql:quickload "shop3")

(in-package :shop-user)

(defdomain (simple-schedule-domain :type pddl-domain :redefine-ok T) (
            (:predicates (raining)
                         (work-today)
                         (need-groceries)
                         (have-homework)
                         (found-movie)
            )

            (:action)

            (:method (monday)
                     work-rain-homework
                     (and (work-today) (raining) (have-homework))
                     (:ordered (:task !go-to-school)
                               (:task !go-to-work)
                               (:task !do-chores)
                               (:task !do-homework))

                     rain-homework
                     (and (raining) (have-homework))
                     (:ordered (:task !go-to-school)
                               (:task !stay-for-tutoring)
                               (:task !do-chores)
                               (:task !do-homework))

                     work-homework
                     (and (work-today) (have-homework))
                     (:ordered (:task !go-to-school)
                               (:task !go-to-work)
                               (:task !go-running)
                               (:task !do-homework))

                     homework
                     (have-homework)
                     (:ordered (:task !go-to-school)
                               (:task !stay-for-tutoring)
                               (:task !go-running)
                               (:task !do-homework))

                     work-rain
                     (and (work-today) (raining))
                     (:ordered (:task !go-to-school)
                               (:task !go-to-work)
                               (:task !do-chores)
                               (:task !play-videogames))

                     rain
                     (raining)
                     (:ordered (:task !go-to-school)
                               (:task !stay-for-tutoring)
                               (:task !do-chores)
                               (:task !play-videogames))

                     no-work
                     ()
                     (:ordered (:task !go-to-school)
                               (:task !stay-for-tutoring)
                               (:task !go-running)
                               (:task !play-videogames))

            )

            (:method (tuesday)

            )
 
            (:method (wednesday)

            )

            (:method (thursday)

            )

            (:method (friday)

            )
 
  )
)
