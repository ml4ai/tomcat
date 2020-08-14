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

                     work
                     (work-today)
                     (:ordered (:task !go-to-school)
                               (:task !stay-for-tutoring)
                               (:task !go-running)
                               (:task !play-videogames))

                     no-work
                     ()
                     (:ordered (:task !go-to-school)
                               (:task !stay-for-tutoring)
                               (:task !go-running)
                               (:task !play-videogames))

            )

            (:method (tuesday)
                     work-rain-homework-store
                     (and (need-groceries) (work-today) (raining) (have-homework))
                     (:ordered (:task !go-to-work)
                               (:task !go-to-school)
                               (:task !go-to-store)
                               (:task !do-homework))

                     work-rain-homework
                     (and (work-today) (raining) (have-homework))
                     (:ordered (:task !go-to-work)
                               (:task !go-to-school)
                               (:task !do-homework)
                               (:task !play-videogames))

                     rain-homework-store
                     (and (need-groceries) (raining) (have-homework))
                     (:ordered (:task !go-to-school)
                               (:task !stay-for-tutoring)
                               (:task !go-to-store)
                               (:task !do-homework))

                     rain-homework
                     (and (raining) (have-homework))
                     (:ordered (:task !go-to-school)
                               (:task !stay-for-tutoring)
                               (:task !do-homework)
                               (:task !play-videogames))

                     work-homework-store
                     (and (need-groceries) (work-today) (have-homework))
                     (:ordered (:task !go-to-work)
                               (:task !go-to-school)
                               (:task !go-to-store)
                               (:task !do-homework))

                     work-homework
                     (and (work-today) (have-homework))
                     (:ordered (:task !go-to-work)
                               (:task !go-to-school)
                               (:task !do-homework)
                               (:task !play-videogames))

                     homework-store
                     (and (need-groceries) (have-homework))
                     (:ordered (:task !go-to-school)
                               (:task !stay-for-tutoring)
                               (:task !go-to-store)
                               (:task !do-homework))

                     homework
                     (have-homework)
                     (:ordered (:task !go-to-school)
                               (:task !stay-for-tutoring)
                               (:task !do-homework)
                               (:task !play-videogames))

                     work-rain-store
                     (and (need-groceries) (work-today) (raining))
                     (:ordered (:task !go-to-work)
                               (:task !go-to-school)
                               (:task !go-to-store)
                               (:task !play-videogames))

                     work-rain
                     (and (work-today) (raining))
                     (:ordered (:task !go-to-work)
                               (:task !go-to-school)
                               (:task !do-chores)
                               (:task !play-videogames))

                     rain-store
                     (and (need-groceries) (raining))
                     (:ordered (:task !go-to-school)
                               (:task !stay-for-tutoring)
                               (:task !go-to-store)
                               (:task !play-videogames))

                     rain
                     (raining)
                     (:ordered (:task !go-to-school)
                               (:task !stay-for-tutoring)
                               (:task !do-chores)
                               (:task !play-videogames))

                     work-store
                     (and (need-groceries) (work-today))
                     (:ordered (:task !go-to-work)
                               (:task !go-to-school)
                               (:task !go-to-store)
                               (:task !play-videogames))

                     work
                     (work-today)
                     (:ordered (:task !go-to-work)
                               (:task !go-to-school)
                               (:task !go-running)
                               (:task !play-videogames))

                     no-work-store
                     (need-groceries)
                     (:ordered (:task !go-to-school)
                               (:task !stay-for-tutoring)
                               (:task !go-to-store)
                               (:task !play-videogames))

                     no-work
                     ()
                     (:ordered (:task !go-to-school)
                               (:task !stay-for-tutoring)
                               (:task !go-running)
                               (:task !play-videogames))
            )
 
            (:method (wednesday)
                     work-rain-homework
                     (and (work-today) (raining) (have-homework))
                     (:ordered (:task !go-to-school)
                               (:task !do-homework)
                               (:task !go-to-work)
                               (:task !do-chores))

                     rain-homework
                     (and (raining) (have-homework))
                     (:ordered (:task !go-to-school)
                               (:task !do-homework)
                               (:task !do-chores)
                               (:task !play-videogames))

                     work-homework
                     (and (work-today) (have-homework))
                     (:ordered (:task !go-to-school)
                               (:task !do-homework)
                               (:task !go-to-work)
                               (:task !do-chores))

                     homework
                     (have-homework)
                     (:ordered (:task !go-to-school)
                               (:task !do-homework)
                               (:task !go-running)
                               (:task !do-chores))

                     work-rain
                     (and (work-today) (raining))
                     (:ordered (:task !go-to-school)
                               (:task !stay-for-tutoring)
                               (:task !go-to-work)
                               (:task !do-chores))

                     rain
                     (raining)
                     (:ordered (:task !go-to-school)
                               (:task !stay-for-tutoring)
                               (:task !play-videogames)
                               (:task !do-chores))

                     work
                     (work-today)
                     (:ordered (:task !go-to-school)
                               (:task !go-to-work)
                               (:task !go-running)
                               (:task !do-chores))

                     no-work
                     ()
                     (:ordered (:task !go-to-school)
                               (:task !stay-for-tutoring)
                               (:task !go-running)
                               (:task !do-chores))
            )

            (:method (thursday)
                     work-rain-homework
                     (and (work-today) (raining) (have-homework))
                     (:ordered (:task !go-to-work)
                               (:task !go-to-school)
                               (:task !do-homework)
                               (:task !play-videogames))

                     rain-homework-movie
                     (and (found-movie) (raining) (have-homework))
                     (:ordered (:task !go-to-school)
                               (:task !do-homework)
                               (:task !do-chores)
                               (:task !watch-movie))

                     rain-homework
                     (and (raining) (have-homework))
                     (:ordered (:task !go-to-school)
                               (:task !do-homework)
                               (:task !do-chores)
                               (:task !play-videogames))

                     work-homework
                     (and (work-today) (have-homework))
                     (:ordered (:task !go-to-work)
                               (:task !go-to-school)
                               (:task !do-homework)
                               (:task !go-running))

                     homework-movie
                     (and (found-movie) (have-homework))
                     (:ordered (:task !go-to-school)
                               (:task !do-homework)
                               (:task !go-running)
                               (:task !watch-movie))

                     homework
                     (have-homework)
                     (:ordered (:task !go-to-school)
                               (:task !do-homework)
                               (:task !go-running)
                               (:task !play-videogames))

                     work-rain
                     (and (work-today) (raining))
                     (:ordered (:task !go-to-work)
                               (:task !go-to-school)
                               (:task !stay-for-tutoring)
                               (:task !play-videogames))

                     rain-movie
                     (and (found-movie) (raining))
                     (:ordered (:task !go-to-school)
                               (:task !stay-for-tutoring)
                               (:task !play-videogames)
                               (:task !watch-movie))

                     rain
                     (raining)
                     (:ordered (:task !go-to-school)
                               (:task !stay-for-tutoring)
                               (:task !play-videogames))

                     work
                     (work-today)
                     (:ordered (:task !go-to-work)
                               (:task !go-to-school)
                               (:task !go-running)
                               (:task !play-videogames))

                     no-work-movie
                     (found-movie)
                     (:ordered (:task !go-to-school)
                               (:task !stay-for-tutoring)
                               (:task !play-videogames)
                               (:task !watch-movie))

                     no-work
                     ()
                     (:ordered (:task !go-to-school)
                               (:task !stay-for-tutoring)
                               (:task !play-videogames))
 
            )

            (:method (friday)

            )
 
  )
)
