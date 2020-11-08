# actions
def go_to_school(state):
    state["went-to-school"] = 1
    return state


def go_to_work(state):
    state["went-to-work"] = 1
    return state


def do_chores(state):
    state["did-chores"] = 1
    return state


def do_homework(state):
    state["did-homework"] = 1
    state["have-homework"] = 0
    return state


def stay_for_tutoring(state):
    state["stayed-for-tutoring"] = 1
    return state


def go_running(state):
    state["ran"] = 1
    return state


def play_videogames(state):
    state["played-videogames"] = 1
    return state


def go_to_store(state):
    state["went-to-store"] = 1
    state["need-groceries"] = 0
    return state


def watch_movie(state):
    state["watched-movie"] = 1
    state["found-movies"] = 0
    return state


actions = [
    go_to_school,
    go_to_work,
    do_chores,
    do_homework,
    stay_for_tutoring,
    go_running,
    play_videogames,
    go_to_store,
    watch_movie,
]

# preconditions


def default(state):
    return True


def work_raining_homework(state):
    if state["raining"]:
        if state["have-homework"]:
            if state["work-today"]:
                return True
    return False


def raining_homework(state):
    if state["raining"]:
        if state["have-homework"]:
            if not state["work-today"]:
                return True
    return False


def work_homework(state):
    if not state["raining"]:
        if state["have-homework"]:
            if state["work-today"]:
                return True
    return False


def homework(state):
    if not state["raining"]:
        if state["have-homework"]:
            if not state["work-today"]:
                return True
    return False


def work_raining(state):
    if state["raining"]:
        if not state["have-homework"]:
            if state["work-today"]:
                return True
    return False


def raining(state):
    if state["raining"]:
        if not state["have-homework"]:
            if not state["work-today"]:
                return True
    return False


def work(state):
    if not state["raining"]:
        if not state["have-homework"]:
            if state["work-today"]:
                return True
    return False


def no_work(state):
    if not state["raining"]:
        if not state["have-homework"]:
            if not state["work-today"]:
                return True
    return False


def work_raining_homework_store(state):
    if state["raining"]:
        if state["have-homework"]:
            if state["work-today"]:
                if state["need-groceries"]:
                    return True
    return False


def work_raining_homework_no_store(state):
    if state["raining"]:
        if state["have-homework"]:
            if state["work-today"]:
                if not state["need-groceries"]:
                    return True
    return False


def raining_homework_store(state):
    if state["raining"]:
        if state["have-homework"]:
            if not state["work-today"]:
                if state["need-groceries"]:
                    return True
    return False


def raining_homework_no_store(state):
    if state["raining"]:
        if state["have-homework"]:
            if not state["work-today"]:
                if not state["need-groceries"]:
                    return True
    return False


def work_homework_store(state):
    if not state["raining"]:
        if state["have-homework"]:
            if state["work-today"]:
                if state["need-groceries"]:
                    return True
    return False


def work_homework_no_store(state):
    if not state["raining"]:
        if state["have-homework"]:
            if state["work-today"]:
                if not state["need-groceries"]:
                    return True
    return False


def homework_store(state):
    if not state["raining"]:
        if state["have-homework"]:
            if not state["work-today"]:
                if state["need-groceries"]:
                    return True
    return False


def homework_no_store(state):
    if not state["raining"]:
        if state["have-homework"]:
            if not state["work-today"]:
                if not state["need-groceries"]:
                    return True
    return False


def work_raining_store(state):
    if state["raining"]:
        if not state["have-homework"]:
            if state["work-today"]:
                if state["need-groceries"]:
                    return True
    return False


def work_raining_no_store(state):
    if state["raining"]:
        if not state["have-homework"]:
            if state["work-today"]:
                if not state["need-groceries"]:
                    return True
    return False


def raining_store(state):
    if state["raining"]:
        if not state["have-homework"]:
            if not state["work-today"]:
                if state["need-groceries"]:
                    return True
    return False


def raining_no_store(state):
    if state["raining"]:
        if not state["have-homework"]:
            if not state["work-today"]:
                if not state["need-groceries"]:
                    return True
    return False


def work_store(state):
    if not state["raining"]:
        if not state["have-homework"]:
            if state["work-today"]:
                if state["need-groceries"]:
                    return True
    return False


def work_no_store(state):
    if not state["raining"]:
        if not state["have-homework"]:
            if state["work-today"]:
                if not state["need-groceries"]:
                    return True
    return False


def no_work_store(state):
    if not state["raining"]:
        if not state["have-homework"]:
            if not state["work-today"]:
                if state["need-groceries"]:
                    return True
    return False


def no_work_no_store(state):
    if not state["raining"]:
        if not state["have-homework"]:
            if not state["work-today"]:
                if not state["need-groceries"]:
                    return True
    return False


def raining_homework_movie(state):
    if state["raining"]:
        if state["have-homework"]:
            if not state["work-today"]:
                if state["found-movie"]:
                    return True
    return False


def raining_homework_no_movie(state):
    if state["raining"]:
        if state["have-homework"]:
            if not state["work-today"]:
                if not state["found-movie"]:
                    return True
    return False


def homework_movie(state):
    if not state["raining"]:
        if state["have-homework"]:
            if not state["work-today"]:
                if state["found-movie"]:
                    return True
    return False


def homework_no_movie(state):
    if not state["raining"]:
        if state["have-homework"]:
            if not state["work-today"]:
                if not state["found-movie"]:
                    return True
    return False


def raining_movie(state):
    if state["raining"]:
        if not state["have-homework"]:
            if not state["work-today"]:
                if state["found-movie"]:
                    return True
    return False


def raining_no_movie(state):
    if state["raining"]:
        if not state["have-homework"]:
            if not state["work-today"]:
                if not state["found-movie"]:
                    return True
    return False


def no_work_movie(state):
    if not state["raining"]:
        if not state["have-homework"]:
            if not state["work-today"]:
                if state["found-movie"]:
                    return True
    return False


def no_work_no_movie(state):
    if not state["raining"]:
        if not state["have-homework"]:
            if not state["work-today"]:
                if not state["found-movie"]:
                    return True
    return False


def work_raining_friday(state):
    if state["raining"]:
        if state["work-today"]:
            return True
    return False


def raining_friday(state):
    if state["raining"]:
        if not state["work-today"]:
            return True
    return False


def work_friday(state):
    if not state["raining"]:
        if state["work-today"]:
            return True
    return False


def no_work_friday(state):
    if not state["raining"]:
        if not state["work-today"]:
            return True
    return False


# methods
methods = [
    {
        "task": "P",
        "preconditions": default,
        "subtasks": ["MONDAY"],
        "t_prob": 0.2,
    },
    {
        "task": "P",
        "preconditions": default,
        "subtasks": ["TUESDAY"],
        "t_prob": 0.2,
    },
    {
        "task": "P",
        "preconditions": default,
        "subtasks": ["WEDNESDAY"],
        "t_prob": 0.2,
    },
    {
        "task": "P",
        "preconditions": default,
        "subtasks": ["THURSDAY"],
        "t_prob": 0.2,
    },
    {
        "task": "P",
        "preconditions": default,
        "subtasks": ["FRIDAY"],
        "t_prob": 0.2,
    },
    {
        "task": "MONDAY",
        "preconditions": work_raining_homework,
        "subtasks": [
            "!go_to_school",
            "!go_to_work",
            "!do_chores",
            "!do_homework",
        ],
        "t_prob": 1,
    },
    {
        "task": "MONDAY",
        "preconditions": raining_homework,
        "subtasks": [
            "!go_to_school",
            "!stay_for_tutoring",
            "!do_chores",
            "!do_homework",
        ],
        "t_prob": 1,
    },
    {
        "task": "MONDAY",
        "preconditions": work_homework,
        "subtasks": [
            "!go_to_school",
            "!go_to_work",
            "!go_running",
            "!do_homework",
        ],
        "t_prob": 1,
    },
    {
        "task": "MONDAY",
        "preconditions": homework,
        "subtasks": [
            "!go_to_school",
            "!stay_for_tutoring",
            "!go_running",
            "!do_homework",
        ],
        "t_prob": 1,
    },
    {
        "task": "MONDAY",
        "preconditions": work_raining,
        "subtasks": [
            "!go_to_school",
            "!go_to_work",
            "!do_chores",
            "!play_videogames",
        ],
        "t_prob": 1,
    },
    {
        "task": "MONDAY",
        "preconditions": raining,
        "subtasks": [
            "!go_to_school",
            "!stay_for_tutoring",
            "!do_chores",
            "!play_videogames",
        ],
        "t_prob": 1,
    },
    {
        "task": "MONDAY",
        "preconditions": work,
        "subtasks": [
            "!go_to_school",
            "!go_to_work",
            "!go_running",
            "!play_videogames",
        ],
        "t_prob": 1,
    },
    {
        "task": "MONDAY",
        "preconditions": no_work,
        "subtasks": [
            "!go_to_school",
            "!stay_for_tutoring",
            "!go_running",
            "!play_videogames",
        ],
        "t_prob": 1,
    },
    {
        "task": "TUESDAY",
        "preconditions": work_raining_homework_store,
        "subtasks": [
            "!go_to_work",
            "!go_to_school",
            "!go_to_store",
            "!do_homework",
        ],
        "t_prob": 1,
    },
    {
        "task": "TUESDAY",
        "preconditions": work_raining_homework_no_store,
        "subtasks": [
            "!go_to_work",
            "!go_to_school",
            "!do_homework",
            "!play_videogames",
        ],
        "t_prob": 1,
    },
    {
        "task": "TUESDAY",
        "preconditions": raining_homework_store,
        "subtasks": [
            "!go_to_school",
            "!stay_for_tutoring",
            "!go_to_store",
            "!do_homework",
        ],
        "t_prob": 1,
    },
    {
        "task": "TUESDAY",
        "preconditions": raining_homework_no_store,
        "subtasks": [
            "!go_to_school",
            "!stay_for_tutoring",
            "!do_homework",
            "!play_videogames",
        ],
        "t_prob": 1,
    },
    {
        "task": "TUESDAY",
        "preconditions": work_homework_store,
        "subtasks": [
            "!go_to_work",
            "!go_to_school",
            "!go_to_store",
            "!do_homework",
        ],
        "t_prob": 1,
    },
    {
        "task": "TUESDAY",
        "preconditions": work_homework_no_store,
        "subtasks": [
            "!go_to_work",
            "!go_to_school",
            "!do_homework",
            "!play_videogames",
        ],
        "t_prob": 1,
    },
    {
        "task": "TUESDAY",
        "preconditions": homework_store,
        "subtasks": [
            "!go_to_school",
            "!stay_for_tutoring",
            "!go_to_store",
            "!do_homework",
        ],
        "t_prob": 1,
    },
    {
        "task": "TUESDAY",
        "preconditions": homework_no_store,
        "subtasks": [
            "!go_to_school",
            "!stay_for_tutoring",
            "!do_homework",
            "!play_videogames",
        ],
        "t_prob": 1,
    },
    {
        "task": "TUESDAY",
        "preconditions": work_raining_store,
        "subtasks": [
            "!go_to_work",
            "!go_to_school",
            "!go_to_store",
            "!play_videogames",
        ],
        "t_prob": 1,
    },
    {
        "task": "TUESDAY",
        "preconditions": work_raining_no_store,
        "subtasks": [
            "!go_to_work",
            "!go_to_school",
            "!do_chores",
            "!play_videogames",
        ],
        "t_prob": 1,
    },
    {
        "task": "TUESDAY",
        "preconditions": raining_store,
        "subtasks": [
            "!go_to_school",
            "!stay_for_tutoring",
            "!go_to_store",
            "!play_videogames",
        ],
        "t_prob": 1,
    },
    {
        "task": "TUESDAY",
        "preconditions": raining_no_store,
        "subtasks": [
            "!go_to_school",
            "!stay_for_tutoring",
            "!do_chores",
            "!play_videogames",
        ],
        "t_prob": 1,
    },
    {
        "task": "TUESDAY",
        "preconditions": work_store,
        "subtasks": [
            "!go_to_work",
            "!go_to_school",
            "!go_to_store",
            "!play_videogames",
        ],
        "t_prob": 1,
    },
    {
        "task": "TUESDAY",
        "preconditions": work_no_store,
        "subtasks": [
            "!go_to_work",
            "!go_to_school",
            "!go_running",
            "!play_videogames",
        ],
        "t_prob": 1,
    },
    {
        "task": "TUESDAY",
        "preconditions": no_work_store,
        "subtasks": [
            "!go_to_school",
            "!stay_for_tutoring",
            "!go_to_store",
            "!play_videogames",
        ],
        "t_prob": 1,
    },
    {
        "task": "TUESDAY",
        "preconditions": no_work_no_store,
        "subtasks": [
            "!go_to_school",
            "!stay_for_tutoring",
            "!go_running",
            "!play_videogames",
        ],
        "t_prob": 1,
    },
    {
        "task": "WEDNESDAY",
        "preconditions": work_raining_homework,
        "subtasks": [
            "!go_to_school",
            "!do_homework",
            "!go_to_work",
            "!do_chores",
        ],
        "t_prob": 1,
    },
    {
        "task": "WEDNESDAY",
        "preconditions": raining_homework,
        "subtasks": [
            "!go_to_school",
            "!do_homework",
            "!do_chores",
            "!play_videogames",
        ],
        "t_prob": 1,
    },
    {
        "task": "WEDNESDAY",
        "preconditions": work_homework,
        "subtasks": [
            "!go_to_school",
            "!do_homework",
            "!go_to_work",
            "!do_chores",
        ],
        "t_prob": 1,
    },
    {
        "task": "WEDNESDAY",
        "preconditions": homework,
        "subtasks": [
            "!go_to_school",
            "!do_homework",
            "!go_running",
            "!do_chores",
        ],
        "t_prob": 1,
    },
    {
        "task": "WEDNESDAY",
        "preconditions": work_raining,
        "subtasks": [
            "!go_to_school",
            "!stay_for_tutoring",
            "!go_to_work",
            "!do_chores",
        ],
        "t_prob": 1,
    },
    {
        "task": "WEDNESDAY",
        "preconditions": raining,
        "subtasks": [
            "!go_to_school",
            "!stay_for_tutoring",
            "!play_videogames",
            "!do_chores",
        ],
        "t_prob": 1,
    },
    {
        "task": "WEDNESDAY",
        "preconditions": work,
        "subtasks": [
            "!go_to_school",
            "!go_to_work",
            "!go_running",
            "!do_chores",
        ],
        "t_prob": 1,
    },
    {
        "task": "WEDNESDAY",
        "preconditions": no_work,
        "subtasks": [
            "!go_to_school",
            "!stay_for_tutoring",
            "!go_running",
            "!do_chores",
        ],
        "t_prob": 1,
    },
    {
        "task": "THURSDAY",
        "preconditions": work_raining_homework,
        "subtasks": [
            "!go_to_work",
            "!go_to_school",
            "!do_homework",
            "!play_videogames",
        ],
        "t_prob": 1,
    },
    {
        "task": "THURSDAY",
        "preconditions": raining_homework_movie,
        "subtasks": [
            "!go_to_school",
            "!do_homework",
            "!do_chores",
            "!watch_movie",
        ],
        "t_prob": 1,
    },
    {
        "task": "THURSDAY",
        "preconditions": raining_homework_no_movie,
        "subtasks": [
            "!go_to_school",
            "!do_homework",
            "!do_chores",
            "!play_videogames",
        ],
        "t_prob": 1,
    },
    {
        "task": "THURSDAY",
        "preconditions": work_homework,
        "subtasks": [
            "!go_to_work",
            "!go_to_school",
            "!do_homework",
            "!go_running",
        ],
        "t_prob": 1,
    },
    {
        "task": "THURSDAY",
        "preconditions": homework_movie,
        "subtasks": [
            "!go_to_school",
            "!do_homework",
            "!go_running",
            "!watch_movie",
        ],
        "t_prob": 1,
    },
    {
        "task": "THURSDAY",
        "preconditions": homework_no_movie,
        "subtasks": [
            "!go_to_school",
            "!do_homework",
            "!go_running",
            "!play_videogames",
        ],
        "t_prob": 1,
    },
    {
        "task": "THURSDAY",
        "preconditions": work_raining,
        "subtasks": [
            "!go_to_work",
            "!go_to_school",
            "!stay_for_tutoring",
            "!play_videogames",
        ],
        "t_prob": 1,
    },
    {
        "task": "THURSDAY",
        "preconditions": raining_movie,
        "subtasks": [
            "!go_to_school",
            "!stay_for_tutoring",
            "!play_videogames",
            "!watch_movie",
        ],
        "t_prob": 1,
    },
    {
        "task": "THURSDAY",
        "preconditions": raining_no_movie,
        "subtasks": [
            "!go_to_school",
            "!stay_for_tutoring",
            "!play_videogames",
        ],
        "t_prob": 1,
    },
    {
        "task": "THURSDAY",
        "preconditions": work,
        "subtasks": [
            "!go_to_work",
            "!go_to_school",
            "!go_running",
            "!play_videogames",
        ],
        "t_prob": 1,
    },
    {
        "task": "THURSDAY",
        "preconditions": no_work_movie,
        "subtasks": [
            "!go_to_school",
            "!stay_for_tutoring",
            "!play_videogames",
            "!watch_movie",
        ],
        "t_prob": 1,
    },
    {
        "task": "THURSDAY",
        "preconditions": no_work_no_movie,
        "subtasks": [
            "!go_to_school",
            "!stay_for_tutoring",
            "!play_videogames",
        ],
        "t_prob": 1,
    },
    {
        "task": "FRIDAY",
        "preconditions": work_raining_friday,
        "subtasks": ["!do_chores", "!play_videogames", "!go_to_work"],
        "t_prob": 1,
    },
    {
        "task": "FRIDAY",
        "preconditions": raining_friday,
        "subtasks": [
            "!go_to_school",
            "!stay_for_tutoring",
            "!do_chores",
            "!play_videogames",
        ],
        "t_prob": 1,
    },
    {
        "task": "FRIDAY",
        "preconditions": work_friday,
        "subtasks": ["!play_videogames", "!go_running", "!go_to_work"],
        "t_prob": 1,
    },
    {
        "task": "FRIDAY",
        "preconditions": no_work_friday,
        "subtasks": [
            "!go_to_school",
            "!go_running",
            "!do_chores",
            "!play_videogames",
        ],
        "t_prob": 1,
    },
]
