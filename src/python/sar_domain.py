from numpy import random, e, math

# Actions
def searchHallway(state):
    green = random.poisson(0.5, 1)[0]
    if state["time"] < 420:
        yellow = random.poisson(0.2, 1)[0]
        state["num_of_yellow_victims_found_in_adj_room"] += yellow
        state["num_of_yellow_victims_found_total"] += yellow
    state["num_of_green_victims_found_in_adj_room"] += green
    state["num_of_green_victims_found_total"] += green
    state["time"] += random.poisson(5.5, 1)[0]
    state["times_searched"] += 1
    return state


def searchRoom(state):
    green = random.poisson(1.5, 1)[0]
    if state["time"] < 420:
        yellow = random.poisson(1, 1)[0]
        yellow_adj = random.poisson(0.2, 1)[0]
        state["num_of_yellow_victims_found_in_current_room"] += yellow
        state["num_of_yellow_victims_found_in_adj_room"] += yellow_adj
        state["num_of_yellow_victims_found_total"] += yellow + yellow_adj
    green_adj = random.poisson(0.5, 1)[0]
    state["num_of_green_victims_found_in_current_room"] += green
    state["num_of_green_victims_found_in_adj_room"] += green_adj
    state["num_of_green_victims_found_total"] += green + green_adj
    state["time"] += random.poisson(5.5, 1)[0]
    state["times_searched"] += 1
    return state


def triageGreen(state):
    state["num_of_green_victims_triaged_total"] += 1
    state["num_of_green_victims_triaged_in_current_room"] += 1
    state["time"] += 8
    return state


def triageYellow(state):
    state["num_of_yellow_victims_triaged_total"] += 1
    state["num_of_yellow_victims_triaged_in_current_room"] += 1
    state["time"] += 15
    return state


def move(state):
    state["num_of_green_victims_seen_in_current_room"] = 0
    state["num_of_yellow_victims_seen_in_current_room"] = 0
    state["num_of_green_victims_found_in_adj_room"] = 0
    state["num_of_yellow_victims_found_in_adj_room"] = 0
    state["num_of_green_victims_triaged_in_current_room"] = 0
    state["num_of_yellow_victims_triaged_in_current_room"] = 0
    state["current_loc"] = state["next_loc"].pop()
    state["time"] = random.poisson(5.6, 1)[0]
    state["times_searched"] = 0
    return state


def exit(state):
    return state


actions = [searchHallway, searchRoom, triageGreen, triageYellow, move, exit]

# Preconditions


def default(state):
    return 0.5


def willSearchHall_YF(state):
    if state["time"] >= 600:
        return 0
    if state["current_loc"] in state["hallways"]:
        if not state["times_searched"]:
            return 1
        if (
            state["time"] < 405
            and state["num_of_yellow_victims_found_in_adj_room"]
        ):
            return 0
        if (
            state["time"] >= 405
            and state["num_of_green_victims_found_in_adj_room"]
        ):
            return 0
        return 1 / (e * math.factorial(state["times_search"]))
    return 0


def willSearchRoom_YF(state):
    if state["time"] >= 600:
        return 0
    if state["current_loc"] in state["rooms"]:
        if not state["times_searched"]:
            return 1
        yellows_to_triage = (
            state["num_of_yellow_victims_found_in_current_room"]
            - state["num_of_yellow_victims_triaged_in_current_room"]
        )
        greens_to_triage = (
            state["num_of_green_victims_found_in_current_room"]
            - state["num_of_green_victims_triaged_in_current_room"]
        )

        if state["time"] < 405 and yellows_to_triage:
            return 0
        if state["time"] >= 405 and greens_to_triage:
            return 0
        if (
            state["time"] < 405
            and state["num_of_yellow_victims_found_in_adj_room"]
        ):
            return 0
        if (
            state["time"] >= 405
            and state["num_of_green_victims_found_in_adj_room"]
        ):
            return 0
        return 1 / (e * math.factorial(state["times_search"]))
    return 0


def willTriageYellow_YF(state):
    if state["time"] >= 405:
        return 0
    yellows_to_triage = (
        state["num_of_yellow_victims_found_in_current_room"]
        - state["num_of_yellow_victims_triaged_in_current_room"]
    )

    if yellows_to_triage:
        return 1
    return 0


def willTriageGreen_YF(state):
    if state["time"] >= 600:
        return 0
    if state["time"] < 405:
        return 0
    greens_to_triage = (
        state["num_of_green_victims_found_in_current_room"]
        - state["num_of_green_victims_triaged_in_current_room"]
    )

    if greens_to_triage:
        return 1
    return 0


def willMove_YF(state):
    if state["time"] >= 600 or state["next_loc"].empty():
        return 0
    if willTriageGreen_YF(state) or willTriageYellow_YF(state):
        return 0
    if state["current_loc"] in state["hallways"]:
        return 1 - willSearchHall_YF(state)
    return 1 - willSearchRoom_YF(state)


def willExit_YF(state):
    if state["time"] >= 600:
        return 1
    if (
        willMove_YF(state)
        or willTriageGreen_YF(state)
        or willTriageYellow_YF(state)
        or willSearchRoom_YF(state)
        or willSearchHall_YF(state)
    ):
        return 0
    return 1


# Methods
methods = [
    {"task": "P", "preconditions": default, "subtasks": ["YF"],},
    {"task": "P", "preconditions": default, "subtasks": ["O"],},
    {
        "task": "YF",
        "preconditions": willSearchHall_YF,
        "subtasks": ["!searchHallway", "YF"],
    },
    {
        "task": "YF",
        "preconditions": willSearchRoom_YF,
        "subtasks": ["!searchRoom", "YF"],
    },
    {
        "task": "YF",
        "preconditions": willTriageYellow_YF,
        "subtasks": ["!triageYellow", "YF"],
    },
    {
        "task": "YF",
        "preconditions": willTriageGreen_YF,
        "subtasks": ["!triageGreen", "YF"],
    },
    {"task": "YF", "preconditions": willMove_YF, "subtasks": ["!move", "YF"],},
    {"task": "YF", "preconditions": willExit_YF, "subtasks": ["!exit", "YF"],},
]
