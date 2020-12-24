from numpy import random, e, math
from PSDG_Domain import PSDG_Action, PSDG_Method

# Actions
## SearchHallway
def searchHallway_effect(state):
    green = random.poisson(0.5, 1)[0]
    if state["time"] < 420:
        yellow = random.poisson(0.2, 1)[0]
        state["num_of_yellow_victims_found_in_adj_room"] += yellow
        state["num_of_yellow_victims_found_total"] += yellow

    state["num_of_green_victims_found_in_adj_room"] += green
    state["num_of_green_victims_found_total"] += green
    state["time"] += random.poisson(5.5, 1)[0]
    state["times_searched"] += 1
    state["recent_search"] = 1
    return state


def searchHallway_trans_prob(state_0, state_1):
    if not (state_0["current_loc"] in state_0["hallways"]):
        return 0.0

    if not (state_1["current_loc"] in state_1["hallways"]):
        return 0.0

    if state_1["times_searched"] - state_0["times_searched"] < 1:
        return 0.0

    if state_1["recent_search"] != 1:
        return 0.0

    green_found = (
        state_1["num_of_green_victims_found_in_adj_room"]
        - state_0["num_of_green_victims_found_in_adj_room"]
    )

    if green_found < 0:
        return 0.0

    green_found_prob = (0.5 ** green_found * e ** (-0.5)) / math.factorial(
        green_found
    )

    yellow_found = (
        state_1["num_of_yellow_victims_found_in_adj_room"]
        - state_0["num_of_yellow_victims_found_in_adj_room"]
    )

    if yellow_found < 0:
        return 0.0

    if yellow_found >= 1 and state_0["time"] >= 420:
        return 0.0

    yellow_found_prob = (0.2 ** yellow_found * e ** (-0.2)) / math.factorial(
        yellow_found
    )

    time_elasped = state_1["time"] - state_0["time"]

    time_elasped_prob = (5.5 ** time_elasped * e ** (-5.5)) / math.factorial(
        time_elasped
    )

    return green_found_prob * yellow_found_prob * time_elasped_prob


searchHallway = PSDG_Action(
    "!searchHallway", searchHallway_effect, searchHallway_trans_prob
)

## searchRoom


def searchRoom_effect(state):
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
    state["recent_search"] = 1
    return state


def searchRoom_trans_prob(state_0, state_1):
    if not (state_0["current_loc"] in state_0["rooms"]):
        return 0.0

    if not (state_1["current_loc"] in state_1["rooms"]):
        return 0.0

    if state_1["times_searched"] - state_0["times_searched"] < 1:
        return 0.0

    if state_1["recent_search"] != 1:
        return 0.0

    green_found_adj = (
        state_1["num_of_green_victims_found_in_adj_room"]
        - state_0["num_of_green_victims_found_in_adj_room"]
    )

    if green_found_adj < 0:
        return 0.0

    green_found_adj_prob = (
        0.5 ** green_found_adj * e ** (-0.5)
    ) / math.factorial(green_found_adj)

    green_found_curr = (
        state_1["num_of_green_victims_found_in_current_room"]
        - state_0["num_of_green_victims_found_in_current_room"]
    )

    if green_found_curr < 0:
        return 0.0

    green_found_curr_prob = (
        1.5 ** green_found_curr * e ** (-1.5)
    ) / math.factorial(green_found_curr)

    yellow_found_adj = (
        state_1["num_of_yellow_victims_found_in_adj_room"]
        - state_0["num_of_yellow_victims_found_in_adj_room"]
    )

    if yellow_found_adj < 0:
        return 0.0

    if yellow_found_adj >= 1 and state_0["time"] >= 420:
        return 0.0

    yellow_found_adj_prob = (
        0.2 ** yellow_found_adj * e ** (-0.2)
    ) / math.factorial(yellow_found_adj)

    yellow_found_curr = (
        state_1["num_of_yellow_victims_found_in_current_room"]
        - state_0["num_of_yellow_victims_found_in_current_room"]
    )

    if yellow_found_curr < 0:
        return 0.0

    if yellow_found_curr >= 1 and state_0["time"] >= 420:
        return 0.0

    yellow_found_curr_prob = (
        1 ** yellow_found_curr * e ** (-1)
    ) / math.factorial(yellow_found_curr)

    time_elasped = state_1["time"] - state_0["time"]

    time_elasped_prob = (5.5 ** time_elasped * e ** (-5.5)) / math.factorial(
        time_elasped
    )

    return (
        green_found_adj_prob
        * yellow_found_adj_prob
        * time_elasped_prob
        * green_found_curr_prob
        * yellow_found_curr_prob
    )


searchRoom = PSDG_Action(
    "!searchRoom", searchRoom_effect, searchRoom_trans_prob
)

## triageGreen


def triageGreen_effect(state):
    state["num_of_green_victims_triaged_total"] += 1
    state["num_of_green_victims_triaged_in_current_room"] += 1
    state["time"] += 8
    state["recent_search"] = 0
    return state


def triageGreen_trans_prob(state_0, state_1):
    if (
        state_1["num_of_green_victims_triaged_in_current_room"]
        <= state_0["num_of_green_victims_triaged_in_current_room"]
    ):
        return 0.0

    if (state_1["time"] - state_0["time"]) != 8:
        return 0.0

    if state_1["recent_search"] != 0:
        return 0.0

    return 1.0


triageGreen = PSDG_Action(
    "!triageGreen", triageGreen_effect, triageGreen_trans_prob
)

## triageYellow


def triageYellow_effect(state):
    state["num_of_yellow_victims_triaged_total"] += 1
    state["num_of_yellow_victims_triaged_in_current_room"] += 1
    state["time"] += 15
    state["recent_search"] = 0
    return state


def triageYellow_trans_prob(state_0, state_1):
    if (
        state_1["num_of_yellow_victims_triaged_in_current_room"]
        <= state_0["num_of_yellow_victims_triaged_in_current_room"]
    ):
        return 0.0

    if (state_1["time"] - state_0["time"]) != 15:
        return 0.0

    if state_1["recent_search"] != 0:
        return 0.0

    if state_0["time"] >= 420:
        return 0.0

    return 1.0


triageYellow = PSDG_Action(
    "!triageYellow", triageYellow_effect, triageYellow_trans_prob
)

## move


def move_effect(state):
    state["num_of_green_victims_found_in_current_room"] = 0
    state["num_of_yellow_victims_found_in_current_room"] = 0
    state["num_of_green_victims_found_in_adj_room"] = 0
    state["num_of_yellow_victims_found_in_adj_room"] = 0
    state["num_of_green_victims_triaged_in_current_room"] = 0
    state["num_of_yellow_victims_triaged_in_current_room"] = 0
    state["current_loc"] = state["next_loc"].pop(0)
    state["time"] += random.poisson(5.6, 1)[0]
    state["times_searched"] = 0
    state["recent_search"] = 0
    return state


def move_trans_prob(state_0, state_1):
    if state_1["num_of_green_victims_found_in_current_room"] != 0:
        return 0.0

    if state_1["num_of_yellow_victims_found_in_current_room"] != 0:
        return 0.0

    if state_1["num_of_green_victims_found_in_adj_room"] != 0:
        return 0.0

    if state_1["num_of_yellow_victims_found_in_adj_room"] != 0:
        return 0.0

    if state_1["num_of_green_victims_triaged_in_current_room"] != 0:
        return 0.0

    if state_1["num_of_yellow_victims_triaged_in_current_room"] != 0:
        return 0.0

    if state_0["next_loc"][0] != state_1["current_loc"]:
        return 0.0

    if state["times_search"] != 0:
        return 0.0

    if state["recent_search"] != 0:
        return 0.0

    time_elasped = state_1["time"] - state_0["time"]

    time_elasped_prob = (5.6 ** time_elasped * e ** (-5.6)) / math.factorial(
        time_elasped
    )


move = PSDG_Action("!move", move_effect, move_trans_prob)

## exit


def exit_effect(state):
    return state


def exit_trans_prob(state_0, state_1):
    if state_0 == state_1:
        return 1.0
    return 0.0


exit = PSDG_Action("!exit", exit_effect, exit_trans_prob)

actions = [searchHallway, searchRoom, triageGreen, triageYellow, move, exit]

# Methods

## start methods

### default precondition
def default(state):
    return 0.5


initial_YF = PSDG_Method("P", default, ["YF"])
initial_O = PSDG_Method("P", default, ["O"])

## searchHall YF method


def willSearchHall_YF(state):
    if state["time"] >= 600:
        return 0
    if state["recent_search"]:
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
        return 1 / (e * math.factorial(state["times_searched"]))
    return 0


searchHall_YF = PSDG_Method("YF", willSearchHall_YF, ["!searchHallway", "YF"])

## searchRoom YF method


def willSearchRoom_YF(state):
    if state["time"] >= 600:
        return 0
    if state["recent_search"]:
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
        return 1 / (e * math.factorial(state["times_searched"]))
    return 0


searchRoom_YF = PSDG_Method("YF", willSearchRoom_YF, ["!searchRoom", "YF"])

## triageYellow YF method


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


triageYellow_YF = PSDG_Method(
    "YF", willTriageYellow_YF, ["!triageYellow", "YF"]
)

## triageGreen YF method


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


triageGreen_YF = PSDG_Method("YF", willTriageGreen_YF, ["!triageGreen", "YF"])

## move YF method


def willMove_YF(state):
    if state["time"] >= 600 or not state["next_loc"]:
        return 0
    if willTriageGreen_YF(state) or willTriageYellow_YF(state):
        return 0
    if state["current_loc"] in state["hallways"]:
        return 1 - willSearchHall_YF(state)
    return 1 - willSearchRoom_YF(state)


move_YF = PSDG_Method("YF", willMove_YF, ["!move", "YF"])

## exit YF method


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


exit_YF = PSDG_Method("YF", willExit_YF, ["!exit"])

## SearchHallway O method


def willSearchHall_O(state):
    if state["time"] >= 600:
        return 0
    if state["current_loc"] in state["hallways"]:
        if not state["times_searched"]:
            return 1
        if state["num_of_yellow_victims_found_in_adj_room"]:
            return 0
        if state["num_of_green_victims_found_in_adj_room"]:
            return 0
        return 1 / (e * math.factorial(state["times_searched"]))
    return 0


searchHall_O = PSDG_Method("O", willSearchHall_O, ["!searchHallway", "O"])

## SearchRoom O method


def willSearchRoom_O(state):
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

        if yellows_to_triage:
            return 0
        if greens_to_triage:
            return 0
        if state["num_of_yellow_victims_found_in_adj_room"]:
            return 0
        if state["num_of_green_victims_found_in_adj_room"]:
            return 0
        return 1 / (e * math.factorial(state["times_searched"]))
    return 0


searchRoom_O = PSDG_Method("O", willSearchRoom_O, ["!searchRoom", "O"])

## triageYellow O method


def willTriageYellow_O(state):
    if state["time"] >= 405:
        return 0
    yellows_to_triage = (
        state["num_of_yellow_victims_found_in_current_room"]
        - state["num_of_yellow_victims_triaged_in_current_room"]
    )

    if yellows_to_triage:
        return 0.5
    return 0


triageYellow_O = PSDG_Method("O", willTriageYellow_O, ["!triageYellow", "O"])

## triageGreen O method


def willTriageGreen_O(state):
    if state["time"] >= 600:
        return 0
    greens_to_triage = (
        state["num_of_green_victims_found_in_current_room"]
        - state["num_of_green_victims_triaged_in_current_room"]
    )

    if greens_to_triage:
        return 0.5
    return 0


triageGreen_O = PSDG_Method("O", willTriageGreen_O, ["!triageGreen", "O"])

## move O method


def willMove_O(state):
    if state["time"] >= 600 or not state["next_loc"]:
        return 0
    if willTriageGreen_YF(state) or willTriageYellow_YF(state):
        return 0
    if state["current_loc"] in state["hallways"]:
        return 1 - willSearchHall_YF(state)
    return 1 - willSearchRoom_YF(state)


move_O = PSDG_Method("O", willMove_O, ["!move", "O"])

## exit O method


def willExit_O(state):
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


exit_O = PSDG_Method("O", willExit_O, ["!exit"])

# Methods
methods = [
    initial_YF,
    initial_O,
    searchHall_YF,
    searchRoom_YF,
    triageYellow_YF,
    triageGreen_YF,
    move_YF,
    exit_YF,
    searchHall_O,
    searchRoom_O,
    triageYellow_O,
    triageGreen_O,
    move_O,
    exit_O,
]
