from nltk import Nonterminal
from PSDG import PSDG
from PSDProduction import PSDProduction
import sys
import random
import copy
import itertools


class PSDG_Domain:
    def __init__(self, methods, actions):
        self.methods = []
        for i in methods:
            if i["task"]:
                if i["task"][0] == "!":
                    raise ValueError(
                        "Compound Task name should not start with !"
                    )
            else:
                raise ValueError(
                    "Compound Task name should have atleast one character"
                )
            if not callable(i["preconditions"]):
                raise ValueError("Preconditions must be callable object")
            if (not isinstance(i["subtasks"], list)) or (
                not all(isinstance(e, str) for e in i["subtasks"])
            ):
                raise ValueError("Subtasks must be a list of strings")
            self.methods.append(i)
        self.actions = []
        for i in actions:
            if not callable(i):
                raise ValueError("Action must be callable object")
            self.actions.append(i)

    def initialize_planning(self, i_state):
        self.i_state = i_state
        self.c_state = copy.deepcopy(i_state)
        self.states = []
        prods = []
        for i in self.methods:
            subtasks = []
            for j in i["subtasks"]:
                if j[0] == "!":
                    subtasks.append(j)
                else:
                    subtasks.append(Nonterminal(j))
            prods.append(
                PSDProduction(
                    Nonterminal(i["task"]),
                    subtasks,
                    i_state,
                    i["preconditions"],
                )
            )
        self.psdg = PSDG(Nonterminal("P"), prods)

    def _sample_plan(self,symbol=None,depth=None):
        if depth is None:
            depth = sys.maxsize

        if depth > 0:
            if symbol is None:
                symbol = self.psdg.start()
            plan = []
            prod = random.choices(
                [x for x in self.psdg.productions(lhs=symbol)],
                [x.prob() for x in self.psdg.productions(lhs=symbol)],
            )[0]

            for i in prod.rhs():
                if isinstance(i, Nonterminal):
                    plan = plan + self._sample_plan(i,depth-1)
                else:
                    for j in self.actions:
                        if j.__name__ == i[1:]:
                            self.c_state = j(self.c_state)
                    self.psdg.update_prods(self.c_state)
                    self.states.append(copy.deepcopy(self.c_state))
                    plan.append(i)
            return plan
        return []

    def sample_plans(
        self, n_samples=1, output_options=None, start=None, depth=None
    ):
        state_seqs = []
        plans = []
        if start is not None:
            start = Nonterminal(start)
        for i in range(n_samples):
            self.states = [self.i_state]
            plans.append(self._sample_plan(start,depth))
            state_seqs.append(self.states)
            self.c_state = copy.deepcopy(self.i_state)
            self.psdg.update_prods(self.c_state)

        if output_options == "a":
            return plans
        if output_options == "s":
            return state_seqs
        if output_options == "as":
            return tuple(zip(plans, state_seqs))

        plan_traces = []
        for i, j in enumerate(state_seqs):
            trace = [None] * (len(j) + len(plans[i]))
            trace[::2] = j
            trace[1::2] = plans[i]
            plan_traces.append(trace)
        return plan_traces

