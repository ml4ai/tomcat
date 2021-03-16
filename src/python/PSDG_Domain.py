from nltk import Nonterminal
from PSDG import PSDG
from PSDProduction import PSDProduction
import sys
import random
import copy
import itertools
from typing import List, Set, Dict, Tuple, Optional, Callable


class PSDG_Domain:
    def __init__(
        self, methods: List["PSDG_Method"], actions: List["PSDG_Action"]
    ) -> None:
        self.methods = methods
        self.actions = actions

    def initialize_planning(self, i_state: Dict) -> None:
        self.i_state = i_state
        self.c_state = copy.deepcopy(i_state)
        self.states = []
        prods = []
        for i in self.methods:
            subtasks = []
            for j in i.subtasks:
                if j[0] == "!":
                    subtasks.append(j)
                else:
                    subtasks.append(Nonterminal(j))
            prods.append(
                PSDProduction(
                    Nonterminal(i.task), subtasks, i_state, i.preconditions,
                )
            )
        self.psdg = PSDG(Nonterminal("P"), prods)

    def _sample_plan(
        self, symbol: Optional[Nonterminal] = None, depth: Optional[int] = None
    ) -> List[str]:
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
                    plan = plan + self._sample_plan(i, depth - 1)
                else:
                    for j in self.actions:
                        if j.__name__ == i:
                            self.c_state = j.applyEffects(self.c_state)
                    self.psdg.update_prods(self.c_state)
                    self.states.append(copy.deepcopy(self.c_state))
                    plan.append(i)
            return plan
        return []

    def sample_plans(
        self,
        n_samples: int = 1,
        output_options: Optional[str] = None,
        start: Optional[str] = None,
        depth: Optional[int] = None,
    ):
        state_seqs = []
        plans = []
        if start is not None:
            start = Nonterminal(start)
        for i in range(n_samples):
            self.states = [self.i_state]
            plans.append(self._sample_plan(start, depth))
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


class PSDG_Action:
    def __init__(
        self,
        name: str,
        effects: Callable[[Dict], Dict],
        trans_prob: Callable[[Dict, Dict], float],
    ):
        self.__name__ = name
        self._effects = effects
        self._trans_prob = trans_prob

    def applyEffects(self, state: Dict):
        return self._effects(state)

    def state_trans_prob(self, state_0: Dict, state_1: Dict):
        return self._trans_prob(state_0, state_1)


class PSDG_Method:
    def __init__(
        self,
        task: str,
        preconditions: Callable[[Dict], float],
        subtasks: List[str],
    ):
        self.task = task
        self.preconditions = preconditions
        self.subtasks = subtasks
