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

    def _sample_from_grammar(self, grammar, actions, start=None, depth=None):
        """
        This is based off of the generate function in nltk for CFG. This uses ancestral
        sampling to generate one possible plan/sentence rather than all possible
        plans/sentences.
        see http://www.nltk.org/_modules/nltk/parse/generate.html#generate

        :param grammar: The Grammar used to generate sentences.
        :param start: The Nonterminal from which to start generate sentences.
        :param depth: The maximal depth of the generated tree.
        :param n: The maximum number of sentences to return.
        :return: An iterator of lists of terminal tokens.
        """
        if not start:
            start = grammar.start()
        if depth is None:
            depth = sys.maxsize

        self.states = [copy.deepcopy(self.c_state)]

        iter = self._sample_all(grammar, actions, [start], depth)

        return iter

    def _sample_all(self, grammar, actions, items, depth):
        if items:
            try:
                for frag1 in self._sample_one(
                    grammar, actions, items[0], depth
                ):
                    for frag2 in self._sample_all(
                        grammar, actions, items[1:], depth
                    ):
                        yield frag1 + frag2
            except RuntimeError as _error:
                if _error.message == "maximum recursion depth exceeded":
                    # Helpful error message while still showing the recursion stack.
                    raise RuntimeError(
                        "The grammar has rule(s) that yield infinite recursion!!"
                    )
                else:
                    raise
        else:
            yield []

    def _sample_one(self, grammar, actions, item, depth):
        if depth > 0:
            if isinstance(item, Nonterminal):
                prod = random.choices(
                    [x for x in grammar.productions(lhs=item)],
                    [x.prob() for x in grammar.productions(lhs=item)],
                )[0]
                for frag in self._sample_all(
                    grammar, actions, prod.rhs(), depth - 1
                ):
                    yield frag
            else:
                for i in actions:
                    if i.__name__ == item[1:]:
                        self.c_state = i(self.c_state)
                grammar.update_prods(self.c_state)
                self.states.append(copy.deepcopy(self.c_state))
                yield [item]

    def sample_plans(
        self, n_samples=1, output_options=None, start=None, depth=None
    ):
        state_seqs = []
        plans = []
        if start is not None:
            start = Nonterminal(start)
        for i in range(n_samples):
            plans.append(
                list(
                    self._sample_from_grammar(
                        self.psdg, self.actions, start, depth
                    )
                )[0]
            )
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
