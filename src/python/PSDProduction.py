from nltk import Production, ProbabilisticMixIn
from nltk import Nonterminal
from typing import Union, List, Set, Dict, Tuple, Optional, Callable


class PSDProduction(Production, ProbabilisticMixIn):
    """
    Edited version of nltk's ProbablisticProduction class
    See http://www.nltk.org/_modules/nltk/grammar.html#ProbabilisticProduction

    This was edited to contain a precondition function and to be able to update
    the probability of production given a new state. The probability passed into
    the constructor represents the probability of the production if the passed
    state satisfies the preconditions.


    :see also: ``Production``
    """

    def __init__(
        self,
        lhs: Nonterminal,
        rhs: List[Union[Nonterminal, str]],
        current_s: Dict,
        precond: Callable[[Dict], Dict],
    ) -> None:
        self.precond = precond
        p = self.precond(current_s)
        ProbabilisticMixIn.__init__(self, prob=p)
        Production.__init__(self, lhs, rhs)

    def __str__(self, omit_probs: bool = False) -> str:
        if omit_probs:
            return super().__str__()
        return super().__str__() + (
            " [1.0]" if (self.prob() == 1.0) else " [%g]" % self.prob()
        )

    def __eq__(self, other: 'PSDProduction') -> bool:
        return (
            type(self) == type(other)
            and self._lhs == other._lhs
            and self._rhs == other._rhs
            and self.prob() == other.prob()
        )

    def __ne__(self, other: 'PSDProduction')  -> bool:
        return not self == other

    def __hash__(self) -> int:
        return hash((self._lhs, self._rhs, self.prob()))

    def update_prob(self, state: Dict) -> None:
        p = self.precond(state)
        self.set_prob(p)
