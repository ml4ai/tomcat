from nltk import Production, ProbabilisticMixIn


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

    def __init__(self, lhs, rhs, current_s, precond):
        self.precond = precond
        p = self.precond(current_s)
        ProbabilisticMixIn.__init__(self, prob=p)
        Production.__init__(self, lhs, rhs)

    def __str__(self):
        return super().__str__() + (
            " [1.0]" if (self.prob() == 1.0) else " [%g]" % self.prob()
        )

    def __eq__(self, other):
        return (
            type(self) == type(other)
            and self._lhs == other._lhs
            and self._rhs == other._rhs
            and self.prob() == other.prob()
        )

    def __ne__(self, other):
        return not self == other

    def __hash__(self):
        return hash((self._lhs, self._rhs, self.prob()))

    def update_prob(self, state):
        p = self.precond(state)
        self.set_prob(p)
