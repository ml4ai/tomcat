from nltk import PCFG


class PSDG(PCFG):
    def __init__(self, start, productions, calculate_leftcorners=True):
        PCFG.__init__(self, start, productions, calculate_leftcorners)

    def update_prods(self, state):
        for i in self._productions:
            i.update_prob(state)
