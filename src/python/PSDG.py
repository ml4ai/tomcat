from nltk import PCFG
from PSDProduction import PSDProduction
from nltk import Nonterminal
from typing import List, Set, Dict, Tuple, Optional, Callable


class PSDG(PCFG):
    def __init__(
        self,
        start: Nonterminal,
        productions: List[PSDProduction],
        calculate_leftcorners: bool = True,
    ) -> None:
        PCFG.__init__(self, start, productions, calculate_leftcorners)

        terminals = set()
        nonterminals = set()

        for i in self.productions():
            nonterminals.add(i.lhs())
            for k in i.rhs():
                if not(isinstance(k,Nonterminal)):
                    terminals.add(k)

        self.terminals = list(terminals)
        self.nonterminals = list(nonterminals)

    def update_prods(self, state: Dict) -> None:
        for i in self._productions:
            i.update_prob(state)
