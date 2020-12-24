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

    def update_prods(self, state: Dict) -> None:
        for i in self._productions:
            i.update_prob(state)
