from textgrids import TextGrid

from typing import Tuple
from copy import deepcopy

from typing import List


class PraatAnnotation:

    def __init__(self, filepath: str):
        self._grid = TextGrid(filepath)

    @property
    def sound_intervals(self) -> Tuple[float, float]:
        for index, sound_period in enumerate(self._grid["silences"]):
            if sound_period.text == "s":
                yield index, sound_period.xmin, sound_period.xmax

    @property
    def transcripts(self) -> Tuple[float, float]:
        for index, sound_period in enumerate(self._grid["transcripts"]):
            if sound_period.text != "":
                yield index, sound_period.text

    def reset_transcript_tier(self):
        self._grid["transcripts"] = deepcopy(self._grid["silences"])
        for i in range(len(self._grid["transcripts"])):
            self._grid["transcripts"][i].text = ""

    def reset_labels_tier(self):
        self._grid["dialog_labels"] = deepcopy(self._grid["transcripts"])
        for i in range(len(self._grid["dialog_labels"])):
            self._grid["dialog_labels"][i].text = ""

    def set_transcript(self, index: int, text: str):
        self._grid["transcripts"][index].text = text

    def set_labels(self, index: int, labels: List[str]):
        self._grid["dialog_labels"][index].text = ",".join(map(lambda x: x.strip(), labels))

    def save(self, out_filepath: str):
        self._grid.write(out_filepath + ".TextGrid")
