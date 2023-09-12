from textgrids import TextGrid

from typing import Tuple
from copy import deepcopy

class PraatAnnotation:

    def __init__(self, filepath: str):
        self._grid = TextGrid(filepath)

    @property
    def sound_intervals(self) -> Tuple[float, float]:
        for index, sound_period in enumerate(self._grid["silences"]):
            if sound_period.text == "s":
                yield index, sound_period.xmin, sound_period.xmax

    def reset_transcript_tier(self):
        self._grid["transcripts"] = deepcopy(self._grid["silences"])
        for i in range(len(self._grid["transcripts"])):
            self._grid["transcripts"][i].text = ""

    def set_transcript(self, index: int, text: str):
        self._grid["transcripts"][index].text = text

    def save(self, out_filepath: str):
        self._grid.write(out_filepath + ".TextGrid")
