from textgrids import TextGrid, Tier

from typing import Tuple
from copy import deepcopy
import os

from typing import List


class PraatAnnotation:

    def __init__(self, filepath: str):
        self._grid = TextGrid(filepath)

    @property
    def sound_intervals(self, standardized: bool = False) -> Tuple[float, float]:
        tier_name = "standardized_silences" if standardized else "silences"
        for index, sound_period in enumerate(self._grid[tier_name]):
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
        filename, file_extension = os.path.splitext(out_filepath)
        self._grid.write(filename + ".TextGrid")

    def standardize_silences(self, silence_threshold: float):
        merged_silences = self._merge_silences(ref_tier=self._grid["silences"])
        merged_sounds = self._merge_sounds(ref_tier=merged_silences, silence_threshold=silence_threshold)
        self._grid["standardized_silences"] = merged_sounds

    @staticmethod
    def _merge_silences(ref_tier: Tier):
        tier = deepcopy(ref_tier)
        num_intervals = len(tier)

        merged_silences = []
        i = 0
        while i < num_intervals:
            interval = tier[i]

            if interval.text in ["n", ""]:
                # Look ahead and merge subsequent silence periods
                j = i + 1
                while j < num_intervals and tier[j].text in ["n", ""]:
                    interval.xmax = tier[j].xmax
                    j += 1

                merged_silences.append(interval)

                i = j
                continue
            else:
                merged_silences.append(interval)

            i += 1

        return Tier(data=merged_silences)

    @staticmethod
    def _merge_sounds(ref_tier: Tier, silence_threshold: float):
        tier = deepcopy(ref_tier)
        num_intervals = len(tier)

        merged_sounds = []
        i = 0
        while i < num_intervals:
            interval = tier[i]

            if interval.text == "s":
                # Look ahead for the next "s" one. Merge if the duration between them is at most the threshold.
                j = i + 1
                intervals_to_merge = [interval]
                while j < num_intervals and tier[j].text != "s":
                    intervals_to_merge.append(tier[j])
                    j += 1

                if j == num_intervals or tier[j].xmin - interval.xmax > silence_threshold:
                    # No merge necessary
                    merged_sounds.extend(intervals_to_merge)
                else:
                    # Merge intervals by making the start time of the last interval be the start time of the first
                    # interval to merge.
                    tier[j].xmin = interval.xmin

                i = j
                continue
            else:
                merged_sounds.append(interval)

            i += 1

        return Tier(data=merged_sounds)
