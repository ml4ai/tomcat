import json
from logging import error
from typing import Any

import requests
from tqdm import tqdm

from audio.entity.praat_annotation import PraatAnnotation


class SentenceLabeler:
    def annotate_labels(self, transcripts_annotation: PraatAnnotation) -> Any:
        raise NotImplementedError


class ToMCATDialogAgent(SentenceLabeler):
    def __init__(self, host: str = "localhost", port: int = 8080):
        self._api_url = f"http://{host}:{port}"
        response = requests.post(self._api_url, data={"message": "status"})

        if response.status_code != 200:
            error(f"Server Error! Request status code {response.status_code}.")

    def annotate_labels(self, annotation: PraatAnnotation):
        annotation.reset_labels_tier()

        transcripts = list(annotation.transcripts)
        for index, text in tqdm(transcripts, position=1, leave=False):
            labels = self._get_labels(text)
            annotation.set_labels(index, labels)

    def _get_labels(self, sentence: str):
        response = requests.post(self._api_url, data={"message": sentence})

        labels = []
        if response.status_code == 200:
            results = json.loads(response.text)
            for res in results:
                labels.extend(res["labels"])
        else:
            error(
                f"Server Error! Request status code {response.status_code}. Sentence: {sentence}."
            )

        return labels
