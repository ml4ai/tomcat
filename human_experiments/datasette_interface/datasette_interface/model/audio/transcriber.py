from typing import Any

import numpy as np
import torch
import whisper
from pydub.audio_segment import AudioSegment


class Transcriber:
    def transcribe(self, audio_segment: AudioSegment) -> Any:
        raise NotImplementedError


class Whisper(Transcriber):
    def __init__(self, model_name: str = "base.en"):
        self._model = whisper.load_model(model_name, download_root="cache")

    def transcribe(self, audio_segment: AudioSegment) -> Any:
        audio_segment = audio_segment.set_channels(1).set_frame_rate(16000)

        # We need to normalize the entries as it is done when we provide an audio file and let
        # Whisper load the audio.
        # Ref: https://github.com/openai/whisper/blob/9f70a352f9f8630ab3aa0d06af5cb9532bd8c21d/
        # whisper/audio.py#L49
        model_input = (
            torch.from_numpy(np.array(audio_segment.get_array_of_samples(), float))
            / 32768.0
        )
        model_input = model_input.to(torch.float32)
        return self._model.transcribe(model_input, language="en")
