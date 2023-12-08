import logging
import os
import subprocess

from pydub import AudioSegment
from tqdm import tqdm

from audio.entity.praat_annotation import PraatAnnotation
from audio.entity.transcriber import Transcriber


class PCMAudio:
    def __init__(self, filepath: str):
        self.filepath = filepath

    def fix_header(self, out_filepath: str):
        """
        Fill header fields that depend on the final size of the audio file.

        Reference: http://soundfile.sapp.org/doc/WaveFormat/
        """

        file_size = os.path.getsize(self.filepath)
        chunksize = (
            file_size - 8
        )  # file size in bytes - 8 bytes of header (ChunkID and ChunkSize)
        chunksize = chunksize.to_bytes(4, "little")

        subchunk2size = file_size - 44  # file size in bytes - 44 bytes of header
        subchunk2size_in_bytes = subchunk2size.to_bytes(4, "little")

        with open(self.filepath, "rb") as input_file, open(
            out_filepath, "wb"
        ) as output_file:
            input_data = input_file.read()
            output_file.write(input_data)

            # Write chunksize at 4th byte
            output_file.seek(4)
            output_file.write(chunksize)

            # Write subchunk2size at 40th byte
            output_file.seek(40)
            output_file.write(subchunk2size_in_bytes)

    def extract_vocalic_features(self, out_filepath: str):
        """
        We use shell execution to generate the vocalics. There is a Python wrapper but I could not make it produce
        a csv file with the same columns so I opted for the CLI solution. Also, this does not change the OpenSmile
        input and output in the config files.
        """
        command = f"SMILExtract -C opensmile/is09-13/IS13_ComParE.conf -I {self.filepath} -D {out_filepath}"

        logs = logging.getLoggerClass().root.handlers[0].baseFilename
        with open(logs, "a") as log_file:
            if (
                subprocess.call(
                    command, shell=True, stdout=log_file, stderr=subprocess.STDOUT
                )
                != 0
            ):
                logging.error(
                    f"Error extracting vocalic features from {self.filepath}."
                )

    def transcribe_annotated_utterances(
        self, transcriber: Transcriber, annotation: PraatAnnotation
    ):
        annotation.reset_transcript_tier()

        full_audio = AudioSegment.from_wav(self.filepath)
        intervals = list(annotation.sound_intervals)
        for index, start_frame, end_frame in tqdm(intervals, position=2, leave=False):
            lb = int(start_frame * full_audio.frame_rate)
            ub = int(end_frame * full_audio.frame_rate)
            audio_segment = full_audio.get_sample_slice(lb, ub)
            result = transcriber.transcribe(audio_segment)

            # Remove double quotes not to break the annotation, whitespaces in the extremities and capitalize the
            # first letter.
            text = result["text"].replace('"', "").strip()
            if len(text) > 0:
                text = text[0].upper() + text[1:]
            annotation.set_transcript(index, text)
