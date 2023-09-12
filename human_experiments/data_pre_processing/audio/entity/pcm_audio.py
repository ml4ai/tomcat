import os

import pandas as pd
import opensmile
import audiofile
import logging

import subprocess


class PCMAudio:

    def __init__(self, filepath: str):
        self.filepath = filepath

    def fix_header(self, out_filepath: str):
        """
        Fill header fields that depend on the final size of the audio file.

        Reference: http://soundfile.sapp.org/doc/WaveFormat/
        """

        file_size = os.path.getsize(self.filepath)
        chunksize = file_size - 8  # file size in bytes - 8 bytes of header (ChunkID and ChunkSize)
        chunksize = chunksize.to_bytes(4, "little")

        subchunk2size = file_size - 44  # file size in bytes - 44 bytes of header
        subchunk2size_in_bytes = subchunk2size.to_bytes(4, "little")

        with open(self.filepath, 'rb') as input_file, open(out_filepath, 'wb') as output_file:
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
            if subprocess.call(command, shell=True, stdout=log_file, stderr=subprocess.STDOUT) != 0:
                logging.error(f"Error extracting vocalic features from {self.filepath}.")
