import os

import pandas as pd
import opensmile
import audiofile
import logging


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

    def extract_vocalic_features(self) -> pd.DataFrame:
        # signal, sampling_rate = audiofile.read(self.filepath)

        smile = opensmile.Smile(
            feature_set="opensmile/is09-13/IS13_ComParE.conf",
            feature_level='lld;lld_de',
            logfile=logging.getLoggerClass().root.handlers[0].baseFilename,
            options={"frameTime": False, "timeFrame": False, "append": False, "quoteStrings": True}
        )
        return smile.process_file(self.filepath)
