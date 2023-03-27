import json
from typing import Any, Dict

from csv import DictWriter
from common.lsl import LSLStringStream


class Writer:

    def __init__(self, csv_writer: DictWriter, lsl_writer: LSLStringStream):
        self._csv_writer = csv_writer
        self._lsl_writer = lsl_writer

    def write_header(self):
        self._csv_writer.writeheader()
        # No need to send the header to LSL

    def write(self, content: Dict[str, Any]):
        self._csv_writer.writerow(content)

        try:
            if self._lsl_writer is not None:
                self._lsl_writer.send(json.dumps(content))
        except Exception as ex:
            # Do not crash the program if we have an issue with LSL
            print(f"Could not send data to LSL. Error {ex}.")

    def close(self):
        del self._lsl_writer
