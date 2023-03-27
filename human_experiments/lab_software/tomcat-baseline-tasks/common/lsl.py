from __future__ import annotations

from typing import Any, Dict

import json
from pylsl import StreamInfo, StreamOutlet, IRREGULAR_RATE


class PersistentStreamOutlet(StreamOutlet):
    pass

    # def __del__(self):
    #     pass
    #
    # def force_deletion(self):
    #     super().__del__()


class LSLStringStream:

    def __init__(self, name: str, source_id: str, stream_type: str):
        self._stream_info = StreamInfo(name=name,
                                       type=stream_type,
                                       channel_count=1,
                                       nominal_srate=IRREGULAR_RATE,
                                       channel_format="string",
                                       source_id=source_id)

        self.outlet = PersistentStreamOutlet(self._stream_info)

    def send(self, sample: str):
        self.outlet.push_sample([sample])

    # def force_deletion(self):
    #     self.outlet.force_deletion()
