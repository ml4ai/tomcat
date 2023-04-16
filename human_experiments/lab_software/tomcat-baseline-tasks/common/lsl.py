from __future__ import annotations

from pylsl import StreamInfo, StreamOutlet, IRREGULAR_RATE

# We wait the number of seconds defined below after creating an LSL Stream such that LabRecorder has time to
# detect the stream is online and gets ready to save data.
TIMEOUT_LSL_STREAM_WAIT_FOR_CONSUMER = 30


class LSLStringStream:

    def __init__(self, name: str, source_id: str, stream_type: str):
        self._stream_info = StreamInfo(name=name,
                                       type=stream_type,
                                       channel_count=1,
                                       nominal_srate=IRREGULAR_RATE,
                                       channel_format="string",
                                       source_id=source_id)

        self.outlet = StreamOutlet(self._stream_info)

        print(f"Stream {name} is online. Waiting up to {TIMEOUT_LSL_STREAM_WAIT_FOR_CONSUMER} seconds for consumers.")
        if self.outlet.wait_for_consumers(TIMEOUT_LSL_STREAM_WAIT_FOR_CONSUMER):
            print(f"Consumer Detected.")
        else:
            print(f"Consumer undetected after timeout. Some data might be lost in the final .xdf file.")

    def send(self, sample: str):
        self.outlet.push_sample([sample])
