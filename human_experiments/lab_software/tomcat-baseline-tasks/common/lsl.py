from pylsl import StreamInfo, StreamOutlet, IRREGULAR_RATE


class LSLStringStream:

    def __init__(self, name: str, source_id: str):
        stream_info = StreamInfo(name=name,
                                 type="task_data",
                                 channel_count=1,
                                 nominal_srate=IRREGULAR_RATE,
                                 channel_format="string",
                                 source_id=source_id)

        self.outlet = StreamOutlet(stream_info)

    def send(self, sample: str):
        self.outlet.push_sample([sample])
