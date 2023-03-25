from pylsl import StreamInfo, StreamOutlet, IRREGULAR_RATE


class LSLStringStream:

    def __init__(self,
                 task_name: str,
                 source_id: str,
                 sample_rate: int = IRREGULAR_RATE):

        stream_info = StreamInfo(name=task_name,
                                 type="baseline_task",
                                 channel_count=1,
                                 nominal_srate=sample_rate,
                                 channel_format="string",
                                 source_id=f"baseline_task_{source_id}")

        self.outlet = StreamOutlet(stream_info)

    def send(self, sample: str):
        self.outlet.push_sample([sample])
