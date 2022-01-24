import queue
from utils import get_current_time

GOOGLE_STREAMING_LIMIT = 240000  # 4 minutes

class AudioStream(object):
    """Opens a stream as a generator that yields audio chunks."""

    def __init__(self):
        # Create a thread-safe buffer of audio data
        self._buff = queue.Queue()
        self.closed = True
        self.start_time = get_current_time()
        self.audio_input = []
        self.last_audio_input = []
        self.result_end_time = 0
        self.is_final_end_time = 0
        self.final_request_end_time = 0
        self.bridging_offset = 0
        self.new_stream = True
        self.restart_counter = 0

    def __enter__(self):
        self.closed = False
        return self

    def __exit__(self, type, value, traceback):
        self.closed = True
        # Signal the generator to terminate so that the client's
        # streaming_recognize method will not block the process termination.
        self._buff.put(None)

    def fill_buffer(self, in_data):
        """Continuously collect data from the audio stream, into the buffer."""
        self._buff.put(in_data)

    def generator(self):
        while not self.closed:

            data = []

            # If the time limit was reached in the previous iteration of this
            # loop, then the new_stream property will be true. In this case, we
            # try to prepend the audio chunks that were missed in between the
            # requests to the list of chunks for the new request.
            if self.new_stream and self.last_audio_input:

                chunk_time = GOOGLE_STREAMING_LIMIT / len(
                    self.last_audio_input
                )

                if chunk_time != 0:
                    # We constrain the bridging offset to be between 0 and
                    # self.final_request_end_time.
                    self.bridging_offset = min(
                        self.final_request_end_time,
                        max(0, self.bridging_offset),
                    )

                    # Calculate the number of chunks (to use as a starting
                    # index offset)
                    chunks_from_ms = round(
                        (self.final_request_end_time - self.bridging_offset)
                        / chunk_time
                    )

                    # Set the new bridging offset
                    self.bridging_offset = round(
                        (len(self.last_audio_input) - chunks_from_ms)
                        * chunk_time
                    )

                    for i in range(chunks_from_ms, len(self.last_audio_input)):
                        data.append(self.last_audio_input[i])

                self.new_stream = False

            # Use a blocking get() to ensure there's at least one chunk of
            # data, and stop iteration if the chunk is None, indicating the
            # end of the audio stream.
            chunk = self._buff.get()
            self.audio_input.append(chunk)

            if chunk is None:
                return

            data.append(chunk)

            # Now consume whatever other data's still buffered.
            while True:
                try:
                    chunk = self._buff.get(block=False)

                    if chunk is None:
                        return

                    data.append(chunk)
                    self.audio_input.append(chunk)

                except queue.Empty:
                    break

            yield b"".join(data)
