import time
import numpy as np

def float32_array_to_int16_array(array: bytes):
    """This function takes as input a (binary) array of 32-bit floating point
    numbers, decodes them into a numpy array, and converts the floats to 16-bit
    integers. This is done in order to preserve compatibility with the Google
    Cloud Speech API, which accepts a limited set of encodings. Specifically,
    we opt for the LINEAR16 encoding."""

    # Decode bytes to numpy array.
    input_buffer = np.frombuffer(array, dtype=np.float32)

    # Convert 32-bit floats to 16-bit integers.
    chunk = np.rint(32767 * input_buffer).astype(np.int16).tobytes()
    return chunk

def get_current_time() -> int:
    """Returns current time in milliseconds."""

    return int(round(time.time() * 1000))
