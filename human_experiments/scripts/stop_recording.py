import socket
import time
import sys

"""Script to stop LabRecorder recording data."""

# Send LabRecorder API stop recording command to each instance running on CAT
PORT = 22349  # single lab recorder instance with all the streams
s = socket.create_connection(("localhost", PORT))
s.sendall(b"stop\n")
s.close()

# Must give LabRecorder instances time to switch to "stop recording mode" and
# properly close their XDF files.
time.sleep(10.0)

sys.exit("All LabRecorder recordings stopped (Lion, Tiger, Leopard and Baseline Tasks)!")
