import socket
import time
import sys

"""Script to stop LabRecorder recording data."""

# Send LabRecorder API stop recording command to each instance running on CAT
# (Lion on port:22345, Tiger on port:22346, Leopard on port:22347, and TOM (Baseline Tasks) on port:22348)
for port in (22345, 22346, 22347, 22348):
    s = socket.create_connection(("localhost", port))
    s.sendall(b"stop\n")
    s.close()

# Must give LabRecorder instances time to switch to "stop recording mode" and
# properly close their XDF files.
time.sleep(10.0)

sys.exit("All LabRecorder recordings stopped (Lion, Tiger, Leopard and Baseline Tasks)!")
