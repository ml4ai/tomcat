import socket
import time
import sys

#s1 = socket.create_connection(("localhost", 22345))
#s1.sendall(b"stop\n")
#s1.close()

#s2 = socket.create_connection(("localhost", 22346))
#s2.sendall(b"stop\n")
#s2.close()

#s3 = socket.create_connection(("localhost", 22347))
#s3.sendall(b"stop\n")
#s3.close()

# Send LabRecorder API stop recording command to each intance running on CAT
# (Lion on port:22345, Tiger on port:22346 and Leopard on port:22347)
for port in (22345, 22346, 22347):
    s = socket.create_connection(("localhost", port))
    s.sendall(b"stop\n")
    s.close()

# Must give LabRecorder instances time to switch to "stop recording mode" and
# properly close their "xdf" files.
time.sleep(10.0)

sys.exit("All LabRecorder recordings stopped (Lion, Tiger and Leopard)!")
