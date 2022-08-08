import socket
import subprocess, os
import time
import sys

s1 = socket.create_connection(("localhost", 22345))
s1.sendall(b"stop\n")
s1.close()

s2 = socket.create_connection(("localhost", 22346))
s2.sendall(b"stop\n")
s2.close()

s3 = socket.create_connection(("localhost", 22347))
s3.sendall(b"stop\n")
s3.close()

time.sleep(10.0)

sys.exit("All LabRecorder Recordings Stopped (Lion, Tiger and Leopard)!")

