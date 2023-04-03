from typing import Any

import argparse
import time
import zmq

DEFAULT_PUPIL_PORT = 50020
START_RECORDING = "R"
STOP_RECORDING = "r"
TIMESTAMP = "t"


def send_command(conn: Any, command: str) -> str:
    try:
        conn.send_string(command)
        time.sleep(2)
        conn.recv_string(zmq.NOBLOCK)
    except zmq.ZMQError:
        return "error"

    return "ok"


def is_alive(conn: Any) -> bool:
    return send_command(conn, TIMESTAMP) == "ok"


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Controls Pupil Capture remotely via its API.')
    parser.add_argument("--action", type=str, choices=["start", "stop"], help="Action to be performed.")
    parser.add_argument("--host", type=str, default="localhost",
                        help="Address of the machine where Pupil Capture is running")

    args = parser.parse_args()

    ctx = zmq.Context()
    pupil_remote = zmq.Socket(ctx, zmq.REQ)
    pupil_remote.connect(f"tcp://{args.host}:{DEFAULT_PUPIL_PORT}")

    if is_alive(pupil_remote):
        if args.action == "start":
            if send_command(pupil_remote, START_RECORDING) == "error":
                raise "Could not start recording."
        elif args.action == "stop":
            if send_command(pupil_remote, STOP_RECORDING) == "error":
                raise "Could not stop recording."
    else:
        raise "Pupil Capture seems to be offline."
