import argparse
import zmq

DEFAULT_PUPIL_PORT = 50020
START_RECORDING = "R"
STOP_RECORDING = "r"

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Controls Pupil Capture remotely via its API.')
    parser.add_argument("--action", type=str, choices=["start", "stop"], help="Action to be performed.")
    parser.add_argument("--host", type=str, default="localhost",
                        help="Address of the machine where Pupil Capture is running")

    args = parser.parse_args()

    ctx = zmq.Context()
    pupil_remote = zmq.Socket(ctx, zmq.REQ)
    pupil_remote.connect(f"tcp://{args.host}:{DEFAULT_PUPIL_PORT}")

    if args.action == "start":
        pupil_remote.send_string(START_RECORDING)
        if "ok" not in pupil_remote.recv_string(zmq.NOBLOCK).lower():
            raise "Could not start recording."
    elif args.action == "stop":
        pupil_remote.send_string(STOP_RECORDING)
        if "ok" not in pupil_remote.recv_string(zmq.NOBLOCK).lower():
            raise "Could not stop recording."
