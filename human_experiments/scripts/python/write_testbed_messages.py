#!/usr/bin/env python3

# Author: Adarsh Pyarelal (adarsh@arizona.edu)
# With contributions from:
# - Prakash Manghwani
# - Jeffrey Rye

""" ELKless Replayer is a simple program to replay messages from a file
containing messages collected during a TA3 experimental trial and dumped from
an Elasticsearch database.

For each line in the input file, it extracts the JSON-serialized message and
the topic it was published to, and republishes the message to the same
topic."""

# To see the command line arguments and invocation pattern, run
#     ./elkless_replayer -h

import json
from dateutil.parser import parse
import argparse
import paho.mqtt.client as mqtt
from datetime import datetime


class MessageWriter:

    def __init__(self, host, port, metadata_filepath):
        self._host = host
        self._port = port
        self._metadata_file = open(metadata_filepath, "a")

    @staticmethod
    def on_message(client, metadata_file, message):
        json_message = json.loads(message.payload)

        # Add the topic and current timestamp to the message
        json_message["topic"] = message.topic
        json_message["@timestamp"] = datetime.utcnow().isoformat() + "Z"

        metadata_file.write(json.dumps(json_message) + "\n")

    def start(self):
        client = mqtt.Client(userdata=self._metadata_file)
        client.connect(self._host, port=self._port)
        client.subscribe("#")
        client.on_message = MessageWriter.on_message
        client.loop_forever()

    def __del__(self):
        self._metadata_file.close()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "-m",
        "--host",
        help="Host that the mosquitto message broker is running on.",
        default="localhost",
    )
    parser.add_argument(
        "-p",
        "--port",
        type=int,
        help="Port that the mosquitto message broker is running on.",
        default=1883,
    )
    parser.add_argument(
        "-f",
        "--filepath",
        help="File where the messages must be saved to."
    )
    args = parser.parse_args()

    MessageWriter(args.host, args.port, args.filepath).start()
