import argparse
import json
import os

import paho.mqtt.client as mqtt


class TrialMonitor:

    def __init__(self, mqtt_host_address: str, mqtt_host_port: int, experiment_dir: str):
        self._mqtt_host_address = mqtt_host_address
        self._mqtt_host_port = mqtt_host_port
        self._trial_info_filepath = f"{experiment_dir}/trial_info.json"

        self.trial_info = {
            "id": [],
            "number": []
        }

    def load_trial_info_from_file(self):
        if os.path.exists(self._trial_info_filepath):
            with open(self._trial_info_filepath, "r") as f:
                self.trial_info = json.load(f)

    def write_trial_info_to_file(self):
        with open(self._trial_info_filepath, "w") as f:
            json.dump(self.trial_info, f)

    @staticmethod
    def on_message(client, trial_monitor, message):
        json_message = json.loads(message.payload)

        if message.topic == "trial":
            if json_message["msg"]["sub_type"] == "start":
                # Load to guarantee the list contains data from previous trials.
                trial_monitor.load_trial_info_from_file()
                trial_monitor.trial_info["id"].append(json_message["msg"]["trial_id"])
                trial_monitor.trial_info["number"].append(json_message["data"]["trial_number"])
            else:
                # Writing to a file at the end of every trial to guarantee we don't lose data if the subsequent trials
                # break for any reason.
                trial_monitor.write_trial_info_to_file()

    def start(self):
        client = mqtt.Client(userdata=self)
        client.connect(self._mqtt_host_address, port=self._mqtt_host_port)
        client.subscribe("trial")
        client.on_message = TrialMonitor.on_message
        client.loop_forever()


if __name__ == "__main__":
    parser = argparse.ArgumentParser("Watches for trial START and STOP.")
    parser.add_argument("--address", default="localhost", help="Address of the message broker.")
    parser.add_argument("--port", default=1883, help="Port of the message broker.")
    parser.add_argument("--exp_dir", required=True,
                        help="Directory where the file containing info about the trials must be saved.")

    args = parser.parse_args()
    monitor = TrialMonitor(args.address, int(args.port), args.exp_dir)
    monitor.start()
