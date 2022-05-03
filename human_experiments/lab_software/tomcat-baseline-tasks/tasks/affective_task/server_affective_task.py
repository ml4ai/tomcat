import csv
import json
import os
from time import sleep, time, monotonic
import psutil

from common import record_metadata, request_clients_end
from network import receive, send

from .config_affective_task import (BLANK_SCREEN_MILLISECONDS,
                                    CROSS_SCREEN_MILLISECONDS,
                                    DISCUSSION_TIMER, INDIVIDUAL_IMAGE_TIMER,
                                    INDIVIDUAL_RATING_TIMER, TEAM_IMAGE_TIMER,
                                    TEAM_RATING_TIMER)
from .utils import get_image_paths


class ServerAffectiveTask:
    def __init__(self, 
                 to_client_connections: list,
                 from_client_connections: dict,
                 session_name: str = '',
                 data_save_path: str = '') -> None:
        self._to_client_connections = to_client_connections
        self._from_client_connections = from_client_connections

        data_path = data_save_path + "/affective"

        csv_file_name = data_path + '/' + session_name + '_' + str(int(time()))

        self._csv_file = open(csv_file_name + ".csv", 'w', newline='')
        header = ['time', 'monotonic_time', 'boot_time', 'image_path', 'subject_id', 'rating']
        self._csv_writer = csv.DictWriter(self._csv_file, delimiter=';', fieldnames = header)
        self._csv_writer.writeheader()
        
        metadata = {}
        metadata["blank_screen_milliseconds"] = BLANK_SCREEN_MILLISECONDS
        metadata["cross_screen_milliseconds"] = CROSS_SCREEN_MILLISECONDS
        metadata["individual_image_timer"] = INDIVIDUAL_IMAGE_TIMER
        metadata["individual_rating_timer"] = INDIVIDUAL_RATING_TIMER
        metadata["team_image_timer"] = TEAM_IMAGE_TIMER
        metadata["team_discussion_timer"] = DISCUSSION_TIMER
        metadata["team_rating_timer"] = TEAM_RATING_TIMER

        json_file_name = csv_file_name + "_metadata"

        record_metadata(json_file_name, metadata)

    def run(self, images_dir: str, collaboration: bool = False):
        # Extract images
        image_paths = sorted(get_image_paths(images_dir))
        if collaboration:
            image_paths = [path for path in image_paths if "Team" in path]
        else:
            image_paths = [path for path in image_paths if "Indivijual" in path]

        data = {}
        data["type"] = "state"
        data["state"] = {}

        if collaboration:
            data["state"]["image_timer"] = TEAM_IMAGE_TIMER
            data["state"]["discussion_timer"] = DISCUSSION_TIMER
            data["state"]["rating_timer"] = TEAM_RATING_TIMER
        else:
            data["state"]["image_timer"] = INDIVIDUAL_IMAGE_TIMER
            data["state"]["rating_timer"] = INDIVIDUAL_RATING_TIMER

        print("[STATUS] Running affective task")

        selected_rating_participant = 0 # cycling through participant during collaboration

        for image_path in image_paths:
            data["state"]["image_path"] = image_path

            selected_rating_participant %= len(self._to_client_connections)

            for i, to_client_connection in enumerate(self._to_client_connections):
                if i == selected_rating_participant:
                    data["state"]["selected"] = True
                else:
                    data["state"]["selected"] = False
                send([to_client_connection], data)

            while(True):
                responses = receive(self._from_client_connections)
                response = list(responses.values())[0]
                client_name = list(responses.keys())[0]
                if response["type"] == "rating":
                    break
                else:
                    if response["type"] == "update":
                        record_activity = {
                            "selected_rating_type": response["update"]["rating_type"],
                            "selected_rating": response["update"]["rating_index"] - 2
                        }
                        self._csv_writer.writerow({"time" : time(), "monotonic_time" : monotonic(), 
                                                "boot_time" : psutil.boot_time(), "image_path" : image_path, 
                                                "subject_id" : client_name, "rating" : json.dumps(record_activity)})

                    # forward response to other clients
                    for i, to_client_connection in enumerate(self._to_client_connections):
                        if i != selected_rating_participant:
                            send([to_client_connection], response)

            # record clients' responses
            current_time = time()
            monotonic_time = monotonic()
            boot_time = psutil.boot_time() #time since last reboot
            for client_name, response in responses.items():
                if response["type"] == "rating":
                    self._csv_writer.writerow({"time" : current_time, "monotonic_time" : monotonic_time, 
                                                "boot_time" : boot_time, "image_path" : image_path, 
                                                "subject_id" : client_name, "rating" : json.dumps(response["rating"])})
                else:
                    raise RuntimeError("Cannot handle message type: " + response["type"])

                selected_rating_participant += 1

            # wait for the client timers to finish before sending the next image
            sleep(0.1)

        request_clients_end(self._to_client_connections)

        print("[STATUS] Affective task ended")

    def close_file(self):
        self._csv_file.close()
