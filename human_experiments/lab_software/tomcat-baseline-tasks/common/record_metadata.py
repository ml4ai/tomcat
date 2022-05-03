import json


def record_metadata(file_name: str, data: dict):
    with open(file_name + ".json", 'w') as json_file:
        json.dump(data, json_file, indent=4)
