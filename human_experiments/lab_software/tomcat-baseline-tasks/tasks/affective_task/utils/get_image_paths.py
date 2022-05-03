from os import listdir
from os.path import isfile, join
from typing import List


def get_image_paths(dir: str) -> List[str]:
    files = []

    for f in listdir(dir):
        file_path = join(dir, f)

        if isfile(file_path):
            files.append(file_path)

    return files
