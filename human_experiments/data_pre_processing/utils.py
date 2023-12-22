import contextlib
import os


@contextlib.contextmanager
def cd(path):
    old_path = os.getcwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(old_path)


def is_directory_with_unified_xdf_files(session):
    year, month, day, hour = [int(x) for x in session.split("_")[1:]]
    return (year, month) >= (2023, 4)
