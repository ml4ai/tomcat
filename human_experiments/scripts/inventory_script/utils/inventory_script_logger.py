import sys
import logging


def inventory_script_logging(logger_name, level=logging.DEBUG):
    logger = logging.getLogger(logger_name)
    logger.setLevel(level)
    console_handler = logging.StreamHandler(sys.stdout)
    logger.addHandler(console_handler)
    file_handler = logging.FileHandler(logger_name, mode="a")
    logger.addHandler(file_handler)
    return logger
