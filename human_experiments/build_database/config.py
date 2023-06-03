import sys
import logging

DB_PATH = "/space/adarsh/tomcat/test.db"
LOG_FILE_PATH = "/space/adarsh/tomcat/build_database.log"
FILE_HANDLER = logging.FileHandler(filename=LOG_FILE_PATH, mode="w")
STDERR_HANDLER = logging.StreamHandler(stream=sys.stderr)
logging_handlers = [FILE_HANDLER, STDERR_HANDLER]
