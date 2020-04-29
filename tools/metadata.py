# takes UUID as a command-line argument and prints out a JSON object to
# standard output which contains: 
#   UUID of session
#   timestamp, using datetime module from python
#   USER ID (from $USER)

import sys
import os
import json
from datetime import datetime

# Return current local date and time in ISO-8601 format.
datetime = datetime.now()
timestamp = datetime.isoformat()
# print("Timestamp:", timestamp)



metadata = {
        "UUID":sys.argv[1],           #set up CommandLine Argument"
        "Timestamp":timestamp,
        "User ID":os.getenv("USER")         
           }

print(json.dumps(metadata, indent=4)) # Serialize metadata

