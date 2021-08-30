#!/usr/bin/env python

# WS server example
import asyncio

import websockets

import json
from uuid import uuid4
import sys
from urllib.parse import urlparse, parse_qs
import datetime
from utils import float32_array_to_int16_array
from audio_stream import AudioStream
from logging import debug, info
import numpy as np
from soundfile import SoundFile

RECORDING_IN_PROGRESS = True
async def consumer_handler(websocket, path):
    # Number of channels. Currently we expect only one channel.
    n_channels = 1
    query_params = parse_qs(urlparse(websocket.path).query)
    print(query_params)
    participant_id = query_params["id"][0]
    if participant_id == "null":
        participant_id = str(uuid4())
    await websocket.send(json.dumps({"participantId": participant_id}))

    info(f"Participant {participant_id} is now connected.")

    sample_rate = int(query_params["sampleRate"][0])
    audio_stream = AudioStream()


    with SoundFile(
        f"recordings/participant_{participant_id}.wav",
        "w",
        sample_rate,
        n_channels,
        "FLOAT",
    ) as f:
        async for data in websocket:

            debug(
                f"Received chunk of size {len(data)} bytes from browser at "
                f"{datetime.datetime.utcnow().isoformat()}Z"
            )
            if RECORDING_IN_PROGRESS:
                f.write(np.frombuffer(data, dtype=np.float32))
            chunk = float32_array_to_int16_array(data)
            audio_stream.fill_buffer(chunk)




start_server = websockets.serve(consumer_handler, "localhost", 8888)
try:
    asyncio.get_event_loop().run_until_complete(start_server)
    asyncio.get_event_loop().run_forever()
except KeyboardInterrupt:
    sys.stderr.write("Websocket Closed. Exiting now.")