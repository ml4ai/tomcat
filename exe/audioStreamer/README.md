## Disable ClientMap audio streaming:
When using the audioStreamer with the Testbed, the ClientMap audio streaming functionality must be disabled to avoid interference. Make the following modification to the file testbed/local/AsistDataIngester/appsettings.json:

`"DisableSpeechToText":  true` -> `"DisableSpeechToText":  false`

## Install dependencies:
1. libsndfile
2. portaudio
3. boost
4. nlohmann-json
5. paho.mqtt.cpp

### MacOS
All dependencies are available through MacPorts. They can be installed with the command:
`sudo port install libsndfile portaudio boost nlohmann-json paho.mqtt.cpp`

## Build audioStreamer
```bash
mkdir build
cd build
cmake ..
make 
```

## Expose port in ASR_Agent
If running the ASR_Agent with Docker, you may need to expose port 8888 so that the audioStreamer can connect to it. Add the following to the asr_agent service in the docker-compose file for the ASR_Agent: 

```text
ports:
  - 8888:8888
```

	

## Run audioStreamer 
### Immediate mode
```bash
./audioStreamer --player_name Player100
``` 
### MQTT mode
```bash
./audioStreamer --use_mqtt true --mqtt_host  localhost --mqtt_port 1883 --player_name Player100
```

