import sounddevice as sd
import numpy as np
import pylsl as lsl
import time

# --- LSL Stream Configuration ---
stream_name = 'USBAudioStream'
stream_type = 'Audio'
n_channels = 1  # Mono microphone
srate = 44100   # Sample rate (adjust as needed for your mic)
channel_format = lsl.cf_float32  # Data type for audio samples

# --- Audio Input Configuration ---
# device_id = None  # Set to None to use default input, or specify device ID
""" rchamplin@cat:~$ usb-devices

T:  Bus=05 Lev=02 Prnt=02 Port=03 Cnt=01 Dev#=  3 Spd=12  MxCh= 0
D:  Ver= 2.00 Cls=00(>ifc ) Sub=00 Prot=00 MxPS=64 #Cfgs=  1
P:  Vendor=2886 ProdID=0018 Rev=03.00
S:  Manufacturer=SEEED
S:  Product=ReSpeaker 4 Mic Array (UAC1.0)
C:  #Ifs= 5 Cfg#= 1 Atr=80 MxPwr=500mA
I:  If#=0x0 Alt= 0 #EPs= 0 Cls=01(audio) Sub=01 Prot=00 Driver=snd-usb-audio
I:  If#=0x1 Alt= 0 #EPs= 0 Cls=01(audio) Sub=02 Prot=00 Driver=snd-usb-audio
I:  If#=0x2 Alt= 0 #EPs= 0 Cls=01(audio) Sub=02 Prot=00 Driver=snd-usb-audio
I:  If#=0x3 Alt= 0 #EPs= 0 Cls=ff(vend.) Sub=ff Prot=ff Driver=(none)
I:  If#=0x4 Alt= 0 #EPs= 0 Cls=fe(app. ) Sub=01 Prot=01 Driver=(none)
"""
device_id = '2886:0018'  # Bus 005 Device 003: ID 2886:0018
chunk_size = 1024 # Number of frames per LSL chunk

def audio_callback(indata, frames, time_info, status):
    """This function is called for each audio chunk."""
    if status:
        print(status)
    
    # Push the audio data to the LSL outlet
    outlet.push_chunk(indata.flatten())

try:
    # --- LSL Setup ---
    info = lsl.StreamInfo(stream_name, stream_type, n_channels, srate, channel_format, 'myuid12345')
    outlet = lsl.StreamOutlet(info)
    print(f"LSL stream '{stream_name}' created.")

    # --- Sounddevice Setup ---
    print("Available audio devices:")
    print(sd.query_devices())

    if device_id is None:
        # Use the default input device
        input_device_info = sd.query_devices(kind='input')
        print(f"Using default input device: {input_device_info['name']}")
    else:
        # Use a specific device by ID
        input_device_info = sd.query_devices(device_id, kind='input')
        print(f"Using specified input device: {input_device_info['name']}")

    # Open the audio stream
    with sd.InputStream(samplerate=srate, channels=n_channels, 
                        dtype='float32', callback=audio_callback, 
                        blocksize=chunk_size, device=device_id):
        print("\nStreaming audio from microphone to LSL. Press Ctrl+C to stop.")
        while True:
            time.sleep(1) # Keep the main thread alive

except KeyboardInterrupt:
    print("\nStopping audio stream and LSL outlet.")
except Exception as e:
    print(f"An error occurred: {e}")
finally:
    # Ensure resources are cleaned up (though 'with' statement handles sounddevice)
    pass