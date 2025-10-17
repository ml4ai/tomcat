import pylsl
import wave
import numpy as np
import time

# Configuration
STREAM_TYPE = 'Audio'
STREAM_NAME = 'MyAudioStream' # Replace with your LSL stream name
OUTPUT_FILENAME = 'recorded_audio.wav'
RECORD_DURATION_SECONDS = 10 # Adjust as needed

print(f"Looking for an LSL stream with type '{STREAM_TYPE}' and name '{STREAM_NAME}'...")
streams = pylsl.resolve_byprop('type', STREAM_TYPE, timeout=5)

if not streams:
    print("No matching LSL stream found.")
else:
    inlet = pylsl.StreamInlet(streams[0])
    stream_info = inlet.info()

    # Get stream properties for WAV file header
    n_channels = stream_info.channel_count()
    s_rate = int(stream_info.nominal_srate())
    
    # Assuming float32 data from LSL, convert to int16 for WAV
    # Adjust based on your actual LSL stream format
    samp_width = 2 # 2 bytes for 16-bit PCM

    print(f"Connected to stream: {stream_info.name()} ({n_channels} channels, {s_rate} Hz)")

    frames = []
    start_time = time.time()

    with wave.open(OUTPUT_FILENAME, 'wb') as wf:
        wf.setnchannels(n_channels)
        wf.setsampwidth(samp_width)
        wf.setframerate(s_rate)

        print(f"Recording audio to {OUTPUT_FILENAME} for {RECORD_DURATION_SECONDS} seconds...")
        try:
            while (time.time() - start_time) < RECORD_DURATION_SECONDS:
                sample, timestamp = inlet.pull_sample(timeout=1.0)
                if sample is not None:
                    # Convert float32 LSL data to int16 for WAV
                    # Scale to 16-bit range and convert to bytes
                    audio_data_int16 = np.array(sample, dtype=np.float32) * 32767
                    audio_data_int16 = audio_data_int16.astype(np.int16)
                    wf.writeframes(audio_data_int16.tobytes())
        except KeyboardInterrupt:
            print("Recording stopped by user.")
        finally:
            print(f"Recording finished. Audio saved to {OUTPUT_FILENAME}")
            inlet.close_stream()