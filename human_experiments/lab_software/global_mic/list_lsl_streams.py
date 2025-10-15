import pylsl

# Resolve all streams on the network
streams = pylsl.resolve_streams()

# Print the name and type of each stream
if not streams:
    print("No LSL streams found on the network.")
else:
    print("Found LSL streams:")
    for stream in streams:
        info = stream.info()
        print(f"  Name: {info.name()}, Type: {info.type()}, Source ID: {info.source_id()}")
