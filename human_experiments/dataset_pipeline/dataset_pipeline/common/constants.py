from pathlib import Path

STATIONS = ["lion", "tiger", "leopard"]

# Anchored to the package so vocalics works regardless of the caller's CWD (the
# old bare relative string only resolved when CWD was the project root).
OPENSMILE_CONFIG_DIR = str(Path(__file__).resolve().parent.parent / "asset" / "opensmile")

EEG_FREQUENCY = 500
EEG_NOTCH_FILTER_FREQUENCY = 60
EEG_NOTCH_WIDTH = 2
EEG_TRANSISION_BANDWIDTH = 9

FNIRS_FREQUENCY = 10
FNIRS_LOW_FREQUENCY_THRESHOLD = 0.01
FNIRS_HIGH_FREQUENCY_THRESHOLD = 0.2
FNIRS_BANDPASS_FILTER_METHOD = "iir"

REST_STATE_DURATION_IN_SECONDS = 300
