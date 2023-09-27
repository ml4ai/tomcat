import argparse

from logging import info

from config import NUM_PROCESSES
from reader.data_reader import DataReader


def sync_raw_signals(data_reader: DataReader, frequency: int, num_jobs: int):
    pass




if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="""
            Synchronizes raw signals from a specific modality across participants in a specific frequency. We first,
            resample (typically upsample) the signals and interpolate to generate equally spaced values, which we 
            can then align across participants.           
        """
    )

    parser.add_argument("--modality", type=str, required=True, choices=["eeg", "ekg", "gsr", "fnirs", "gaze"],
                        help="Modality of the raw data.")
    parser.add_argument("--freq", type=int, required=True,
                        help="Desired frequency. Typically higher than the empirical frequency of the raw data.")
    parser.add_argument("--n_jobs", type=int, required=False, default=NUM_PROCESSES,
                        help="Number of jobs for parallel processing.")

    args = parser.parse_args()

    # TODO - Configure log

    sync_raw_signals(args.modality, args.freq, args.n_jobs)
