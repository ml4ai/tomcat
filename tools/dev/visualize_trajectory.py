# Purpose: Overlay the observed trace data from gameplay on the ToMCAT Zombie Invasion map
# Example execution: python plot_observation_trace.py <input-observation-file-path> -o <output-filepath> --animation

import sys
import json
from datetime import datetime

import matplotlib.pyplot as plt
from matplotlib.path import Path
from matplotlib.collections import PathCollection
from matplotlib.animation import FuncAnimation
from tqdm import trange
import argparse


def create_visualization(
    observations_file, output_file, viz_type="animation"
):
    """Generates a plot overlaying user trace data on the SAR V1 map."""

    # Load time and position data from the observation file
    position_times, position_coords = get_time_and_position_data(
        observations_file
    )

    # Create the plot object with a helpful title
    fig = plt.figure(figsize=(9, 8))
    ax = plt.gca()
    ax.set_title("SAR Gameplay Time-based Position Trace")

    # Add arena walls and house objects
    arena_walls = PathCollection(
        [
            Path([(44, 43), (98, 43), (98, 100), (44, 100), (44, 43)]),
            Path([(58, 77), (60, 77), (60, 74), (73, 74), (83, 74), (83, 43)]),
        ],
        edgecolor="black",
        facecolor="None",
        lw=3,
    )

    small_and_large_houses = PathCollection(
        [
            Path([(50, 96), (56, 96), (56, 90), (50, 90), (50, 96)]),
            Path([(45, 49), (49, 49), (49, 44), (45, 44), (45, 49)]),
            Path([(87, 55), (92, 55), (92, 49), (87, 49), (87, 55)]),
            Path(
                [
                    (85, 97),
                    (94, 97),
                    (94, 91),
                    (90, 91),
                    (90, 85),
                    (84, 85),
                    (84, 90),
                    (85, 90),
                    (85, 97),
                ]
            ),
            Path(
                [
                    (73, 74),
                    (83, 74),
                    (83, 63),
                    (78, 63),
                    (78, 69),
                    (73, 69),
                    (73, 74),
                ]
            ),
            Path(
                [
                    (59, 66),
                    (68, 66),
                    (68, 60),
                    (64, 60),
                    (64, 54),
                    (58, 54),
                    (58, 59),
                    (59, 59),
                    (59, 66),
                ]
            ),
        ],
        color=(166 / 255, 89 / 255, 50 / 255, 1.0),
        lw=1,
    )

    ax.add_collection(small_and_large_houses)
    ax.add_collection(arena_walls)

    # Plot the user observation trace with a time legend
    (X, Y) = map(list, zip(*position_coords))
    if viz_type == "plot":
        cax = ax.scatter(
            X, Y, s=1, c=position_times, cmap=plt.get_cmap("viridis")
        )
        cbar = fig.colorbar(cax)
        cbar.ax.set_ylabel("Time in minutes")
        plt.tight_layout()
        plt.savefig(output_file)
    elif viz_type == "animation":
        timestamp = plt.text(80, 101, "time = 0.0")

        (ln,) = plt.plot([], [])

        def init():
            return ln, timestamp

        xdata = []
        ydata = []

        def update(frame):
            xdata.append(X[frame])
            ydata.append(Y[frame])
            ln.set_data(xdata, ydata)
            timestamp.set_text(f"Time = {position_times[frame]}")
            return ln, timestamp

        animation = FuncAnimation(
            fig,
            update,
            frames=trange(len(position_times)),
            interval=1,
            init_func=init,
        )
        animation.save(output_file)
    else:
        raise Exception("viz_type must be one of [plot|animation]")


def get_time_and_position_data(observations_filename):
    """
    Parses the JSON objects and timestamps from an observations file and returns
    a collection of timestamps with a matching collection of positions.
    """
    timestamps, positions = list(), list()
    with open(observations_filename, "r") as infile:
        for line in infile:
            # Split the line into a JSON object and a timestamp string
            split_pt = line.find("{")

            # Recover the (x, y) position data from a JSON object
            obj = json.loads(line[split_pt:])
            positions.append((float(obj["ZPos"]), float(obj["XPos"])))

            # Convert a time string into a measure of seconds
            time_string = line[:split_pt].strip()
            dt_obj = datetime.strptime(time_string, "%Y%m%dT%H%M%S.%f")
            timestamps.append(datetime_to_seconds(dt_obj))

    # Shorten time and position data by the border entry point
    border_pt_idx = find_border_entry(positions, 0)
    timestamps = timestamps[border_pt_idx:]
    start_time = min(timestamps)
    position_times = [(s - start_time) / 60 for s in timestamps]
    position_coords = positions[border_pt_idx:]

    return position_times, position_coords


def find_border_entry(positions, i, border_pt=42):
    """
    Recursively search through the positions list for the first position where
    the player actually moves. Return the index of that position.
    """
    i = 0
    while i < len(positions) - 1 and positions[i][1] < border_pt:
        i += 1
    return i


def datetime_to_seconds(dt):
    """Convert a Python Datetime object into an amount in seconds."""
    return (
        (dt.day * 86400)
        + (dt.hour * 3600)
        + (dt.minute * 60)
        + dt.second
        + (dt.microsecond * 1e-6)
    )


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("input", help="Name of input observations file")
    parser.add_argument("-o", "--output", help="Name of output")
    group = parser.add_mutually_exclusive_group()
    group.add_argument(
        "--plot", help="Create static plot", action="store_true"
    )
    group.add_argument(
        "--animation", help="Create animation", action="store_true"
    )
    args = parser.parse_args()

    if args.plot:
        if args.output:
            create_visualization(args.input, args.output, viz_type="plot")
        else:
            create_visualization(args.input, "trajectories.pdf", viz_type="plot")
    else:
        if args.output:
            create_visualization(args.input, args.output, viz_type="animation")
        else:
            create_visualization(args.input, "trajectories.mp4", viz_type="animation")
