# Purpose: Overlay the observed trace data from gameplay on the ToMCAT SAR map
# Execution: python plot_SAR_observation_trace.py <observation-file-path>

import sys
import json
import math
from datetime import datetime

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.image as mpimg


def main():
    """Generates a plot overlaying user trace data on the SAR V1 map."""
    obs_file = sys.argv[1]

    # Load time and position data from the observation file
    time_data, position_data = get_time_and_position_data(obs_file)

    # Get the first change-position-point to truncate trace data
    i = find_first_position_change(position_data, 0)
    time_start, time_end = min(time_data), max(time_data)

    # Shorten the position data by the first change-position-point
    important_position_data = position_data[i:]

    # Shorten the time data by the first change-position-point and normalize
    # by the start time
    times = [t - time_start for t in time_data[i:]]

    # Separation X, Y coordinate lists for plotting purposes
    (X, Y) = map(list, zip(*important_position_data))

    # Load the image data for the V1 SAR map
    img = mpimg.imread("SAR_v1_reference.png")

    # Plot the image and trace data
    fig, ax = plt.subplots()
    cax = ax.imshow(img)
    ax.scatter(X, Y, c=times, cmap=plt.get_cmap("viridis"))

    # Add a helpful title
    ax.set_title("SAR Gameplay Time-based Position Trace")

    # Remove unnecessary axes ticks from image data
    ax.set_xticks([])
    ax.set_yticks([])

    # Add a colorbar legend with ticks in minutes
    min_start, min_end = 0, math.ceil(seconds_to_minutes(time_end - time_start))
    cbar = fig.colorbar(cax)
    cbar.ax.set_yticklabels(
        [f"{int(i)} minutes" for i in np.linspace(min_start, min_end, num=6)]
    )

    # Display the plots
    plt.show()


def get_time_and_position_data(observations_filename):
    timestamps, positions = list(), list()
    with open(observations_filename, "r") as infile:
        for line in infile:
            # Use the start of the JSON object to split the timestamp data
            # and the JSON data
            json_start = line.find("{")

            # Recover and parse the JSON data
            json_data = json.loads(line[json_start:])

            # Add a tuple (x, y) position scaled by the arena dimensions
            positions.append((
                scaleX(float(json_data["XPos"])),
                scaleY(float(json_data["ZPos"]))
            ))

            # Parse a time string from the line
            time_string = line[:json_start].strip()

            # Convert the time string to a Python datetime object
            timestamp = datetime.strptime(time_string, "%Y%m%dT%H%M%S.%f")

            # Create a timestamp in seconds
            timestamps.append(datetime_to_seconds(timestamp))
    return timestamps, positions


def find_first_position_change(positions, i):
    """
    Recursively search through the positions list for the first position where
    the player actually moves. Return the index of that position.
    """
    return i if i == len(positions)-1 or positions[i] != positions[i+1] \
        else find_first_position_change(positions, i+1)


def scaleX(x):
    # Scale the amount to be within 1000 (leaving a 40px barrier on either side)
    # NOTE Xrange is from 0 -- 1080
    return (((x-22)/(99.7-22)) * 1000) + 40


def scaleY(y):
    # Scale the amount to be within 1100 (leaving a 40px barrier on either side)
    # NOTE Yrange is from 0 -- 1180 AND the axes is inverted
    return 1140 - ((((y-45)/(97-45)) * 1100) + 40)


def datetime_to_seconds(dt):
    return (dt.day*86400) + (dt.hour*3600) + (dt.minute*60) \
        + dt.second + (dt.microsecond*1e-6)


def seconds_to_minutes(seconds):
    return seconds / 60


if __name__ == '__main__':
    main()
