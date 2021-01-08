#!/usr/bin/python3

"""
Filename: pro_gen_vizualizer.py
Author: Adi Banerjee
Purpose: This program will vizualize the procedurally generated map from the low_level_map.json file. 
         It does not accept alternate filenames. The vizualized map is saved in the same directory as
         script with the name "vizualized_plt.png"
"""

import matplotlib.patches as patches
from matplotlib.collections import PatchCollection
import matplotlib.pyplot as plt
import numpy as np
import json

# Colours to use for each material
color_index = {
    "planks": "#cc7904",
    "glowstone": "#a4aba9",
    "cobblestone": "#8f8e89",
    "lava": "#e81f15",
    "water": "#1569e8",
    "sand": "#ffd080",
    "gravel": "#face2f",
}


# Read the low_level_map.json file
low_level_json = "./low_level_map.json"
with open(low_level_json) as f:
    data = json.load(f)
blocks = data["blocks"]

# Create the plot and set some parameters
fig, ax = plt.subplots()
rect_size = 2

# Add all blocks as patches to a list
patch_list = []
for block in blocks:
    if block["material"] in color_index:
        color = color_index.get(block["material"], None)

        x, y = int(block["x"]), int(block["z"])
        rect = patches.Rectangle(
            (x, y),
            rect_size,
            rect_size,
            color=color,
        )  # Create rectangular patch of the color

        patch_list.append(rect)


# Use patch collection to add the patches to plot
p = PatchCollection(patch_list, alpha=0.1, match_original=True)
ax.add_collection(p)

# Some settings
ax.set_aspect("equal")
ax.yaxis.set_ticks_position("right")
ax.xaxis.set_label_coords(0.5, -0.1)
ax.yaxis.set_label_coords(1.150, 0.5)

plt.margins(0, 0)
plt.gca().invert_xaxis()
plt.xlabel("X-axis")
plt.ylabel("Z-axis")
plt.tight_layout()

plt.savefig("vizualized_plt.png")
