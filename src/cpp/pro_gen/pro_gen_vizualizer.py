#!/usr/bin/python3

"""
Filename: pro_gen_vizualizer.py
Author: Adi Banerjee
Purpose: This program will vizualize the procedurally generated map from the low_level_map.json file.
         It does not accept alternate filenames. The vizualized map is saved in the same directory as
         script with the name "visualized_plt.pdf"
"""

import matplotlib.patches as patches
from matplotlib.collections import PatchCollection
import matplotlib.pyplot as plt
import numpy as np
import json


# Read the low_level_map.json file
semantic_json = "./build/semantic_map.json"
with open(semantic_json) as f:
    data = json.load(f)
locations = data["locations"]


fig, ax = plt.subplots()
patch_list = []
for location in locations:
    if location["bounds"]["type"] == "cuboid" and not location["id"] == "blank_box":
            coordinate_list = location["bounds"]["coordinates"]
            top_left_coords = coordinate_list[0]
            bottom_right_coords = coordinate_list[1]
            x1 = int(top_left_coords["x"])
            z1 = int(top_left_coords["z"])
            x2 = int(bottom_right_coords["x"])
            z2 = int(bottom_right_coords["z"])
            width = abs(x2 - x1)
            height = abs(z2 - z1)

            rect = patches.Rectangle(
                (x1, z1),
                width,
                height,
                linewidth=1,
                edgecolor="black",
                fill=False
            )

            patch_list.append(rect)

            patch_center_z = z1 + height/2
            ax.annotate(location["id"], (x2, patch_center_z), color='blue', weight='bold', 
                fontsize=3, ha='center', va='center')

# Use patch collection to add the patches to plot
p = PatchCollection(patch_list, match_original=True)
ax.add_collection(p)

# Some settings
ax.set_aspect("equal")
ax.yaxis.set_ticks_position("right")
ax.xaxis.set_label_coords(0.5, -0.1)
ax.yaxis.set_label_coords(1.150, 0.5)

plt.margins(0, 0)
plt.gca().invert_xaxis()
plt.xlabel("X")
plt.ylabel("Z")

plt.savefig("vizualized_plt.pdf")
plt.close()
