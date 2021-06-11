#!/usr/bin/python

"""
Filename: pyglfov_converter.py
Author: Adi Banerjee
Purpose: Converts our library's low_level_map to CMU's PyGLFov spec.

Python 3 is required to run this script.
"""

import json
import os


# Read the semantic_map.json file
semantic_json = "./low_level_map.json"
with open(semantic_json) as f:
    data = json.load(f)
blocks = data["blocks"]

converted_list = []
for block in blocks:
    cur_block = dict()
    cur_block["type"] = block["material"]
    cur_block["location"] = {"x":block["x"], "y":block["y"], "z":block["z"]}
    converted_list.append(cur_block)
    
    try:
        cur_block["powered"] = block["powered"]
    except:
        pass
    try:
        cur_block["facing"] = block["facing"]
    except:
        pass
    try:
        cur_block["half"] = block["half"]
    except:
        pass
    try:
        cur_block["hinge"] = block["hinge"]
    except:
        pass
    try:
        cur_block["open"] = block["open"]
    except:
        pass

f = open("converted_file.json", "w")
f.write(json.dumps(converted_list, indent=4))
f.close()