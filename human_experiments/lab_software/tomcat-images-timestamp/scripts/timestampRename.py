#!/usr/bin/env python3

import argparse
import os

parser = argparse.ArgumentParser()

parser.add_argument("-d", "--directory", help = "Directory with images and csv file")

args = parser.parse_args()

path = args.directory.rstrip('/')

with open(f"{args.directory}/outFile.csv", "r") as outFile:
    for line in outFile:
        oldName, newName = line.strip().split(';')
        newName = newName.replace(':', '_') + ".png"
        oldName, newName = f"{path}/{oldName}", f"{path}/{newName}"
        os.rename(oldName, newName)
