# This script constructs a dictionary with Minecraft types to bootstrap a
# Minecraft ontology for reading.

# Usage: python create_ontology.py [--input <FILE_PATH>] [--output <FILE_PATH>]

import sys
import argparse
from ruamel.yaml import YAML
from pathlib import Path

import xml.etree.ElementTree as ET

arguments = argparse.ArgumentParser()

yaml = YAML(typ="safe")
yaml.default_flow_style = False

minecraft_ontology_dict = {}

arguments = argparse.ArgumentParser()

arguments.add_argument("-i", "--input", help="Input Schema")
arguments.add_argument("-o", "--output", help="YAML output")
args = arguments.parse_args()


def schemaToYAML(schema, output):
    try:
        tree = ET.parse(Path(schema))
        out = open(Path(output), "w")
        for simple_type in tree.getroot():
            category = simple_type.attrib["name"]
            for restriction_or_union in simple_type:
                if restriction_or_union.attrib != {}:
                    member_values = [
                        member.attrib["value"]
                        for member in restriction_or_union
                    ]
                    if member_values != []:
                        minecraft_ontology_dict[category] = member_values
        data = yaml.load(str(minecraft_ontology_dict))
        yaml.dump(data, out)
        return 1
    except Exception as e:
        print("Error:", str(e))
        sys.exit(1)


if __name__ == "__main__":
    schemaToYAML(schema=args.input, output=args.output)
