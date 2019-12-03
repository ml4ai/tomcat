# This script constructs a dictionary with Minecraft types to bootstrap a
# Minecraft ontology for reading.

# Usage: python extract_info_from_xml_2.py <SCHEMA_FILE> .. 
import sys

from ruamel.yaml import YAML
from pathlib import Path

import xml.etree.ElementTree as ET
from pprint import pprint

yaml = YAML(typ='safe')
yaml.default_flow_style = False

minecraft_ontology_dict = {}

if len(sys.argv) > 1:
    for schema in sys.argv[1:]:
        tree = ET.parse(schema)
        out = open(Path(Path(schema).parent).joinpath('../YAML/' + Path(schema).stem + '.yaml'), 'w')
        for simple_type in tree.getroot():
            category = simple_type.attrib["name"]
            for restriction_or_union in simple_type:
                if restriction_or_union.attrib!={}:
                    minecraft_ontology_dict[category] = [member.attrib['value'] for member in restriction_or_union]
        data = yaml.load(str(minecraft_ontology_dict))
        yaml.dump(data, out)
