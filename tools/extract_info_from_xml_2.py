# This script constructs a dictionary with Minecraft types to bootstrap a
# Minecraft ontology for reading.

# Usage: python extract_info_from_xml_2.py

import xml.etree.ElementTree as ET
from pprint import pprint

minecraft_ontology_dict = {}

tree = ET.parse('../external/malmo/Schemas/Types.xsd.in')

for simple_type in tree.getroot():
    category = simple_type.attrib["name"]
    for restriction_or_union in simple_type:
        if restriction_or_union.attrib!={}:
            minecraft_ontology_dict[category] = [member.attrib['value'] for member in restriction_or_union]

pprint(minecraft_ontology_dict)
