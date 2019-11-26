import xml.etree.ElementTree as ET
import xmlschema
import re

## Walk through the tree, extract children nodes from each branch, print leafs

def learnTree(treename, tree):
     content = set()
     elements = extractElements(treename, tree)
     for node in tree.iter():
         print("PARENT -- ", node.tag, node.keys(), node.text)
         for child in node.iter():
             print("\tCHILD -- ", child.tag, child.keys(), child.text)
             for data in child:
                 print("\t\tDATA -- ", data.tag, data.keys(), data.text)
                 [print(x.text) for x in data.iter()]

     print("-----\n",elements,"\n-----\n")

def extractElements(name, tree):
     rx = r"(\{.+\})(\w+\b)"
     elements = set("---- {name} ---".format(name=name))
     for node in tree.iter():
         tag = re.search(rx, node.tag).group(2)
         attributes = node.keys()
         elements.add(r"{Tag}, {Attributes}".format(Tag=tag, Attributes=attributes))
     return elements


"""
matches = re.finditer(regex, test_str, re.MULTILINE)

tree = ET.parse('Types.xsd.in')
missionTypes = xmlschema.XMLSchema('Types.xsd.in')

pprint(dict(missionTypes.types))

for child in tree.getroot():
    [print(x.tag) for x in child.iter()]
    [print(x.attrib) for x in child.iter('{http://www.w3.org/2001/XMLSchema}simpleType')]


str("{http://www.w3.org/2001/XMLSchema}simpleType").endswith("simpleType")

[element[1] for element in re.findall(rx, test_str)]

for node in tree.iter():
     es.add(r"{Tag}, {Attributes}".format(Tag=re.search(rx, node.tag).group(2), Attributes=node.keys()))
"""
