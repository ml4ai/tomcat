# The Purpose of this script is to parse the domain elements from a SHOP3
# planning domain file (with .lisp extension). Requires the filename of the
# .lisp to be parsed and the name of the output file (preferably a JSON).  

# Imports
import json
import re
import sys


def parse_domain_elements(
    file_name,
    out_file,
):
    types = set()
    predicates = set()
    get_predicates = False
    get_types = False
    actions = set()
    compound_tasks = set()
    get_methods = False
    methods = set()
    domain_name = ""
    with open(file_name) as in_file:
            for line in in_file:
                l = line.strip().split(";;")[0]
                if l[:10] == "(defdomain":
                    domain_name = l[10:].split()[0][1:]
                if l[:7] == "(:types":
                    get_types = True
                if get_types:
                    for i in l.strip().split():
                        if i != "-" and i != "(:types":
                            if i[-1] == ")":
                                types.add(i[:-1])
                                get_types = False
                            else:
                                types.add(i)
                if l[:12] == "(:predicates":
                    get_predicates = True
                    l = l[12:]
                if get_predicates:
                    if l == ")":
                        get_predicates = False
                    else:
                        for i in re.findall('\[[^\]]*\]|\([^\)]*\)|\"[^\"]*\"|\S+',l):
                            if i.split()[0][1:][-1] == ")":
                                predicates.add(i.split()[0][1:-1])
                            else:
                                predicates.add(i.split()[0][1:])
                            if i[-1] == ")" and i[-2] == ")":
                                get_predicates = False
                if l[:8] == "(:action":
                    actions.add(l.split()[1])
                if l[:8] == "(:method":
                    get_methods = True
                    compound_tasks.add(l.split()[1][1:])
                if get_methods:
                    if l == ")":
                        get_methods = False
                    else:
                        if l:
                            if l[0] != "(":
                                methods.add(l)



    types.discard('')
    predicates.discard('')
    actions.discard('')
    compound_tasks.discard('')
    methods.discard('')
    domain_def = {
        "Domain Name": domain_name,
        "Types": list(types),
        "Predicates": list(predicates),
        "Actions": list(actions),
        "Compound Tasks": list(compound_tasks),
        "Methods": list(methods)
    }

    with open(out_file, "w") as json_file:
        json.dump(domain_def, json_file, indent=4)


if __name__ == "__main__":
    parse_domain_elements(file_name=sys.argv[1],out_file=sys.argv[2])

