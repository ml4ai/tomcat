# The Purpose of this script is to parse the domain elements from a SHOP3
# planning domain file (with .lisp extension) and represent it the domain in
# json format. Requires the filename of the
# .lisp to be parsed and the name of the output file (preferably a JSON).  

# Imports
import json
import re
import sys

def parse_domain_elements(
        file_name,
        out_file
):

    types = []
    predicates = []
    get_predicates = False
    get_types = False
    actions = []
    get_actions = False
    operators = []
    get_operators = False
    compound_tasks = []
    get_ct = False
    get_methods = False
    axioms = []
    domain_name = ""
    act_dict = {"header": "", "args": [], "preconditions": None, "effects": None, "cost": 1}
    op_dict = {"header": "", "args": 0, "preconditions": None, "add-effects": None, "delete-effects": None, "cost": 1}
    ct_dict = {"header": "", "args": 0, "methods": []}
    m_dict = {"header": "", "preconditions": None, "tasks": []}
    op_count = 0
    with open(file_name) as in_file:
            for line in in_file:
                l = line.strip().split(";;")[0]
                if l[:10] == "(defdomain":
                    domain_name = l[10:].split()[0][1:]
                if l[:7] == "(:types":
                    get_types = True
                    continue
                if get_types:
                    if l != ")":
                        tps = l.strip().split()
                        if len(tps) == 1:
                            types.append({"header": tps[0], "supertype": None})
                        else:
                            tps = l.split("-")
                            subtps = tps[0].strip()
                            supertp = tps[1].strip()
                            for i in subtps.split():
                                types.append({"header": i, "supertype": supertp})
                    else:
                        get_types = False
                if l[:12] == "(:predicates":
                    get_predicates = True
                    continue
                if get_predicates:
                    if l == ")":
                        get_predicates = False
                    else:
                        pred = l.strip().strip("()").split()
                        pred_dict = {"header": pred[0], "args": []}
                        for i in range(1,len(pred)):
                            if pred[i][0] == "?":
                                if i != len(pred) - 1 and pred[i + 1] == "-":
                                    pred_dict["args"].append(pred[i + 2])
                                else:
                                    pred_dict["args"].append("any")
                        predicates.append(pred_dict)
                if l[:8] == "(:action":
                    get_actions = True
                    act_dict["header"] = l.split()[1]
                    continue
                if get_actions:
                    if l == ")":
                        actions.append(act_dict)
                        act_dict = {"header": "", "args": [], "preconditions": None, "effects": None, "cost": 1}
                        get_actions = False
                    else:
                        if l[:11] == ":parameters":
                            params = l[11:].strip().strip("()").split()
                            for i in range(len(params)):
                                if params[i][0] == "?":
                                    if i != len(params) - 1 and params[i + 1] == "-":
                                        act_dict["args"].append(params[i + 2])
                                    else:
                                        act_dict["args"].append("any")
                        if l[:13] == ":precondition":
                            act_dict["preconditions"] = l[13:].strip()
                        if l[:7] == ":effect":
                            act_dict["effects"] = l[7:].strip()
                        if l[:5] == ":cost":
                            act_dict["cost"] = l[5:].strip()
                if l[:10] == "(:operator":
                    ops = l.split()
                    op_dict["header"] = ops[1][1:]
                    op_dict["args"] = len(ops) - 2
                    get_operators = True
                    continue
                if get_operators:
                    if l == ")":
                        get_operators = False
                        op_count = 0
                        operators.append(op_dict)
                        op_dict = {"header": "", "args": 0, "preconditions": None, "add-effects": None, "delete-effects": None, "cost": 1}
                    else:
                        if op_count == 0:
                            op_dict["preconditions"] = l.strip()
                            op_count = 1
                            continue
                        if op_count == 1:
                            op_dict["delete-effects"] = l.strip()
                            op_count = 2
                            continue
                        if op_count == 2:
                            op_dict["add-effects"] = l.strip()
                            op_count = 3
                            continue
                        if op_count == 3:
                            op_dict["cost"] = l.strip()
                            continue
                if l[:8] == "(:method":
                    get_ct = True
                    ct = l.split()
                    ct_dict["header"] = ct[1][1:]
                    ct_dict["args"] = len(ct) - 2
                    continue
                if get_ct:
                    if l == ")":
                        get_ct = False
                        ct_dict["methods"].append(m_dict)
                        compound_tasks.append(ct_dict)
                        ct_dict = {"header": "", "args": 0, "methods": []}
                        m_dict = {"header": "", "preconditions": None, "tasks": []}
                    else:
                        if not l:
                            get_methods = False
                            ct_dict["methods"].append(m_dict)
                            m_dict = {"header": "", "preconditions": None, "tasks": []}
                        else:
                            if l[0] != "(":
                                m_dict["header"] = l
                            elif l[:7] == "((:task":
                                m_dict["tasks"].append(l[7:].strip().strip(")"))
                            elif l[:16] == "(:ordered (:task":
                                m_dict["tasks"].append(l[16:].strip().strip(")"))
                            elif l[:6] == "(:task":
                                m_dict["tasks"].append(l[6:].strip().strip(")"))
                            else:
                                if l != "()":
                                    if not m_dict["preconditions"]:
                                        m_dict["preconditions"] = l
                                    else:
                                        m_dict["preconditions"] = m_dict["preconditions"] + " " + l
                if l[:3] == "(:-":
                    ax = l.split(" ",1)[1].split(")",1)
                    axioms.append({"header": ax[0].split()[0][1:].strip(), "args": len(ax[0].split()) - 1, "conditions": ax[1].strip()[:-1]})

    domain_def = {
            "Domain Name": domain_name,
            "Types": types,
            "Predicates": predicates,
            "Actions": actions,
            "Operators": operators,
            "Compound Tasks": compound_tasks,
            "Axioms": axioms
            }

    with open(out_file, "w") as json_file:
        json.dump(domain_def, json_file, indent=4)

if __name__ == "__main__":
    parse_domain_elements(file_name=sys.argv[1],out_file=sys.argv[2])
