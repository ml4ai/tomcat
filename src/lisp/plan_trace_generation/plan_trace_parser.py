import sys
import json
sys.path.append("..")
from plan_trace_generation.pttree import PTTNode
from sexpdata import loads, dumps,Symbol

def parser(infile,outfile):
    plan_trees = ""
    plan_trees_collection = []
    found_plan_trees = False
    found_state = False
    skip = True
    state = ""
    states = []
    state_collection = []
    all_states = []
    depth = 0
    with open(infile) as in_file:
        for line in in_file:
            if skip:
                if line.strip()[0:16] == "Defining problem":
                    skip = False
                continue
            if line.strip()[0:16] == "Defining problem":
                found_plan_trees = False
                found_state = False
                plan_trees = plan_trees.strip()
                plan_trees_collection.append(plan_trees)
                plan_trees = ""
                all_states.append(state_collection)
                state = ""
                states = []
                state_collection = []
                depth = 0
            if not line.strip():
                continue
            if line.strip()[0:5] == "Depth":
                l = line.strip().split(",")
                if l[1][0:7] == " trying":
                    depth = int(l[0][6:])
                    found_state = True
                    continue
                if line.strip().split(",")[1][0:18] == " have found a plan":
                    state_collection.append(states)
                    state = ""
                    states = []
                    continue
            if found_state:
                if line.strip()[0:5] == "state":
                    state = line.strip()[6:].strip()
                else:
                    state = state + " " + line.strip()
                if state[-2:] == "))":
                    states.append((depth,state))
                    found_state = False
                    state = ""
            if found_plan_trees:
                plan_trees = plan_trees + " " + line.strip()
            else:
                if line.strip() == "Plan trees:":
                    found_plan_trees = True
                    found_states = False

    plan_trees = plan_trees.strip()
    plan_trees_collection.append(plan_trees)
    all_states.append(state_collection)

    for i in all_states:
        for j in range(1,len(i)):
            m_idx = [idx for idx, element in enumerate(i[j-1]) if element[0] + 1 == i[j][0][0]][0]
            i[j] = i[j-1][:m_idx+1] + i[j]

    state_seqs = []
    for i in all_states:
        for j in i:
            state_seq = []
            for k in j:
                state_seq.append(k[1])
            state_seqs.append(state_seq)

    pt = []
    for i in plan_trees_collection:
        pt = pt + loads(i)

    PTTrees = []
    for i,j in enumerate(pt):
        tree = PTTNode.from_sexplist(j[0])
        tree_nodes = tree.preorder()
        for k,l in enumerate(tree_nodes):
            l.state = state_seqs[i][k]
        PTTrees.append(tree)

    for i in PTTrees:
        i.remove_recurse()

    with open(outfile, "w") as json_file:
        json.dump(PTTrees, json_file, indent=4)

if __name__ == "__main__":
    parser(infile=sys.argv[1],outfile=sys.argv[2])
