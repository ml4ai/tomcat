from pttree import PTTNode
import json
import re

def fsm_extractor(infile,keywords,properties=[]):

    with open(infile, "r") as read_file:
        ptts_json = json.load(read_file)

    ptts = []
    for i in ptts_json:
        ptts.append(PTTNode.from_dict(i))

    fsm_seqs = []
    for i in ptts:
        fsm_seq = []
        for j in i.preorder():
            t = j.task.split()
            if t[0] in keywords:
                for l in properties:
                    if t[0] == l["task"]:
                        obj = t[l["task_arg"]]
                        for k in re.findall('\[[^\]]*\]|\([^\)]*\)|\"[^\"]*\"|\S+',j.state[1:-1]):
                            pred = k[1:-1].split()
                            if l["predicate"] == pred[0] and obj in pred:
                                t[0] = t[0]+"-"+l["predicate"]
                fsm_seq.append(t[0])
        fsm_seqs.append(fsm_seq)

    fsm_seqs_set = set(tuple(x) for x in fsm_seqs)
    fsm_seqs = [ list(x) for x in fsm_seqs_set ]

    return fsm_seqs
