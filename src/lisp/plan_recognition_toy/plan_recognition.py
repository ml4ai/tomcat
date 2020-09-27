import json
import sys
sys.path.append("..")
from plan_trace_generation.pttree import PTTNode
import re
import numpy as np

def upload_plans(infile):
    with open(infile, "r") as read_file:
        ptts_json = json.load(read_file)

    ptts = []
    for i in ptts_json:
        ptts.append(PTTNode.from_dict(i))

    return ptts

def state_match(s1,s2):
    return set(re.findall('\[[^\]]*\]|\([^\)]*\)|\"[^\"]*\"|\S+',s1[1:-1])) == set(re.findall('\[[^\]]*\]|\([^\)]*\)|\"[^\"]*\"|\S+',s2[1:-1]))

def likelihood(obs, given, plans):
    total = 0
    count = 0
    match = False
    for i in plans:
        if i.task == given:
            total = total + 1
            leafSeq = i.getleafNodes()[:len(obs)]
            for i in range(len(obs)):
                if state_match(leafSeq[i].state,obs[i][0]) and leafSeq[i].task == obs[i][1]:
                    match = True
                else:
                    match = False
            if match:
                count = count + 1
    return count/total

def posterior_dist(obs,cats,plans):
    probs = np.zeros(len(cats))
    post = []
    for i,j in enumerate(cats):
        probs[i] = likelihood(obs,j["task"],plans)*j["prior"]
    probs = probs/probs.sum()

    for i,j in enumerate(probs):
        post.append({"task": cats[i]["task"], "posterior": j})
    return post
