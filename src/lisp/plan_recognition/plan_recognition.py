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


def state_match(s1, s2):
    return set(
        re.findall('\[[^\]]*\]|\([^\)]*\)|"[^"]*"|\S+', s1[1:-1])
    ) == set(re.findall('\[[^\]]*\]|\([^\)]*\)|"[^"]*"|\S+', s2[1:-1]))

def task_match(t1,t2):
    if t1 == t2:
        return 1
    return 0


def likelihood(obs, given, plans):
    total = 0
    count = 0
    match = False
    for i in plans:
        if i.task == given:
            total = total + 1
            leafSeq = i.getleafNodes()[: len(obs)]
            if len(obs) > len(leafSeq):
                continue
            for i in range(len(obs)):
                if (
                    state_match(leafSeq[i].state, obs[i][0])
                    and leafSeq[i].task == obs[i][1]
                ):
                    match = True
                else:
                    match = False
            if match:
                count = count + 1
    return count / total


def posterior_dist(obs, cats, plans):
    probs = np.zeros(len(cats))
    post = []
    for i, j in enumerate(cats):
        probs[i] = likelihood(obs, j["task"], plans) * j["prior"]
    probs = probs / probs.sum()

    for i, j in enumerate(probs):
        post.append({"task": cats[i]["task"], "posterior": j})
    return post


def state_simScore(s1, s2):
    return len(
        set(re.findall('\[[^\]]*\]|\([^\)]*\)|"[^"]*"|\S+', s1[1:-1]))
        & set(re.findall('\[[^\]]*\]|\([^\)]*\)|"[^"]*"|\S+', s2[1:-1]))
    )


def ob_dist(ob,leaf):
    return state_simScore(ob[0],leaf.state) + task_match(ob[1],leaf.task)




