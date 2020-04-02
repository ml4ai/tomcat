# The Purpose of this script is to parse plan traces from the output of
# PDDL HTN planning domains from shop3. Although there are many general
# components, this is specifically set-up for
# sar-individual-plan-trace-version.lisp. 

# Imports
import json
import re
import sys

default_description = "This is the search and rescue domain featuring a single rescuer"
default_problem_def = {
    "initial_state":[
            "BUILDING_B",
            "IN_V1_R1",
            "IN_V2_R1",
            "IN_V3_R2",
            "IN_V4_R2",
            "IN_V5_R3",
            "INJURED_V2",
            "INJURED_V3",
            "INJURED_V5",
            "RESCUER_T1",
            "ROOM_R1",
            "ROOM_R2",
            "ROOM_R3",
            "VICTIM_V1",
            "VICTIM_V2",
            "VICTIM_V3",
            "VICTIM_V4",
            "VICTIM_V5"
        ],
    "task":"enter-building-and-complete-mission_T1_B_R1"}

def initial_clean(file_name):
    TAG = 'Defining problem SAR-INDIVIDUAL-PROBLEM ...'

    tag_found = False
    remove_effect = False
    remove_task = False
    truncated_data = []
    p_plans = []
    state_str = ""
    with open(file_name) as in_file:
        for line in in_file:
            if not tag_found:
                if line.strip() == TAG:
                    tag_found = True
            elif line.strip()[0:7] == "state (":
                if line.strip()[-2:] == "))":
                    truncated_data.append(line.strip())
                else:
                    state_str = line.strip() + " "
            elif state_str:
                if line.strip()[-2:] == "))":
                    state_str = state_str + line.strip()
                    truncated_data.append(state_str)
                    state_str = ""
                elif line.strip()[-1] == "*":
                    close_id = line.strip().index("Storing new plan in *plans-found*")
                    state_str = state_str + line.strip()[0:close_id-1]
                    truncated_data.append(state_str)
                    state_str = ""
                    p_plans.append(truncated_data)
                    truncated_data = []
                else:
                    state_str = state_str + line.strip() + " "
            elif line.strip()[0:6] == "task (":
                if line.strip()[-1] != ")":
                    remove_task = True
            elif line.strip()[0:8] == "effect (":
                if line.strip()[8] == "(" or line.strip()[8:11] == "AND":
                    if line.strip()[-2:] != "))":
                        remove_effect = True
            elif remove_task:
                if line.strip()[-1] == ")":
                    remove_task = False
            elif remove_effect:
                if line.strip()[-2:] == "))":
                    remove_effect = False
            elif line.strip()[0:5] == "Depth":
                comma_id = line.strip().index(',')
                truncated_data.append(line.strip()[0:comma_id])
            elif line.strip()[0:4] == "PDDL":
                close_id = line.strip().index(")")
                truncated_data.append(line.strip()[5:close_id+1])
            elif line.isspace():
                continue
            elif line.strip()[0] == "*":
                continue
            else:
                truncated_data.append(line.strip())
    return p_plans

def tokenize_state(state_str):
    temp_str = state_str[7:-1].replace(" ","_")
    temp_str = re.split(r'[()]', temp_str)
    return [i for i in temp_str if (i != "_") and (i != "")]

def tokenize_action(action_str):
    return action_str[8:-1].replace(" ","_")

def plan_list_to_dict(plan,initial_state,plan_id):
    d_plan = {"id":plan_id, "action_count": len(plan[:-2])/3, "state_0":initial_state}
    c = 0
    for i in plan[:-1]:
        if i[0:5] == "Depth":
            c = c + 1
        if i[0:6] == "action":
            d_plan[f"action_{c-1}"] = tokenize_action(i)
        if i[0:5] == "state":
            d_plan[f"state_{c}"] = tokenize_state(i)
    return d_plan

def create_plan_traces(
        file_name,domain_name ="SAR(Individual)",
        description=default_description,
        problem_def=default_problem_def,
        out_file = "sar-individual-plan-traces.json"
        ):
    p_plans = initial_clean(file_name)

    del p_plans[0][1:-1:4]

    for i in range(1,len(p_plans)):
        p_plans[i] = p_plans[i][1:]
        del p_plans[i][1:-1:4]

    for i in range(1,len(p_plans)):
        if p_plans[i][0] != p_plans[i - 1][0]:
            match = p_plans[i][0]
            match_list = []
            for j in p_plans[i - 1]:
                if j != match:
                    match_list.append(j)
                else:
                    break
            p_plans[i] = match_list + p_plans[i]

    plans = {
    "domain":domain_name,
    "description":description,
    "problem_def":problem_def,
    "plan_traces": []
    }

    for i,p in enumerate(p_plans):
        plans["plan_traces"].append(plan_list_to_dict(p,plans["problem_def"]["initial_state"],i))

    with open(out_file,'w') as json_file:
        json.dump(plans,json_file,indent=4)

if __name__ == "__main__":
    create_plan_traces(sys.argv[1])
