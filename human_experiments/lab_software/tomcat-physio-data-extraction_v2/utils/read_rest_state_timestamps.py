from termcolor import colored

def read_rest_state_timestamps(block):
    for i in range(0,len(block)):
        if block[i]['info']['name'][0] ==  'RestState':
            rest_state = block[i]['time_stamps']

            print(colored("[Status] Reading ", "green", attrs=["bold"]),
                colored(block[i]["info"]["name"], "blue"),
            )

    return rest_state
