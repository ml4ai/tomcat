from termcolor import colored

def read_nirs(block):
    for i in range(0,len(block)):
        if block[i]['info']['name'][0] == 'lion_0297' and block[i]['info']['type'][0] == 'NIRS':
            print(colored("[Status] Reading ", "green", attrs=["bold"]),
                colored(block[i]["info"]["type"], "blue"),
                colored(block[i]["info"]["name"], "blue"),
            )
            lion_0297_block = block[i]

        if block[i]['info']['name'][0] == 'tiger_0239' and block[i]['info']['type'][0] == 'NIRS':
            print(colored("[Status] Reading ", "green", attrs=["bold"]),
                colored(block[i]["info"]["type"], "blue"),
                colored(block[i]["info"]["name"], "blue"),
            )
            tiger_0239_block = block[i]
        
        if block[i]['info']['name'][0] == 'leopard_0171' and block[i]['info']['type'][0] == 'NIRS':
            print(colored("[Status] Reading ", "green", attrs=["bold"]),
                colored(block[i]["info"]["type"], "blue"),
                colored(block[i]["info"]["name"], "blue"),
            )
            leopard_0171_block = block[i]

    return lion_0297_block, tiger_0239_block, leopard_0171_block