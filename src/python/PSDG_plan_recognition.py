from nltk import Nonterminal
from numpy import math,e
import copy

def _initialize_B_state(grammar,d):
    B_s = {'Q': 0.0, 'N': {}, "P": {}, "Sig": {}, "T|N": {}, "T": {}}


    for i in range(1,d+1):
        B_s["N"][i] = {}
        B_s["T|N"][i] = {}
        for j in grammar.nonterminals:
            B_s["N"][i][j] = 0.0
            B_s["T|N"][i][j] = 0.0

        B_s["T|N"][i]['nil'] = 1.0
        B_s["N"][i]['nil'] = 0.0

        B_s["P"][i] = {}
        for a in grammar.productions():
            for l in range(1,len(a.rhs()) + 1):
                B_s["P"][i][(a.__str__(omit_probs = True), l)] = 0.0

        B_s["T"][i] = 0.0

    for e in grammar.terminals:
        B_s["Sig"][e] = 0.0

    return B_s

def _initialize_E_state(grammar,d,t):
    E_s = {"t": t - 1,"pi_2": {}, "p_Q": {}, "p_T_sig_T": {}, "p_T_sig": {}}

    for i in range(1,d+1):
        E_s["pi_2"][i] = {}
        for j in grammar.nonterminals:
            E_s["pi_2"][i][j] = {'T': 0.0, '-T': 0.0}

        E_s["pi_2"][i]['nil'] = {'T': 0.0, '-T': 0.0}

        E_s["p_Q"][i] = 0.0

        E_s["p_T_sig_T"][i] = 0.0

        E_s["p_T_sig"][i] = 0.0

    return E_s

def _compute_T(grammar,B,d,t):
    for l in range(d - 1,0,-1):
        pos_sym = (x for x in B[t]["N"][l] if B[t]["N"][l][x] > 0.0)
        for sym in pos_sym:
            for a in grammar.productions(lhs=sym):
                if a.rhs()[-1] != sym:
                    if isinstance(a.rhs()[-1],Nonterminal):
                        B[t]['T|N'][l][sym] += (
                            (B[t]["P"][l][(a.__str__(omit_probs = True), len(a.rhs()))]
                                        *B[t]['T|N'][l+1][a.rhs()[-1]])/B[t]["N"][l][sym]
                        )
                    else:
                        B[t]['T|N'][l][sym] += (
                                B[t]["P"][l][(a.__str__(omit_probs = True), len(a.rhs()))]/B[t]["N"][l][sym]
                        )

            B[t]['T'][l] += B[t]['T|N'][l][sym]*B[t]["N"][l][sym]
    return B

def _initialize_belief_state(grammar,d,q):
    grammar.update_prods(q)
    B = {1: _initialize_B_state(grammar,d)}
    B[1]['Q'] = 1.0
    B[1]['N'][1][grammar.start()] = 1.0
    for l in range(2,d + 1):
        pos_sym = (x for x in B[1]["N"][l - 1] if B[1]["N"][l - 1][x] > 0.0)
        for sym in pos_sym:
            for a in grammar.productions(lhs=sym):
                B[1]["P"][l-1][(a.__str__(omit_probs = True), 1)] += B[1]["N"][l-1][sym]*a.prob()
                if isinstance(a.rhs()[0],Nonterminal):
                    B[1]["N"][l][a.rhs()[0]] += B[1]["P"][l-1][(a.__str__(omit_probs = True),1)]
                else:
                    B[1]["Sig"][a.rhs()[0]] += B[1]["P"][l-1][(a.__str__(omit_probs = True),1)]

        p_sum = 0.0
        for i in grammar.nonterminals:
            p_sum += B[1]['N'][l][i]
        B[1]['N'][l]['nil'] = 1 - p_sum
    B = _compute_T(grammar,B,d,1)
    return B

def _explanation_phase(grammar,actions,B,d,q_0,q_1,t):

    E = _initialize_E_state(grammar,d,t)
    if not(t in B):
        B[t] = _initialize_B_state(grammar,d)

    for i in actions:
        B[t]["Q"] += i.state_trans_prob(q_0,q_1)*B[t-1]["Sig"][i.__name__]

    max_m = 0
    for a in grammar.productions():
        if len(a.rhs()) >= max_m:
            max_m = len(a.rhs())

    for l in range(d - 1,0,-1):
        pos_sym = (x for x in B[t-1]["N"][l] if B[t-1]["N"][l][x] > 0.0)
        for sym in pos_sym:
            for a in grammar.productions(lhs=sym):
                if a.rhs()[-1] != sym:
                    if isinstance(a.rhs()[-1],Nonterminal):
                        E['pi_2'][l][sym]['T'] += (
                            B[t-1]["P"][l][(a.__str__(omit_probs = True), len(a.rhs()))]
                            *E['pi_2'][l+1][a.rhs()[-1]]['T']
                        )

                        E['pi_2'][l][sym]['-T'] += (
                            B[t-1]["P"][l][(a.__str__(omit_probs = True), len(a.rhs()))]
                            *E['pi_2'][l+1][a.rhs()[-1]]['-T']
                        )
                    else:
                        act = next((x for x in actions if x.__name__ == a.rhs()[-1]), None)
                        E['pi_2'][l][sym]['T'] += (
                            B[t-1]["P"][l][(a.__str__(omit_probs = True), len(a.rhs()))]
                            *act.state_trans_prob(q_0,q_1)
                        )

                for b in range(1,len(a.rhs())):
                    if isinstance(a.rhs()[b-1],Nonterminal):
                        E['pi_2'][l][sym]['-T'] += (
                            B[t-1]["P"][l][(a.__str__(omit_probs = True), b)]
                            *E['pi_2'][l+1][a.rhs()[b-1]]['-T']
                        )

                        E['pi_2'][l][sym]['-T'] += (
                            B[t-1]["P"][l][(a.__str__(omit_probs = True), b)]
                            *E['pi_2'][l+1][a.rhs()[b-1]]['T']
                        )
                    else:
                        act = next((x for x in actions if x.__name__ == a.rhs()[b-1]), None)
                        E['pi_2'][l][sym]['-T'] += (
                            B[t-1]["P"][l][(a.__str__(omit_probs = True), b)]
                            *act.state_trans_prob(q_0,q_1)
                        )

            E['pi_2'][l][sym]['T'] /= B[t-1]["N"][l][sym]
            E['pi_2'][l][sym]['-T'] /= B[t-1]["N"][l][sym]


            E['p_Q'][l] += B[t-1]["N"][l][sym]*E['pi_2'][l][sym]['T']

        for k in range(1,l + 1):
            for a in grammar.productions():
                for i,b in enumerate(a.rhs()):
                    if not(isinstance(b,Nonterminal)):
                        act = next((x for x in actions if x.__name__ == b), None)
                        E['p_Q'][l] += (
                            B[t-1]["P"][k][(a.__str__(omit_probs = True), i+1)]
                            *act.state_trans_prob(q_0,q_1)
                        )

        for m in range(1,max_m + 1):
            for a in grammar.productions():
                if len(a.rhs()) == m:
                    if not(isinstance(a.rhs()[-1], Nonterminal)):
                        act = next((x for x in actions if x.__name__ == a.rhs()[-1]), None)
                        E['p_T_sig_T'][l] += (
                            B[t-1]["P"][l][(a.__str__(omit_probs = True), m)]
                            *act.state_trans_prob(q_0,q_1)
                        )
                    else:
                        E['p_T_sig_T'][l] += (
                            B[t-1]["P"][l][(a.__str__(omit_probs = True), m)]
                            *E['pi_2'][l+1][a.rhs()[-1]]['T']
                        )
        if l < (d - 1):
            E['p_T_sig_T'][l] /= E['p_Q'][l+1]
            E['p_T_sig'][l] = E['p_T_sig_T'][l]
        else:
            E['p_T_sig_T'][l] /= B[t]["Q"]

        E['p_T_sig_T'][l] *= B[t-1]["T"][l + 1]
        E['p_T_sig_T'][l] += B[t-1]["N"][l]['nil']

    return B, E

def _sym_exp(B,E,d):
    N_E = {'t': E['t'], 'N':{}}
    for l in range(1,d+1):
        N_E["N"][l] = {}
        for sym in B[E['t']]['N'][l]:
            N_E["N"][l][sym] = (
                B[E['t']]['N'][l][sym]
                *(
                   E['pi_2'][l][sym]['T']
                   +E['pi_2'][l][sym]['-T']
                )
                /B[E['t']+1]['Q']
            )
    return N_E

def _prediction_phase(grammar,actions,B,E,d,q_0,q_1,t):
    grammar.update_prods(q_1)
    B[t]['N'][1][grammar.start()] = 1.0 - E['p_T_sig'][1]
    B[t]['N'][1]['nil'] = 1.0 - B[t]['N'][1][grammar.start()]
    N_Sig_T = {1: {grammar.start(): 0.0, 'nil': 1.0}}
    P_Sig_T = {}
    for l in range(2,d + 1):
        N_Sig_T[l] = {}
        P_Sig_T[l-1] = {}
        pos_sym = (x for x in B[1]["N"][l - 1] if B[1]["N"][l - 1][x] > 0.0)
        for sym in pos_sym:
            for a in grammar.productions(lhs=sym):
                if isinstance(a.rhs()[0],Nonterminal):
                    B[t]["P"][l-1][(a.__str__(omit_probs = True), 1)] = (
                        a.prob()*N_Sig_T[l-1][sym]*B[t-1]['T'][l-1]
                        +((B[t-1]["P"][l-1][(a.__str__(omit_probs = True), 1)]
                        *E['pi_2'][l][a.rhs()[0]]['-T'])/B[t]['Q'])
                    )
                else:
                    B[t]["P"][l-1][(a.__str__(omit_probs = True), 1)] = (
                        a.prob()*N_Sig_T[l-1][sym]*B[t-1]['T'][l-1]
                    )
                B_p_sum = 0.0
                for c in grammar.productions():
                    if c.rhs()[-1] == sym and c.lhs() == sym:
                        if isinstance(c.rhs()[-2], Nonterminal):
                            B_p_sum += (
                                E['pi_2'][l][c.rhs()[-2]]['T']
                                *B[t-1]["P"][l-1][(c.__str__(omit_probs = True), len(c.rhs()) - 1)]
                            )
                        else:
                            act = next((x for x in actions if x.__name__ == c.rhs()[-2]), None)
                            B_p_sum += (
                                act.state_trans_prob(q_0,q_1)
                                *B[t-1]["P"][l-1][(c.__str__(omit_probs = True), len(c.rhs()) - 1)]
                            )
                B_p_sum *= a.prob()
                B_p_sum /= B[t]['Q']
                B[t]["P"][l-1][(a.__str__(omit_probs = True), 1)] += B_p_sum

                P_Sig_T[l-1][(a.__str__(omit_probs = True), 1)] = (
                    a.prob()*N_Sig_T[l-1][sym]*E['p_T_sig_T'][l-1]
                    +B_p_sum
                )
                for b in range(2,len(a.rhs()) + 1):
                    if isinstance(a.rhs()[b-1], Nonterminal):
                        if isinstance(a.rhs()[b-2], Nonterminal):
                            B[t]["P"][l-1][(a.__str__(omit_probs = True), b)] = (
                                (E['pi_2'][l][a.rhs()[b-1]]['-T']
                                *B[t-1]["P"][l-1][(a.__str__(omit_probs = True), b)]
                                +E['pi_2'][l][a.rhs()[b-2]]['T']
                                *B[t-1]["P"][l-1][(a.__str__(omit_probs = True), b-1)])/B[t]['Q']
                            )

                            P_Sig_T[l-1][(a.__str__(omit_probs = True), b)] = (
                                E['pi_2'][l][a.rhs()[b-2]]['T']
                                *B[t-1]["P"][l-1][(a.__str__(omit_probs = True), b-1)]
                                /E['p_Q'][l]
                            )
                        else:
                            act = next((x for x in actions if x.__name__ == a.rhs()[b-2]), None)
                            B[t]["P"][l-1][(a.__str__(omit_probs = True), b)] = (
                                (E['pi_2'][l][a.rhs()[b-1]]['-T']
                                *B[t-1]["P"][l-1][(a.__str__(omit_probs = True), b)]
                                +act.state_trans_prob(q_0,q_1)
                                *B[t-1]["P"][l-1][(a.__str__(omit_probs = True), b-1)])/B[t]['Q']
                            )

                            P_Sig_T[l-1][(a.__str__(omit_probs = True), b)] = (
                                act.state_trans_prob(q_0,q_1)
                                *B[t-1]["P"][l-1][(a.__str__(omit_probs = True), b-1)]
                                /E['p_Q'][l]
                            )
                    else:
                        if isinstance(a.rhs()[b-2], Nonterminal):
                            B[t]["P"][l-1][(a.__str__(omit_probs = True), b)] = (
                                (E['pi_2'][l][a.rhs()[b-2]]['T']
                                *B[t-1]["P"][l-1][(a.__str__(omit_probs = True), b-1)])/B[t]['Q']
                            )

                            P_Sig_T[l-1][(a.__str__(omit_probs = True), b)] = (
                                E['pi_2'][l][a.rhs()[b-2]]['T']
                                *B[t-1]["P"][l-1][(a.__str__(omit_probs = True), b-1)]
                                /E['p_Q'][l]
                            )
                        else:
                            act = next((x for x in actions if x.__name__ == a.rhs()[b-2]), None)
                            B[t]["P"][l-1][(a.__str__(omit_probs = True), b)] = (
                                (act.state_trans_prob(q_0,q_1)
                                *B[t-1]["P"][l-1][(a.__str__(omit_probs = True), b-1)])/B[t]['Q']
                            )

                            P_Sig_T[l-1][(a.__str__(omit_probs = True), b)] = (
                                act.state_trans_prob(q_0,q_1)
                                *B[t-1]["P"][l-1][(a.__str__(omit_probs = True), b-1)]
                                /E['p_Q'][l]
                            )
                for b in range(1,len(a.rhs()) + 1):
                    if isinstance(a.rhs()[b-1], Nonterminal):
                        B[t]["N"][l][a.rhs()[b-1]] += B[t]["P"][l-1][(a.__str__(omit_probs = True),b)]
                        if a.rhs()[b-1] in N_Sig_T[l]:
                            N_Sig_T[l][a.rhs()[b-1]] += P_Sig_T[l-1][(a.__str__(omit_probs = True), b)]
                        else:
                            N_Sig_T[l][a.rhs()[b-1]] = P_Sig_T[l-1][(a.__str__(omit_probs = True), b)]
                    else:
                        B[t]["Sig"][a.rhs()[b-1]] += B[t]["P"][l-1][(a.__str__(omit_probs = True),b)]
        p_sum = 0.0
        for i in grammar.nonterminals:
            p_sum += B[t]['N'][l][i]
        B[t]['N'][l]['nil'] = 1 - p_sum
    B = _compute_T(grammar,B,d,t)
    return B

def generate_initial_belief_state(domain,q,d = 5):
    try:
        return _initialize_belief_state(domain.psdg,d,q)
    except AttributeError:
        print("Try calling domain.initialize_planning(q) first!")
        raise

def update_belief_state(domain,B,q_0,q_1,d=5,include_sym_exp = False):
    t = len(B) + 1
    B,E = _explanation_phase(domain.psdg,domain.actions,B,d,q_0,q_1,t)

    if include_sym_exp:
        return _prediction_phase(domain.psdg,domain.actions,B,E,d,q_0,q_1,t), _sym_exp(B,E,d)
    return _prediction_phase(domain.psdg,domain.actions,B,E,d,q_0,q_1,t)

def generate_belief_state_seq(domain,B,Q,d=5,include_sym_exp = False):
    B = generate_initial_belief_state(domain,Q[0],d)

    if include_sym_exp:
        S = []
        for i in range(1,len(Q)):
            B,s = update_belief_state(domain,B,Q[i-1],Q[i],d,include_sym_exp)
            S.append(s)

        return B,S

    else:
        for i in range(1,len(Q)):
            B = update_belief_state(domain,B,Q[i-1],Q[i],d,include_sym_exp)

        return B


