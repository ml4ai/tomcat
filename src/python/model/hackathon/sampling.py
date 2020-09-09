from hackathon.evidence_extraction import EvidenceSet
import numpy as np
from tqdm import tqdm


class AncestralSampling:

    def __init__(self, model):
        self.model = model

    def sample(self, initial_state, time_steps, number_of_samples, even_until_time=0):
        lt_samples = np.zeros((number_of_samples, time_steps), dtype=np.int)
        rm_samples = np.zeros((number_of_samples, time_steps), dtype=np.int)
        tg_samples = np.zeros((number_of_samples, time_steps), dtype=np.int)
        ty_samples = np.zeros((number_of_samples, time_steps), dtype=np.int)

        s_samples = np.zeros((number_of_samples, time_steps), dtype=np.int)
        s_samples[:, 0] = initial_state

        for d in tqdm(range(number_of_samples), desc='Generating samples'):
            for t in range(1, time_steps):
                if d > 0 and t <= even_until_time:
                    s_samples[d, t] = s_samples[0, t]
                    lt_samples[d, t] = lt_samples[0, t]
                    rm_samples[d, t] = rm_samples[0, t]
                    tg_samples[d, t] = tg_samples[0, t]
                    ty_samples[d, t] = ty_samples[0, t]
                else:
                    s_samples[d, t] = np.random.choice(
                        self.model.number_of_states, p=self.model.cpd_tables.theta_s[s_samples[d, t - 1], :])

                    lt_samples[d, t] = np.random.choice(
                        2, p=self.model.cpd_tables.pi_lt[s_samples[d, t], :])

                    rm_samples[d, t] = np.random.choice(
                        self.model.number_of_rooms, p=self.model.cpd_tables.theta_rm[s_samples[d, t], :])

                    tg_samples[d, t] = np.random.choice(
                        2, p=self.model.cpd_tables.pi_tg[s_samples[d, t], :])

                    ty_samples[d, t] = np.random.choice(
                        2, p=self.model.cpd_tables.pi_ty[s_samples[d, t], :])

        return EvidenceSet(lt_samples, rm_samples, tg_samples, ty_samples)
