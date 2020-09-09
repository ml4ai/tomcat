import numpy as np
from tqdm import tqdm
from hackathon import utils as utils


class ModelInference:
    def __init__(self, model):
        self.model = model

    def get_triaging_marginals_over_time(self, evidence_set, h=1, x=1):
        """
        Sum product algorithm to compute the marginal probabilities of the triaging nodes over time. This algorithm
        uses evidence up to time t to compute the marginals of the nodes TG and TY at time t+1. This is done for each
        time step from the second one until the last.
        """

        tg_marginals = np.zeros(
            (
                evidence_set.number_of_data_points,
                evidence_set.time_slices - h
            )
        )
        ty_marginals = np.zeros(
            (
                evidence_set.number_of_data_points,
                evidence_set.time_slices - h,
            )
        )

        initial_state = 0
        num_states = self.model.cpd_tables.theta_s.shape[0]

        powers_theta_s = np.zeros((h, num_states, num_states))
        log_powers_rho_tg = np.zeros((h, num_states))
        log_powers_rho_ty = np.zeros((h, num_states))

        powers_theta_s[0] = np.eye(num_states)
        log_powers_rho_tg[0] = np.zeros(num_states)
        log_powers_rho_ty[0] = np.zeros(num_states)
        for i in range(1, h):
            powers_theta_s[i] = np.dot(powers_theta_s[i - 1], self.model.cpd_tables.theta_s)
            log_powers_rho_tg[i] = log_powers_rho_tg[i - 1] + utils.log(1 - self.model.cpd_tables.pi_tg[:, x])
            log_powers_rho_ty[i] = log_powers_rho_ty[i - 1] + utils.log(1 - self.model.cpd_tables.pi_ty[:, x])

        # Marginals are computed individually for each data points in the data set
        for d in tqdm(
                range(evidence_set.number_of_data_points), "Computing marginals"
        ):
            o_lt = evidence_set.lt_evidence[d]
            o_rm = evidence_set.rm_evidence[d]
            o_tg = evidence_set.tg_evidence[d]
            o_ty = evidence_set.ty_evidence[d]

            alpha = np.zeros(num_states)
            alpha[initial_state] = 1

            for t in range(evidence_set.time_slices - h):  # Prediction time
                cum_f_tg = 1
                cum_f_ty = 1
                for w in range(h):
                    f_tg = (log_powers_rho_tg[w] + utils.log(np.dot(alpha, powers_theta_s[w])))
                    f_tg = np.exp(f_tg - np.max(f_tg))
                    f_tg = f_tg / sum(f_tg)
                    cum_f_tg *= np.dot(np.dot(f_tg, self.model.cpd_tables.theta_s),
                                       1 - self.model.cpd_tables.pi_tg[:, x])

                    f_ty = (log_powers_rho_ty[w] + utils.log(np.dot(alpha, powers_theta_s[w])))
                    f_ty = np.exp(f_ty - np.max(f_ty))
                    f_ty = f_ty / sum(f_ty)
                    cum_f_ty *= np.dot(np.dot(f_ty, self.model.cpd_tables.theta_s),
                                       1 - self.model.cpd_tables.pi_ty[:, x])

                # Because of precision, cum_f can be slightly greater than 1
                prob_tg = 1 - (cum_f_tg if cum_f_tg < 1 else 1)
                tg_marginals[d, t] = prob_tg

                prob_ty = 1 - (cum_f_ty if cum_f_ty < 1 else 1)
                ty_marginals[d, t] = prob_ty

                # lt = o_lt[t]
                tg = o_tg[t+1]
                ty = o_ty[t+1]
                rm = o_rm[t+1]

                # alpha = self.model.cpd_tables.pi_lt[:, lt] * \
                tg_norm = self.model.cpd_tables.pi_tg[:, tg] / np.sum(self.model.cpd_tables.pi_tg[:, tg])
                ty_norm = self.model.cpd_tables.pi_ty[:, ty] / np.sum(self.model.cpd_tables.pi_ty[:, ty])
                rm_norm = self.model.cpd_tables.theta_rm[:, rm] / np.sum(self.model.cpd_tables.theta_rm[:, rm])
                # temp = self.model.cpd_tables.pi_tg[:, tg] * \
                #         self.model.cpd_tables.pi_ty[:, ty] * \
                #         self.model.cpd_tables.theta_rm[:, rm] * alpha
                temp = tg_norm * ty_norm * rm_norm * alpha
                temp = temp / np.sum(temp)
                alpha = np.dot(temp, self.model.cpd_tables.theta_s)
                alpha = alpha / sum(alpha)

        return tg_marginals, ty_marginals

    def get_triaging_normalized_frequencies(self, evidence_set, h=1, x=1):
        # skip the first one
        # tg_frequencies = self.sum_ahead(np.sum(evidence_set.tg_evidence[:, 1:] == x, axis=0), h)
        # tg_frequencies = tg_frequencies / (evidence_set.number_of_data_points*h)
        #
        # ty_frequencies = self.sum_ahead(np.sum(evidence_set.ty_evidence[:, 1:] == x, axis=0), h)
        # ty_frequencies = ty_frequencies / (evidence_set.number_of_data_points*h)
        tg_frequencies = self.sum_ahead(evidence_set.tg_evidence[:, 1:] == x, h)
        ty_frequencies = self.sum_ahead(evidence_set.ty_evidence[:, 1:] == x, h)

        return tg_frequencies, ty_frequencies

    def sum_ahead(self, data, value_range):
        new_values = []
        # cum_sum = np.cumsum(v)
        # previous = 0
        for i in range(data.shape[1] - value_range + 1):
            if value_range == 1:
                prob = np.mean(data[:, i:i + 1])
            else:
                prob = np.mean(np.bitwise_or.reduce(data[:, i:i + value_range], 1))
            new_values.append(prob)

            # new_values.append(np.sum(v[i:i+value_range]))
            # new_values.append(cum_sum[i+value_range-1] - previous)
            # previous = cum_sum[i]

        return np.array(new_values)
