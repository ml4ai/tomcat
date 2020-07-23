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
            log_powers_rho_tg[i] = log_powers_rho_tg[i - 1] + utils.log(1 - self.model.cpd_tables.pi_tg[:,x])
            log_powers_rho_ty[i] = log_powers_rho_ty[i - 1] + utils.log(1 - self.model.cpd_tables.pi_ty[:,x])

        # log_pi_lt = utils.log(self.models.cpd_tables.pi_lt)
        # log_pi_tg = utils.log(self.models.cpd_tables.pi_tg)
        # log_pi_ty = utils.log(self.models.cpd_tables.pi_ty)
        # log_theta_rm = utils.log(self.models.cpd_tables.theta_rm)
        #
        # union_matrix = np.eye(num_states)
        # agg_theta_s = self.models.cpd_tables.theta_s.copy()
        # for i in range(1,tau):
        #     union_matrix += agg_theta_s
        #     agg_theta_s = np.dot(agg_theta_s, self.models.cpd_tables.theta_s)
        # union_matrix /= np.sum(union_matrix, axis=1).reshape(-1, 1)

        # Marginals are computed individually for each data points in the data set
        for d in tqdm(
                range(evidence_set.number_of_data_points), "Computing marginals"
        ):
            o_lt = evidence_set.lt_evidence[d]
            o_rm = evidence_set.rm_evidence[d]
            o_tg = evidence_set.tg_evidence[d]
            o_ty = evidence_set.ty_evidence[d]

            # alphas = np.zeros((evidence_set.time_slices, num_states))
            # alphas[0, initial_state] = 1  # The initial state has probability 1
            # alpha = alphas[0, :]
            alpha = np.zeros(num_states)
            alpha[initial_state] = 1

            # skip the first one
            for t in range(1, evidence_set.time_slices - h + 1): # Prediction time
                cum_f_tg = 1
                cum_f_ty = 1
                for w in range(h):
                    f_tg = (log_powers_rho_tg[w] + utils.log(np.dot(alpha, powers_theta_s[w])))
                    f_tg = np.exp(f_tg - np.max(f_tg))
                    f_tg = f_tg / sum(f_tg)
                    cum_f_tg *= np.dot(np.dot(f_tg, self.model.cpd_tables.theta_s), 1 - self.model.cpd_tables.pi_tg[:,x])

                    f_ty = (log_powers_rho_ty[w] + utils.log(np.dot(alpha, powers_theta_s[w])))
                    f_ty = np.exp(f_ty - np.max(f_ty))
                    f_ty = f_ty / sum(f_ty)
                    cum_f_ty *= np.dot(np.dot(f_ty, self.model.cpd_tables.theta_s), 1 - self.model.cpd_tables.pi_ty[:,x])

                # Because of precision, cum_f can be slightly greater than 1
                tg_marginals[d, t - 1] = 1 - (cum_f_tg if cum_f_tg < 1 else 1)
                ty_marginals[d, t - 1] = 1 - (cum_f_ty if cum_f_ty < 1 else 1)

                lt = o_lt[t]
                tg = o_tg[t]
                ty = o_ty[t]
                rm = o_rm[t]

                alpha = self.model.cpd_tables.pi_lt[:, lt] * \
                        self.model.cpd_tables.pi_tg[:, tg] * \
                        self.model.cpd_tables.pi_ty[:, ty] * \
                        self.model.cpd_tables.theta_rm[:, rm] * np.dot(alpha, self.model.cpd_tables.theta_s)
                alpha = alpha / sum(alpha)

                # tmp = utils.log(np.dot(alpha, self.models.cpd_tables.theta_s))
                # alpha = utils.normalize_log_array(
                #     log_pi_lt[:, lt] + log_pi_tg[:, tg] + log_pi_ty[:, ty] + log_theta_rm[:, rm] + tmp)
                # tg_marginals[d, t - 2, :] = np.sum(np.dot(union_matrix, self.models.cpd_tables.pi_tg) * np.exp(tmp.reshape(-1, 1)), 0)
                # ty_marginals[d, t - 2, :] = np.sum(np.dot(union_matrix, self.models.cpd_tables.pi_ty) * np.exp(tmp.reshape(-1, 1)), 0)

        return tg_marginals, ty_marginals

    def get_triaging_normalized_frequencies(self, evidence_set, h=1, x=1):
        # skip the first one
        tg_frequencies = self.sum_ahead(np.sum(evidence_set.tg_evidence[:, 1:] == x, axis=0), h)
        tg_frequencies = tg_frequencies / (evidence_set.number_of_data_points*h)

        ty_frequencies = self.sum_ahead(np.sum(evidence_set.ty_evidence[:, 1:] == x, axis=0), h)
        ty_frequencies = ty_frequencies / (evidence_set.number_of_data_points*h)

        return tg_frequencies, ty_frequencies

    def sum_ahead(self, v, value_range):
        if value_range == 1:
            return v
        else:
            new_values = []
            cum_sum = np.cumsum(v)
            previous = 0
            for i in range(len(v)-value_range+1):
                new_values.append(cum_sum[i+value_range-1] - previous)
                previous = cum_sum[i]

            return np.array(new_values)