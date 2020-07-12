import numpy as np
from tqdm import tqdm
from hackathon import utils as utils


class ModelInference:
    def __init__(self, model):
        self.model = model

    def get_triaging_marginals_over_time(self, evidence_set, tau=1):
        """
        Sum product algorithm to compute the marginal probabilities of the triaging nodes over time. This algorithm
        uses evidence up to time t to compute the marginals of the nodes TG and TY at time t+1. This is done for each
        time step from the second one until the last.
        """

        tg_marginals = np.zeros(
            (
                evidence_set.number_of_data_points,
                evidence_set.time_slices - tau,
                self.model.cpd_tables.pi_tg.shape[1],
            )
        )
        ty_marginals = np.zeros(
            (
                evidence_set.number_of_data_points,
                evidence_set.time_slices - tau,
                self.model.cpd_tables.pi_ty.shape[1],
            )
        )

        initial_state = 0
        num_states = self.model.cpd_tables.theta_s.shape[0]

        log_pi_lt = utils.log(self.model.cpd_tables.pi_lt)
        log_pi_tg = utils.log(self.model.cpd_tables.pi_tg)
        log_pi_ty = utils.log(self.model.cpd_tables.pi_ty)
        log_theta_rm = utils.log(self.model.cpd_tables.theta_rm)

        union_matrix = np.eye(num_states)
        agg_theta_s = self.model.cpd_tables.theta_s.copy()
        for i in range(1,tau):
            union_matrix += agg_theta_s
            agg_theta_s = np.dot(agg_theta_s, self.model.cpd_tables.theta_s)
        union_matrix /= np.sum(union_matrix, axis=1).reshape(-1, 1)

        # Marginals are computed individually for each data points in the data set
        for d in tqdm(
                range(evidence_set.number_of_data_points), "Computing marginals"
        ):
            lt_evidence = evidence_set.lt_evidence[d]
            rm_evidence = evidence_set.rm_evidence[d]
            tg_evidence = evidence_set.tg_evidence[d]
            ty_evidence = evidence_set.ty_evidence[d]

            alphas = np.zeros((evidence_set.time_slices, num_states))
            alphas[0, initial_state] = 1  # The initial state has probability 1
            alpha = alphas[0, :]

            for t in range(2, evidence_set.time_slices - tau + 2):
                lt = lt_evidence[t-1]
                tg = tg_evidence[t-1]
                ty = ty_evidence[t-1]
                rm = rm_evidence[t-1]

                tmp = utils.log(np.dot(alpha, self.model.cpd_tables.theta_s))
                alpha = utils.normalize_log_array(
                    log_pi_lt[:, lt] + log_pi_tg[:, tg] + log_pi_ty[:, ty] + log_theta_rm[:, rm] + tmp)
                tg_marginals[d, t - 2, :] = np.sum(np.dot(union_matrix, self.model.cpd_tables.pi_tg) * np.exp(tmp.reshape(-1, 1)), 0)
                ty_marginals[d, t - 2, :] = np.sum(np.dot(union_matrix, self.model.cpd_tables.pi_ty) * np.exp(tmp.reshape(-1, 1)), 0)

        return tg_marginals, ty_marginals

    def get_triaging_normalized_frequencies(self, evidence_set, tau=1):
        tg_frequencies = np.stack(
            [
                self.sum_ahead(np.sum(1 - evidence_set.tg_evidence[:, 1:], axis=0), tau),
                self.sum_ahead(np.sum(evidence_set.tg_evidence[:, 1:], axis=0), tau),
            ],
            axis=1,
        )
        tg_frequencies = tg_frequencies / np.sum(
            tg_frequencies, axis=1
        ).reshape(-1, 1)

        ty_frequencies = np.stack(
            [
                self.sum_ahead(np.sum(1 - evidence_set.ty_evidence[:, 1:], axis=0), tau),
                self.sum_ahead(np.sum(evidence_set.ty_evidence[:, 1:], axis=0), tau),
            ],
            axis=1,
        )
        ty_frequencies = ty_frequencies / np.sum(
            ty_frequencies, axis=1
        ).reshape(-1, 1)

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

            return new_values