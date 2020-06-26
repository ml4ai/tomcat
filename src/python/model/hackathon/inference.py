import numpy as np
from tqdm import tqdm
from hackathon import utils as utils


class ModelInference:
    def __init__(self, model):
        self.model = model

    def get_triaging_marginals_over_time(self, evidence_set):
        """
        Sum product algorithm to compute the marginal probabilities of the triaging nodes over time. This algorithm
        uses evidence up to time t to compute the marginals of the nodes TG and TY at time t+1. This is done for each
        time step from the second one until the last.
        """

        tg_marginals = np.zeros(
            (
                evidence_set.number_of_data_points,
                evidence_set.time_slices - 1,
                self.model.cpd_tables.pi_tg.shape[1],
            )
        )
        ty_marginals = np.zeros(
            (
                evidence_set.number_of_data_points,
                evidence_set.time_slices - 1,
                self.model.cpd_tables.pi_ty.shape[1],
            )
        )

        # The last matrix that shows up in the formula is the same, and it's being precomputed here for efficiency
        tg_last_term = np.stack(
            [
                np.sum(
                    self.model.cpd_tables.theta_s
                    * self.model.cpd_tables.pi_tg[:, col],
                    axis=1,
                )
                for col in range(self.model.cpd_tables.pi_tg.shape[1])
            ]
        )
        ty_last_term = np.stack(
            [
                np.sum(
                    self.model.cpd_tables.theta_s
                    * self.model.cpd_tables.pi_ty[:, col],
                    axis=1,
                )
                for col in range(self.model.cpd_tables.pi_ty.shape[1])
            ]
        )

        # Marginals are computed individually for each data points in the data set
        for d in tqdm(
            range(evidence_set.number_of_data_points), "Computing marginals"
        ):
            lt_evidence = evidence_set.lt_evidence[d]
            rm_evidence = evidence_set.rm_evidence[d]
            tg_evidence = evidence_set.tg_evidence[d]
            ty_evidence = evidence_set.ty_evidence[d]

            # Pre-computing partial matrices so only summation needs to be done in each pass
            partials = self.get_partials(
                lt_evidence, rm_evidence, tg_evidence, ty_evidence
            )

            # The two first marginals will have a slightly different equation, simpler that can be mannualy computed
            # given that the first state is always 0
            tg_marginals[d, 0:2, :] = self.get_two_first_marginals(
                partials,
                self.model.cpd_tables.theta_s,
                self.model.cpd_tables.pi_tg,
                tg_last_term,
            )
            ty_marginals[d, 0:2, :] = self.get_two_first_marginals(
                partials,
                self.model.cpd_tables.theta_s,
                self.model.cpd_tables.pi_ty,
                ty_last_term,
            )

            for t in range(2, evidence_set.time_slices - 1):
                tg_tmp = tg_last_term.copy()
                ty_tmp = ty_last_term.copy()
                for j in range(t - 1, 0, -1):
                    tg_tmp = np.stack(
                        [
                            np.sum(partials[j] * tg_tmp[row], axis=1)
                            for row in range(
                                self.model.cpd_tables.pi_tg.shape[1]
                            )
                        ]
                    )
                    tg_tmp /= np.sum(tg_tmp, axis=0)

                    ty_tmp = np.stack(
                        [
                            np.sum(partials[j] * ty_tmp[row], axis=1)
                            for row in range(
                                self.model.cpd_tables.pi_tg.shape[1]
                            )
                        ]
                    )
                    ty_tmp /= np.sum(ty_tmp, axis=0)

                tg_marginals[d, t] = np.sum(partials[0] * tg_tmp, axis=1)
                tg_marginals[d, t] /= np.sum(tg_marginals[d, t])

                ty_marginals[d, t] = np.sum(partials[0] * ty_tmp, axis=1)
                ty_marginals[d, t] /= np.sum(ty_marginals[d, t])

        return tg_marginals, ty_marginals

    def get_partials(self, lt_evidence, rm_evidence, tg_evidence, ty_evidence):
        """
        This method pre computes all matrices used in the sum product algorithm from time 1 to T
        """
        partials = []

        lt = self.model.cpd_tables.pi_lt[:, lt_evidence[1]]
        rm = self.model.cpd_tables.theta_rm[:, rm_evidence[1]]
        tg = self.model.cpd_tables.pi_tg[:, tg_evidence[1]]
        ty = self.model.cpd_tables.pi_ty[:, ty_evidence[1]]
        partial = (
            utils.log(self.model.cpd_tables.theta_s[0])
            + utils.log(lt)
            + utils.log(rm)
            + utils.log(tg)
            + utils.log(ty)
        )
        partials.append(utils.normalize_log_array(partial))

        time_slices = len(lt_evidence)
        for t in range(2, time_slices):
            lt = self.model.cpd_tables.pi_lt[:, lt_evidence[t]]
            rm = self.model.cpd_tables.theta_rm[:, rm_evidence[t]]
            tg = self.model.cpd_tables.pi_tg[:, tg_evidence[t]]
            ty = self.model.cpd_tables.pi_ty[:, ty_evidence[t]]
            partial = (
                utils.log(self.model.cpd_tables.theta_s)
                + utils.log(lt)
                + utils.log(rm)
                + utils.log(tg)
                + utils.log(ty)
            )
            partials.append(utils.normalize_log_matrix_columnwise(partial))

        return partials

    def get_two_first_marginals(
        self, partials, transition_matrix, emission_matrix, last_term
    ):
        """
        This method computes the marginal for the two first time slices
        """
        marginals = np.zeros((2, emission_matrix.shape[1]))
        marginals[0] = np.sum(transition_matrix[0] * emission_matrix.T, axis=1)
        marginals[0] /= np.sum(marginals[0])
        marginals[1] = np.sum(partials[0] * last_term, axis=1)
        marginals[1] /= np.sum(marginals[1])

        return marginals

    def get_triaging_normalized_frequencies(self, evidence_set):
        tg_frequencies = np.stack(
            [
                np.sum(1 - evidence_set.tg_evidence[:, 1:], axis=0),
                np.sum(evidence_set.tg_evidence[:, 1:], axis=0),
            ],
            axis=1,
        )
        tg_frequencies = tg_frequencies / np.sum(
            tg_frequencies, axis=1
        ).reshape(-1, 1)

        ty_frequencies = np.stack(
            [
                np.sum(1 - evidence_set.ty_evidence[:, 1:], axis=0),
                np.sum(evidence_set.ty_evidence[:, 1:], axis=0),
            ],
            axis=1,
        )
        ty_frequencies = ty_frequencies / np.sum(
            ty_frequencies, axis=1
        ).reshape(-1, 1)

        return tg_frequencies, ty_frequencies
