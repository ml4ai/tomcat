import numpy as np
from tqdm import tqdm
import copy
from scipy.stats import dirichlet, beta
from hackathon import utils as utils


class ParameterLearning:
    def __init__(self, model):
        self.model = model

    def estimate_parameters(
        self, evidence_set, number_of_samples, burn_in, show_progress=True
    ):
        """
        This method executes Gibbs Sampling to estimate CPDs
        """
        local_model = copy.deepcopy(self.model)

        # Create tensors to store the samples
        theta_s_samples = np.zeros(
            (
                number_of_samples,
                self.model.number_of_states,
                self.model.number_of_states,
            )
        )
        pi_lt_samples = np.zeros(
            (number_of_samples, self.model.number_of_states, 2)
        )

        # Start by sampling State_t using simple ancestral sampling
        states_sample = self.get_initial_states_sample_from_parameters_priors(
            evidence_set.number_of_data_points, evidence_set.time_slices
        )
        for _ in tqdm(
            range(burn_in), desc="Burn-in", disable=not show_progress
        ):
            local_model.cpd_tables, states_sample = self.sample(
                evidence_set, local_model.cpd_tables, states_sample
            )

        for i in tqdm(range(number_of_samples), desc="Estimating parameters"):
            local_model.cpd_tables, states_sample = self.sample(
                evidence_set, local_model.cpd_tables, states_sample
            )

            theta_s_samples[i] = local_model.cpd_tables.theta_s
            pi_lt_samples[i] = local_model.cpd_tables.pi_lt

        local_model.cpd_tables.theta_s = np.mean(theta_s_samples, axis=0)
        local_model.cpd_tables.pi_lt = np.mean(pi_lt_samples, axis=0)

        return local_model

    def get_initial_states_sample_from_parameters_priors(
        self, number_of_data_points, time_slices
    ):
        """
        This method samples initial values for S_t using ancestral sampling
        """
        state_samples = np.zeros(
            (number_of_data_points, time_slices), dtype=np.int
        )
        theta_s_sampled = np.zeros(
            (self.model.number_of_states, self.model.number_of_states)
        )
        for state in range(self.model.number_of_states):
            theta_s_sampled[state] = dirichlet(
                self.model.parameter_priors.theta_s_priors[state]
            ).rvs()[0]

        for d in range(number_of_data_points):
            sample = self.sample_states_over_time(
                theta_s_sampled, time_slices - 1
            )
            state_samples[d] = sample

        return state_samples

    def sample_states_over_time(self, theta_s, time_slices):
        """
        This method samples S_t for each time slice given the previous sampled S_t-1
        """
        states_sample = np.zeros(time_slices + 1, dtype=np.int)

        for t in range(1, time_slices + 1):
            states_sample[t] = np.random.choice(
                self.model.number_of_states, p=theta_s[states_sample[t - 1]]
            )

        return states_sample

    def sample(self, evidence_set, cpd_tables, states_sample):
        """
        This method samples parameters and states using EM procedure
        """

        cpd_tables.theta_s, cpd_tables.pi_lt = self.sample_parameters(
            evidence_set, states_sample
        )
        states_sample = self.sample_states(
            evidence_set, cpd_tables, states_sample
        )

        return cpd_tables, states_sample

    def sample_parameters(self, evidence_set, state_sample):
        """
        This method samples parameters given the previously sampled states
        """

        # Pre-processing to sample parameters faster
        posteriors_theta_s = self.model.parameter_priors.theta_s_priors.copy()
        posteriors_pi_lt = self.model.parameter_priors.pi_lt_priors.copy()

        for t in range(1, evidence_set.time_slices):
            for d in range(evidence_set.number_of_data_points):
                # Incrementing the prior parameters
                posteriors_theta_s[state_sample[d][t - 1]][
                    state_sample[d][t]
                ] += 1
                posteriors_pi_lt[state_sample[d][t]][
                    1 - evidence_set.lt_evidence[d][t]
                ] += 1

        # Sample parameters
        theta_s_sample = np.zeros(
            (self.model.number_of_states, self.model.number_of_states)
        )
        pi_lt_sample = np.zeros((self.model.number_of_states, 2))

        for state in range(self.model.number_of_states):
            sample = dirichlet(posteriors_theta_s[state]).rvs()[0]
            theta_s_sample[state] = sample

            sample = beta(*posteriors_pi_lt[state]).rvs()
            pi_lt_sample[state] = [1 - sample, sample]

        return theta_s_sample, pi_lt_sample

    def sample_states(self, evidence_set, distributions, states_sample):
        """
        This method samples states given the previously sampled parameters
        """
        posterior_state = np.zeros(
            (evidence_set.number_of_data_points, self.model.number_of_states)
        )

        for t in range(1, evidence_set.time_slices):
            posterior_state += (
                utils.log(distributions.theta_s[states_sample[:, t - 1]])
                + utils.log(
                    distributions.pi_lt[:, evidence_set.lt_evidence[:, t]]
                ).T
                + utils.log(
                    distributions.theta_rm[:, evidence_set.rm_evidence[:, t]]
                ).T
                + utils.log(
                    distributions.pi_tg[:, evidence_set.tg_evidence[:, t]]
                ).T
                + utils.log(
                    distributions.pi_ty[:, evidence_set.ty_evidence[:, t]].T
                )
            )

            max_value_per_row = np.max(posterior_state, axis=1)[:, np.newaxis]
            posterior_state -= max_value_per_row
            posterior_state = np.exp(posterior_state)
            posterior_state /= np.sum(posterior_state, axis=1)[:, np.newaxis]

            for d, posterior in enumerate(posterior_state):
                states_sample[d, t] = np.random.choice(
                    self.model.number_of_states, p=posterior
                )

        return states_sample
