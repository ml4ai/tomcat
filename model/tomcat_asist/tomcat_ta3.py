import numpy as np
from scipy.stats import dirichlet, beta, invgamma, norm
from tqdm import tqdm
import adapter.data_adapter as data_adapter
import tomcat_asist.data_processing as data_processing
import time
from sklearn.model_selection import KFold

NUMBER_OF_STATES = 115
NUMBER_OF_ROOMS = len(data_adapter.ROOMS) - 1  # Removing Waiting Room from the list
NUMBER_OF_GREEN_VICTIMS = len(data_adapter.GREEN_VICTIMS)
NUMBER_OF_YELLOW_VICTIMS = len(data_adapter.YELLOW_VICTIMS)
ZERO = 10 ** -16


def log(values):
    values[values == 0] = ZERO
    return np.log(values)


class FixedModelDistributions:

    def get_theta_rm(self):
        """
        Predefined distribution because we can always tell the room by the state
        """

        # In the hallways, the number of the room in the number of the state
        theta_rm = np.zeros((NUMBER_OF_STATES, NUMBER_OF_ROOMS))
        for state in range(7):
            theta_rm[state] = self.one_hot_encode(NUMBER_OF_ROOMS, [state])

        for state in range(7, NUMBER_OF_STATES):
            room_index = int((state - 7) / 6) + 7
            theta_rm[state] = self.one_hot_encode(NUMBER_OF_ROOMS, [room_index])

        return theta_rm

    def get_pi_tg(self):
        pi_tg = np.array([[1, ZERO]]).repeat(NUMBER_OF_STATES, axis=0)
        pi_tg[8:NUMBER_OF_STATES:3] = [ZERO, 1]  # Dark and light room triaging of green victims

        return pi_tg

    def get_pi_ty(self):
        pi_ty = np.array([[1, ZERO]]).repeat(NUMBER_OF_STATES, axis=0)
        pi_ty[9:NUMBER_OF_STATES:3] = [ZERO, 1]  # Dark and light room triaging of yellow victims
        return pi_ty

    def get_theta_s_priors(self):
        priors = np.zeros((NUMBER_OF_STATES, NUMBER_OF_STATES))

        priors[0] = self.one_hot_encode(NUMBER_OF_STATES, [0, 1])
        priors[1] = self.one_hot_encode(NUMBER_OF_STATES, [range(3), range(7, 31, 3)])
        priors[2] = self.one_hot_encode(NUMBER_OF_STATES, [range(1, 5), range(55, 97, 3)])

        priors[3] = self.one_hot_encode(NUMBER_OF_STATES,
                                        [2, 3, 5, range(13, 19, 3), range(31, 37, 3), range(55, 61, 3)])
        priors[4] = self.one_hot_encode(NUMBER_OF_STATES, [2, 4, 6, range(73, 79, 3), range(97, 103, 3)])
        priors[5] = self.one_hot_encode(NUMBER_OF_STATES, [3, 5, range(13, 19, 3), range(31, 55, 3)])
        priors[6] = self.one_hot_encode(NUMBER_OF_STATES, [4, 6, range(97, 115, 3)])

        # P(theta_s | s_t-1 = state_some_room)
        priors[7:13] = self.__get_theta_s_prior_per_room_state(7, [range(13, 19, 3)], [1])
        priors[13:19] = self.__get_theta_s_prior_per_room_state(13, [range(7, 13, 3)], [1, 3, 5])
        priors[19:25] = self.__get_theta_s_prior_per_room_state(19, [range(25, 31, 3)], [1])
        priors[25:31] = self.__get_theta_s_prior_per_room_state(25, [range(19, 25, 3)], [1])
        priors[31:37] = self.__get_theta_s_prior_per_room_state(31, [range(37, 43, 3), range(55, 67, 3)], [3, 5])
        priors[37:43] = self.__get_theta_s_prior_per_room_state(37,
                                                                [range(31, 37, 3), range(43, 49, 3), range(61, 67, 3)],
                                                                [5])
        priors[43:49] = self.__get_theta_s_prior_per_room_state(43,
                                                                [range(37, 43, 3), range(49, 55, 3), range(61, 67, 3)],
                                                                [5])
        priors[49:55] = self.__get_theta_s_prior_per_room_state(49, [range(43, 49, 3), range(67, 73, 3)], [5])
        priors[55:61] = self.__get_theta_s_prior_per_room_state(55, [range(31, 37, 3), range(61, 67, 3)], [2, 3])
        priors[61:67] = self.__get_theta_s_prior_per_room_state(61,
                                                                [range(31, 49, 3), range(55, 61, 3), range(67, 73, 3)],
                                                                [2])
        priors[67:73] = self.__get_theta_s_prior_per_room_state(67, [range(49, 55, 3), range(61, 67, 3)], [2])
        priors[73:79] = self.__get_theta_s_prior_per_room_state(73, [range(79, 85, 3), range(97, 103, 3)], [2, 4])
        priors[79:85] = self.__get_theta_s_prior_per_room_state(79,
                                                                [range(73, 79, 3), range(85, 91, 3), range(97, 103, 3)],
                                                                [2])
        priors[85:91] = self.__get_theta_s_prior_per_room_state(85, [range(79, 85, 3), range(91, 97, 3),
                                                                     range(103, 109, 3)], [2])
        priors[91:97] = self.__get_theta_s_prior_per_room_state(91, [range(85, 91, 3), range(109, 115, 3)], [2])
        priors[97:103] = self.__get_theta_s_prior_per_room_state(97, [range(73, 85, 3), range(103, 109, 3)], [4, 6])
        priors[103:109] = self.__get_theta_s_prior_per_room_state(103, [range(85, 91, 3), range(97, 103, 3),
                                                                        range(109, 115, 3)], [6])
        priors[109:115] = self.__get_theta_s_prior_per_room_state(109, [range(91, 97, 3), range(103, 109, 3)], [6])

        return priors

    def one_hot_encode(self, size, one_indices, zero=ZERO):
        vector = zero * np.ones(size)
        for index in one_indices:
            vector[index] = 1

        return vector

    def __get_theta_s_prior_per_room_state(self, initial_state_number, adjacent_room_walk_state_numbers,
                                           adjacent_hallways_state_numbers):
        priors = np.zeros((6, NUMBER_OF_STATES))

        # LRW to TGVLR, TYVLR, DRW | LRW, DRW adjacent rooms | HW adjacent Hallways
        valid_transitions = [range(initial_state_number,
                                   initial_state_number + 4)] + adjacent_room_walk_state_numbers + adjacent_hallways_state_numbers
        priors[0] = self.one_hot_encode(NUMBER_OF_STATES, valid_transitions)

        # Keep triaging + back to room walk (same room)
        valid_transitions = [initial_state_number, initial_state_number + 1]
        priors[1] = self.one_hot_encode(NUMBER_OF_STATES, valid_transitions)

        # Keep triaging + back to room walk (same room)
        valid_transitions = [initial_state_number, initial_state_number + 2]
        priors[2] = self.one_hot_encode(NUMBER_OF_STATES, valid_transitions)

        # DRW to TGVDR, TYVDR, LRW | LRW, DRW adjacent rooms | HW adjacent Hallways
        valid_transitions = [initial_state_number, range(initial_state_number + 3,
                                                         initial_state_number + 6)] + adjacent_room_walk_state_numbers + adjacent_hallways_state_numbers
        priors[3] = self.one_hot_encode(NUMBER_OF_STATES, valid_transitions)

        # Keep triaging + back to room walk (same room)
        valid_transitions = [initial_state_number + 3, initial_state_number + 4]
        priors[4] = self.one_hot_encode(NUMBER_OF_STATES, valid_transitions)

        # Keep triaging + back to room walk (same room)
        valid_transitions = [initial_state_number + 3, initial_state_number + 5]
        priors[5] = self.one_hot_encode(NUMBER_OF_STATES, valid_transitions)

        return priors

    def get_pi_lt_priors(self):
        priors = np.zeros((NUMBER_OF_STATES, 2))

        # Lights in the hallway states (exception below) are always on
        priors[0:7, 0] = 1
        priors[0:7, 1] = ZERO

        # Lights in the staging area can be on or off
        priors[1] = [1, 1]

        for state in range(7, NUMBER_OF_STATES):
            if int((state - 7) / 3) % 2 == 0:
                # States which the light is on
                priors[state] = [1, ZERO]
            else:
                # States which the light is off
                priors[state] = [ZERO, 1]

        return priors

    # def get_sigma_priors(self):
    #     return np.ones((NUMBER_OF_STATES, 2))


class ParameterPriors:

    def __init__(self, theta_s_priors, pi_lt_priors):
        self.theta_s_priors = theta_s_priors
        self.pi_lt_priors = pi_lt_priors
        # self.sigma_dg_priors = sigma_dg_priors
        # self.sigma_dy_priors = sigma_dy_priors


class Distributions:

    def __init__(self, theta_s=None, pi_lt=None, sigma_dg=None, sigma_dy=None):
        # Fixed distributions
        fixed_model_distributions = FixedModelDistributions()
        self.theta_rm = fixed_model_distributions.get_theta_rm()
        self.pi_tg = fixed_model_distributions.get_pi_tg()
        self.pi_ty = fixed_model_distributions.get_pi_ty()

        # Trainable distributions
        self.theta_s = theta_s
        self.pi_lt = pi_lt
        # self.sigma_dg = sigma_dg
        # self.sigma_dy = sigma_dy


class ModelBuilder:

    def estimate_parameters(self, evidence_set, number_of_samples, burn_in):
        parameter_priors = self.get_parameter_priors()

        # Create tensors to store the samples
        theta_s_samples = np.zeros((number_of_samples, NUMBER_OF_STATES, NUMBER_OF_STATES))
        pi_lt_samples = np.zeros((number_of_samples, NUMBER_OF_STATES, 2))
        # sigma_dg_samples = np.zeros((number_of_samples, NUMBER_OF_GREEN_VICTIMS, NUMBER_OF_STATES, 1))
        # sigma_dy_samples = np.zeros((number_of_samples, NUMBER_OF_YELLOW_VICTIMS, NUMBER_OF_STATES, 1))

        # Start by sampling State_t using simple ancestral sampling
        states_sample = self.get_initial_states_sample_from_parameters_priors(parameter_priors.theta_s_priors,
                                                                              evidence_set.number_of_data_points,
                                                                              evidence_set.time_slices)
        distributions = Distributions()

        for _ in tqdm(range(burn_in), desc='Burn-in'):
            distributions, states_sample = self.sample(evidence_set, parameter_priors, distributions, states_sample)

        for i in tqdm(range(number_of_samples), desc='Estimating parameters'):
            distributions, states_sample = self.sample(evidence_set, parameter_priors, distributions, states_sample)

            theta_s_samples[i] = distributions.theta_s
            pi_lt_samples[i] = distributions.pi_lt
            # sigma_dg_samples[i] = distributions.sigma_dg
            # sigma_dy_samples[i] = distributions.sigma_dy

        distributions.theta_s = np.mean(theta_s_samples, axis=0)
        distributions.pi_lt = np.mean(pi_lt_samples, axis=0)
        # distributions.sigma_dg = np.mean(sigma_dg_samples, axis=0)
        # distributions.sigma_dy = np.mean(sigma_dy_samples, axis=0)

        return distributions

    def get_parameter_priors(self):
        fixed_model_distributions = FixedModelDistributions()
        theta_s_priors = fixed_model_distributions.get_theta_s_priors()  # P(theta_s|state = s) one per row
        pi_lt_priors = fixed_model_distributions.get_pi_lt_priors()  # P(pi_lt|state = s) one per row
        # sigma_dg_priors = np.zeros((NUMBER_OF_GREEN_VICTIMS, NUMBER_OF_STATES, 2))
        # for v in range(NUMBER_OF_GREEN_VICTIMS):
        #     sigma_dg_priors[
        #         v] = fixed_model_distributions.get_sigma_priors()  # for victim v: P(sigma_dg|state = s) one per row
        # sigma_dy_priors = np.zeros((NUMBER_OF_YELLOW_VICTIMS, NUMBER_OF_STATES, 2))
        # for v in range(NUMBER_OF_YELLOW_VICTIMS):
        #     sigma_dy_priors[
        #         v] = fixed_model_distributions.get_sigma_priors()  # for victim v: P(sigma_dg|state = s) one per row

        return ParameterPriors(theta_s_priors, pi_lt_priors)

    def get_initial_states_sample_from_parameters_priors(self, theta_s_priors, number_of_data_points, time_slices):
        state_samples = np.zeros((number_of_data_points, time_slices), dtype=np.int)
        theta_s_sampled = np.zeros((NUMBER_OF_STATES, NUMBER_OF_STATES))
        for state in range(NUMBER_OF_STATES):
            theta_s_sampled[state] = dirichlet(theta_s_priors[state]).rvs()[0]

        for d in range(number_of_data_points):
            sample = self.sample_states_over_time(theta_s_sampled, time_slices - 1)
            state_samples[d] = sample

        return state_samples

    def sample(self, evidence_set, parameter_priors, distributions, states_sample):
        posteriors_theta_s = parameter_priors.theta_s_priors
        posteriors_pi_lt = parameter_priors.pi_lt_priors
        # posteriors_sigma_dg = np.zeros((NUMBER_OF_GREEN_VICTIMS, NUMBER_OF_STATES, 2))
        # for v in range(NUMBER_OF_GREEN_VICTIMS):
        #     posteriors_sigma_dg[v] = parameter_priors.sigma_dg_priors[v]
        # posteriors_sigma_dy = np.zeros((NUMBER_OF_YELLOW_VICTIMS, NUMBER_OF_STATES, 2))
        # for v in range(NUMBER_OF_YELLOW_VICTIMS):
        #     posteriors_sigma_dy[v] = parameter_priors.sigma_dy_priors[v]

        for t in range(1, evidence_set.time_slices):
            for d in range(evidence_set.number_of_data_points):
                # Incrementing the prior parameters
                posteriors_theta_s[states_sample[d][t - 1]][states_sample[d][t]] += 1
                posteriors_pi_lt[states_sample[d][t]][1 - evidence_set.lt_evidence[d][t]] += 1
                # for v in range(NUMBER_OF_GREEN_VICTIMS):
                #     posteriors_sigma_dg[v][states_sample[d][t]] += [1 / 2, evidence_set.dg_evidence[v][d][t] / 2]
                # for v in range(NUMBER_OF_YELLOW_VICTIMS):
                #     posteriors_sigma_dy[v][states_sample[d][t]] += [1 / 2, evidence_set.dy_evidence[v][d][t] / 2]

        # Sample parameters
        # ini = time.time()
        distributions.theta_s, distributions.pi_lt = self.sample_parameters(evidence_set, parameter_priors,
                                                                            states_sample)
        # print("Time to sample parameters: {} seconds".format(time.time() - ini))
        # ini = time.time()
        self.sample_states(evidence_set, distributions, states_sample)
        # print("Time to sample states: {} seconds".format(time.time() - ini))

        return distributions, states_sample

    def sample_parameters(self, evidence_set, parameter_priors, state_sample):
        # Preprocessing to sample parameters faster
        posteriors_theta_s = parameter_priors.theta_s_priors
        posteriors_pi_lt = parameter_priors.pi_lt_priors
        # posteriors_sigma_dg = np.zeros((NUMBER_OF_GREEN_VICTIMS, NUMBER_OF_STATES, 2))
        # for v in range(NUMBER_OF_GREEN_VICTIMS):
        #     posteriors_sigma_dg[v] = parameter_priors.sigma_dg_priors[v]
        # posteriors_sigma_dy = np.zeros((NUMBER_OF_YELLOW_VICTIMS, NUMBER_OF_STATES, 2))
        # for v in range(NUMBER_OF_YELLOW_VICTIMS):
        #     posteriors_sigma_dy[v] = parameter_priors.sigma_dy_priors[v]

        for t in range(1, evidence_set.time_slices):
            for d in range(evidence_set.number_of_data_points):
                # Incrementing the prior parameters
                posteriors_theta_s[state_sample[d][t - 1]][state_sample[d][t]] += 1
                posteriors_pi_lt[state_sample[d][t]][1 - evidence_set.lt_evidence[d][t]] += 1
                # for v in range(NUMBER_OF_GREEN_VICTIMS):
                #     posteriors_sigma_dg[v][state_sample[d][t]] += [1 / 2, evidence_set.dg_evidence[v][d][t] / 2]
                # for v in range(NUMBER_OF_YELLOW_VICTIMS):
                #     posteriors_sigma_dy[v][state_sample[d][t]] += [1 / 2, evidence_set.dy_evidence[v][d][t] / 2]

        # Sample parameters
        theta_s_sample = np.zeros((NUMBER_OF_STATES, NUMBER_OF_STATES))
        pi_lt_sample = np.zeros((NUMBER_OF_STATES, 2))
        # sigma_dg_sample = np.zeros((NUMBER_OF_GREEN_VICTIMS, NUMBER_OF_STATES, 1))
        # sigma_dy_sample = np.zeros((NUMBER_OF_YELLOW_VICTIMS, NUMBER_OF_STATES, 1))

        for state in range(NUMBER_OF_STATES):
            sample = dirichlet(posteriors_theta_s[state]).rvs()[0]
            theta_s_sample[state] = sample

            sample = beta(*posteriors_pi_lt[state]).rvs()
            pi_lt_sample[state] = [1 - sample, sample]

            # for v in range(NUMBER_OF_GREEN_VICTIMS):
            #     sample = invgamma(*posteriors_sigma_dg[v][state]).rvs()
            #     sigma_dg_sample[v][state] = sample
            #
            # for v in range(NUMBER_OF_YELLOW_VICTIMS):
            #     sample = invgamma(*posteriors_sigma_dy[v][state]).rvs()
            #     sigma_dy_sample[v][state] = sample

        return theta_s_sample, pi_lt_sample  # , sigma_dg_sample, sigma_dy_sample

    def sample_states(self, evidence_set, distributions, states_sample):
        posterior_state = np.zeros((evidence_set.number_of_data_points, NUMBER_OF_STATES))

        for t in range(1, evidence_set.time_slices):
            posterior_state += log(distributions.theta_s[states_sample[:, t - 1]]) + log(
                distributions.pi_lt[:, evidence_set.lt_evidence[:, t]]).T + log(
                distributions.theta_rm[:, evidence_set.rm_evidence[:, t]]).T + log(
                distributions.pi_tg[:, evidence_set.tg_evidence[:, t]]).T + log(
                distributions.pi_ty[:, evidence_set.ty_evidence[:, t]].T)

            # for v in range(NUMBER_OF_GREEN_VICTIMS):
            #     gaussian = norm(0, distributions.sigma_dg[v].flatten())
            #     pdfs = gaussian.pdf([[distance] for distance in evidence_set.dg_evidence[v][:, t]])
            #     posterior_state += log(pdfs)
            #
            # for v in range(NUMBER_OF_YELLOW_VICTIMS):
            #     gaussian = norm(0, distributions.sigma_dy[v].flatten())
            #     pdfs = gaussian.pdf([[distance] for distance in evidence_set.dy_evidence[v][:, t]])
            #     posterior_state += log(pdfs)

            max_value_per_row = np.max(posterior_state, axis=1)[:, np.newaxis]
            posterior_state -= max_value_per_row
            posterior_state = np.exp(posterior_state)
            posterior_state /= np.sum(posterior_state, axis=1)[:, np.newaxis]

            for d, posterior in enumerate(posterior_state):
                states_sample[d, t] = np.random.choice(NUMBER_OF_STATES, p=posterior)

        return states_sample

    def sample_states_over_time(self, theta_s, time_slices):
        states_sample = np.zeros(time_slices + 1, dtype=np.int)

        for t in range(1, time_slices + 1):
            states_sample[t] = np.random.choice(NUMBER_OF_STATES, p=theta_s[states_sample[t - 1]])

        return states_sample

    def get_triaging_marginals_over_time(self, distributions, evidence_set):
        """
        Sum product algorithm to compute the marginals of the observable nodes over time
        """
        # lt_last_term = np.stack([np.sum(distributions.theta_s * distributions.pi_lt[:, col], axis=1) for col in
        #                          range(distributions.pi_lt.shape[1])])
        # rm_last_term = np.stack([np.sum(distributions.theta_s * distributions.theta_rm[:, col], axis=1) for col in
        #                          range(distributions.theta_rm.shape[1])])
        tg_marginals = np.zeros(
            (evidence_set.number_of_data_points, evidence_set.time_slices - 1, distributions.pi_tg.shape[1]))
        ty_marginals = np.zeros(
            (evidence_set.number_of_data_points, evidence_set.time_slices - 1, distributions.pi_ty.shape[1]))

        tg_last_term = np.stack([np.sum(distributions.theta_s * distributions.pi_tg[:, col], axis=1) for col in
                                 range(distributions.pi_tg.shape[1])])
        ty_last_term = np.stack([np.sum(distributions.theta_s * distributions.pi_ty[:, col], axis=1) for col in
                                 range(distributions.pi_ty.shape[1])])

        for d in tqdm(range(evidence_set.number_of_data_points), 'Computing Marginals'):
            lt_evidence = evidence_set.lt_evidence[d]
            rm_evidence = evidence_set.rm_evidence[d]
            tg_evidence = evidence_set.tg_evidence[d]
            ty_evidence = evidence_set.ty_evidence[d]

            # lt_marginals = np.zeros((evidence_set.time_slices - 1, distributions.pi_lt.shape[1]))
            # rm_marginals = np.zeros((evidence_set.time_slices - 1, distributions.theta_rm.shape[1]))

            partials = self.get_partials(distributions, lt_evidence, rm_evidence, tg_evidence, ty_evidence)

            tg_marginals[d, 0:2, :] = self.get_two_first_marginals(partials, distributions.theta_s, distributions.pi_tg,
                                                                   tg_last_term)
            ty_marginals[d, 0:2, :] = self.get_two_first_marginals(partials, distributions.theta_s, distributions.pi_ty,
                                                                   ty_last_term)

            for t in range(2, evidence_set.time_slices - 1):
                tg_tmp = tg_last_term.copy()
                ty_tmp = ty_last_term.copy()
                for j in range(t - 1, 0, -1):
                    tg_tmp = np.stack(
                        [np.sum(partials[j] * tg_tmp[row], axis=1) for row in range(distributions.pi_tg.shape[1])])
                    tg_tmp /= np.sum(tg_tmp, axis=0)

                    ty_tmp = np.stack(
                        [np.sum(partials[j] * ty_tmp[row], axis=1) for row in range(distributions.pi_tg.shape[1])])
                    ty_tmp /= np.sum(ty_tmp, axis=0)

                tg_marginals[d, t] = np.sum(partials[0] * tg_tmp, axis=1)
                tg_marginals[d, t] /= np.sum(tg_marginals[d, t])

                ty_marginals[d, t] = np.sum(partials[0] * ty_tmp, axis=1)
                ty_marginals[d, t] /= np.sum(ty_marginals[d, t])

        return tg_marginals, ty_marginals

    def get_partials(self, distributions, lt_evidence, rm_evidence, tg_evidence, ty_evidence):
        partials = []

        lt = distributions.pi_lt[:, lt_evidence[1]]
        rm = distributions.theta_rm[:, lt_evidence[1]]
        tg = distributions.pi_tg[:, lt_evidence[1]]
        ty = distributions.pi_ty[:, lt_evidence[1]]
        partial = log(distributions.theta_s[0]) + log(lt) + log(rm) + log(tg) + log(ty)
        partial -= np.max(partial)
        partial = np.exp(partial)
        partials.append(partial / np.sum(partial))

        for t in range(2, evidence_set.time_slices):
            lt = distributions.pi_lt[:, lt_evidence[t]]
            rm = distributions.theta_rm[:, lt_evidence[t]]
            tg = distributions.pi_tg[:, lt_evidence[t]]
            ty = distributions.pi_ty[:, lt_evidence[t]]
            partial = log(distributions.theta_s) + log(lt) + log(rm) + log(tg) + log(ty)
            partial -= np.max(partial, axis=1).reshape(-1, 1)
            partial = np.exp(partial)
            partials.append(partial / np.sum(partial, axis=1).reshape(-1, 1))

        return partials

    def get_two_first_marginals(self, partials, transition_matrix, emission_matrix, last_term):
        marginals = np.zeros((2, emission_matrix.shape[1]))
        marginals[0] = np.sum(transition_matrix[0] * emission_matrix.T, axis=1)
        marginals[0] /= np.sum(marginals[0])
        marginals[1] = np.sum(partials[0] * last_term, axis=1)
        marginals[1] /= np.sum(marginals[1])

        return marginals

    def get_triaging_normalized_frequencies(self, evidence_set):
        tg_frequencies = np.stack(
            [np.sum(1 - evidence_set.tg_evidence[:, 1:], axis=0), np.sum(evidence_set.tg_evidence[:, 1:], axis=0)],
            axis=1)
        tg_frequencies = tg_frequencies / np.sum(tg_frequencies, axis=1).reshape(-1, 1)

        ty_frequencies = np.stack(
            [np.sum(1 - evidence_set.ty_evidence[:, 1:], axis=0), np.sum(evidence_set.ty_evidence[:, 1:], axis=0)],
            axis=1)
        ty_frequencies = ty_frequencies / np.sum(ty_frequencies, axis=1).reshape(-1, 1)

        return tg_frequencies, ty_frequencies


class Experimentation:

    def __init__(self):
        self.model_builder = ModelBuilder()

    def estimate_parameters_on_full_data(self, evidence_folder, parameters_folder, number_of_samples, burn_in):
        dp = data_processing.DataProcessing()
        evidence_set = dp.load_evidence_set(evidence_folder)
        distributions = self.model_builder.estimate_parameters(evidence_set, number_of_samples, burn_in)

        filename_suffix = 'full_data_{}dp_{}s_{}bi'.format(evidence_set.number_of_data_points, number_of_samples,
                                                           burn_in)
        dp.save_theta_s(parameters_folder, distributions.theta_s, filename_suffix)
        dp.save_pi_lt(parameters_folder, distributions.pi_lt, filename_suffix)
        # dp.save_sigma_dg(parameters_folder, distributions.sigma_dg, filename_suffix)
        # dp.save_sigma_dy(parameters_folder, distributions.sigma_dy, filename_suffix)

    def fit_and_evaluate(self, evidence_set, number_of_samples, burn_in, number_of_folds, evaluation_folder):
        folds = self.shuffle_and_split_data(evidence_set, number_of_folds)

        tg_baseline_log_losses = []
        ty_baseline_log_losses = []
        tg_model_log_losses = []
        ty_model_log_losses = []

        for training_set, test_set in folds:
            distributions = self.model_builder.estimate_parameters(training_set, number_of_samples, burn_in)
            tg_marginals, ty_marginals = self.model_builder.get_triaging_marginals_over_time(distributions, test_set)
            tg_baseline_frequencies, ty_baseline_frequencies = self.model_builder.get_triaging_normalized_frequencies(
                training_set)

            tg_baseline_log_loss, ty_baseline_log_loss = self.compute_accuracy_for_triaging(tg_baseline_frequencies,
                                                                                            ty_baseline_frequencies,
                                                                                            test_set)
            tg_model_log_loss, ty_model_log_loss = self.compute_accuracy_for_triaging(tg_marginals, ty_marginals,
                                                                                      test_set)

            tg_baseline_log_losses.append(tg_baseline_log_loss)
            ty_baseline_log_losses.append(ty_baseline_log_loss)
            tg_model_log_losses.append(tg_model_log_loss)
            ty_model_log_losses.append(ty_model_log_loss)

        baseline_filename_suffix = 'baseline_{}dp_{}s_{}bi_{}f'.format(evidence_set.number_of_data_points,
                                                                       number_of_samples, burn_in, number_of_folds)
        filepath = '{}/{}.txt'.format(evaluation_folder, baseline_filename_suffix)
        combined_log_loss = np.array([tg_baseline_log_losses, ty_baseline_log_losses])
        np.savetxt(filepath, combined_log_loss)

        model_filename_suffix = 'model_{}dp_{}s_{}bi_{}f'.format(evidence_set.number_of_data_points,
                                                                 number_of_samples, burn_in, number_of_folds)
        filepath = '{}/{}.txt'.format(evaluation_folder, model_filename_suffix)
        combined_log_loss = np.array([tg_model_log_losses, ty_model_log_losses])
        np.savetxt(filepath, combined_log_loss)

        print('TG')
        print('Baseline: {}'.format(np.mean(tg_baseline_log_loss)))
        print('Model: {}'.format(np.mean(tg_model_log_loss)))

        print('TY')
        print('Baseline: {}'.format(np.mean(ty_baseline_log_loss)))
        print('Model: {}'.format(np.mean(ty_model_log_loss)))

    def shuffle_and_split_data(self, evidence_set, number_of_folds):
        full_data = np.hstack(
            [evidence_set.lt_evidence, evidence_set.rm_evidence, evidence_set.tg_evidence, evidence_set.ty_evidence])
        kf = KFold(number_of_folds, shuffle=True)

        folds = []

        for train_index, test_index in kf.split(full_data):
            lt_train = full_data[train_index, 0:evidence_set.time_slices]
            lt_test = full_data[test_index, 0:evidence_set.time_slices]

            rm_train = full_data[train_index, evidence_set.time_slices:2 * evidence_set.time_slices]
            rm_test = full_data[test_index, evidence_set.time_slices:2 * evidence_set.time_slices]

            tg_train = full_data[train_index, 2 * evidence_set.time_slices:3 * evidence_set.time_slices]
            tg_test = full_data[test_index, 2 * evidence_set.time_slices:3 * evidence_set.time_slices]

            ty_train = full_data[train_index, 3 * evidence_set.time_slices:4 * evidence_set.time_slices]
            ty_test = full_data[test_index, 3 * evidence_set.time_slices:4 * evidence_set.time_slices]

            train_set = data_processing.EvidenceDataSet(lt_train, rm_train, tg_train, ty_train, None, None)
            test_set = data_processing.EvidenceDataSet(lt_test, rm_test, tg_test, ty_test, None, None)
            folds.append((train_set, test_set))

        return folds

    def compute_accuracy_for_triaging(self, tg_marginals, ty_marginals, validation_set):
        """
        We only want to compute the accuracy for TG and TY at this point
        """
        tg_log_loss = 0
        ty_log_loss = 0

        # ylog(p(y)) + (1-y)log(1-p(y))
        for d in range(validation_set.number_of_data_points):
            if len(tg_marginals.shape) == 3:
                tg_log_loss += self.get_log_loss(validation_set.tg_evidence[d], tg_marginals[d])
                ty_log_loss += self.get_log_loss(validation_set.ty_evidence[d], ty_marginals[d])
            else:
                tg_log_loss += self.get_log_loss(validation_set.tg_evidence[d], tg_marginals)
                ty_log_loss += self.get_log_loss(validation_set.ty_evidence[d], ty_marginals)

        return tg_log_loss / evidence_set.number_of_data_points, ty_log_loss / evidence_set.number_of_data_points

    def get_log_loss(self, evidence, marginals):
        log_marginals = log(marginals)
        skip_first_evidence = evidence[1:]
        return -np.mean(skip_first_evidence * log_marginals[:, 1] + (1 - skip_first_evidence) * log_marginals[:, 0])


def predict_with_sum_product():
    T = np.array([[0.3, 0.4, 0.3], [0.2, 0.5, 0.3], [0.1, 0.4, 0.5]])
    Ea = np.array([[0.2, 0.8], [0.3, 0.7], [0.1, 0.9]])
    Eb = np.array([[0.5, 0.5], [0.1, 0.9], [0.4, 0.6]])

    evidence_a = [-1, 0, 1, 1, 1, 0, 0, 0]
    evidence_b = [-1, 1, 1, 0, 0, 1, 1, 0]

    times = 8

    probs = np.zeros((times - 1, 2))
    partials = []
    p = np.array(T[0] * Ea[:, evidence_a[1]] * Eb[:, evidence_b[1]])
    partials.append(p / np.sum(p))
    for i in range(2, times):
        p = T * Ea[:, evidence_a[i]] * Eb[:, evidence_b[i]]
        partials.append(p / np.sum(p, axis=1).reshape(-1, 1))

    temp = np.stack([np.sum(T * Ea[:, col], axis=1) for col in range(Ea.shape[1])])

    probs[0] = np.sum(T[0] * Ea.T, axis=1)
    probs[0] /= np.sum(probs[0])
    probs[1] = np.sum(partials[0] * temp, axis=1)
    probs[1] /= np.sum(probs[1])

    for t in range(2, times - 1):
        temp2 = temp.copy()
        for j in range(t - 1, 0, -1):
            temp2 = np.stack([np.sum(partials[j] * temp2[row], axis=1) for row in range(Ea.shape[1])])
            temp2 /= np.sum(temp2, axis=0)
        probs[t] = np.sum(partials[0] * temp2, axis=1)
        probs[t] /= np.sum(probs[t])

    print(probs)

    # partials.append(T[0] * Ea.T)
    # partials.append(T[0] * Ea[:,evidence_a[1]] * Eb[:,evidence_b[1]])
    # partials.append(np.sum(np.stack([np.sum(T * Ea[:,evidence_a[2]] * Eb[:,evidence_b[2]] * Ea[:,col], axis=1) for col in range(Ea.shape[1])], axis=1)))
    #
    # temp = np.stack([np.sum(T * Ea[:,col], axis=1) for col in range(Ea.shape[1])])
    #
    # print(np.sum(partials[0], axis=1))
    # print(np.sum(partials[1]*temp, axis=1))
    # print(partials[2])
    # print(np.sum(partials[2] * temp, axis=1))

    # for t in range(1,3):
    #     if t-1 == 0:
    #         prob = np.sum(partials[t-1], axis=1)
    #         print(prob)
    #     else:
    #         prob = np.sum(partials[t-1]*temp, axis=1)
    #         print(prob)
    #         # partials.append(T[0] * Ea[:,evidence_a[t-1]] * Eb[:,evidence_b[t-1]])
    #         # prob = np.sum

    # pred_a1 = np.sum(T[0][:, np.newaxis] * Ea, axis=0)
    # print(T[0] * Ea[:,evidence_a[0]] * Eb[:,evidence_b[0]])
    # print(np.stack([np.sum(T * Ea[:,col], axis=1) for col in range(Ea.shape[1])]))
    # print(T[0] * Ea[:,evidence_a[0]] * Eb[:,evidence_b[0]] * np.stack([np.sum(T * Ea[:,col], axis=1) for col in range(Ea.shape[1])]))
    # pred_a2 = np.sum(T[0] * Ea[:,evidence_a[1]] * Eb[:,evidence_b[1]] * np.stack([np.sum(T * Ea[:,col], axis=1) for col in range(Ea.shape[1])]), axis=1)
    # print(pred_a2)
    # print(pred_a2/np.sum(pred_a2))

    # pred_a3 = np.sum(T[0] * Ea[:,evidence_a[0]] * Eb[:,evidence_b[0]] * np.sum(T * Ea[:,evidence_a[1]] * Eb[:,evidence_b[1]] * np.stack([np.sum(T * Ea[:,col], axis=1) for col in range(Ea.shape[1])]), axis=1))
    partials = []
    # partials[0] = T[0] * Ea[:,evidence_a[0]] * Eb[:,evidence_b[0]]

    # for t in range(1,5):
    #     partials[t] =
    #
    #     print(T[0] * Ea[:,evidence_a[0]] * Eb[:,evidence_b[0]])
    # print(T * Ea[:,evidence_a[1]] * Eb[:,evidence_b[1]])
    # print(np.stack([np.sum(T * Ea[:,col], axis=1) for col in range(Ea.shape[1])], axis=1))


if __name__ == '__main__':
    NUMBER_OF_SAMPLES = 10
    BURN_IN = 0

    predict_with_sum_product()

    experimentation = Experimentation()
    # experimentation.estimate_parameters_on_full_data('data/evidence/internal', 'data/parameters/internal', NUMBER_OF_SAMPLES, BURN_IN)

    dp = data_processing.DataProcessing()
    evidence_set = dp.load_evidence_set('data/evidence/internal')
    experimentation.fit_and_evaluate(evidence_set, NUMBER_OF_SAMPLES, BURN_IN, 3, 'data/evaluation/internal')
