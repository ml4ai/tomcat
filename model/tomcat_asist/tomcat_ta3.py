import numpy as np
from scipy.stats import dirichlet, beta, invgamma, norm
from tqdm import tqdm
import adapter.data_adapter as data_adapter
import tomcat_asist.data_processing as data_processing
import time

NUMBER_OF_STATES = 115
NUMBER_OF_ROOMS = len(data_adapter.ROOMS) - 1  # Removing Waiting Room from the list
NUMBER_OF_GREEN_VICTIMS = len(data_adapter.GREEN_VICTIMS)
NUMBER_OF_YELLOW_VICTIMS = len(data_adapter.YELLOW_VICTIMS)
ZERO = 10 ** -16


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

    def get_sigma_priors(self):
        return np.ones((NUMBER_OF_STATES, 2))


class ParameterPriors:

    def __init__(self, theta_s_priors, pi_lt_priors, sigma_dg_priors, sigma_dy_priors):
        self.theta_s_priors = theta_s_priors
        self.pi_lt_priors = pi_lt_priors
        self.sigma_dg_priors = sigma_dg_priors
        self.sigma_dy_priors = sigma_dy_priors


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
        self.sigma_dg = sigma_dg
        self.sigma_dy = sigma_dy


class PredictionSample:

    def __init__(self, states_sample, last_time_slice_lt_sample, last_time_slice_rm_sample, last_time_slice_tg_sample,
                 last_time_slice_ty_sample, last_time_slice_dg_sample, last_time_slice_dy_sample):
        self.states_sample = states_sample
        self.last_time_slice_lt_sample = last_time_slice_lt_sample
        self.last_time_slice_rm_sample = last_time_slice_rm_sample
        self.last_time_slice_tg_sample = last_time_slice_tg_sample
        self.last_time_slice_ty_sample = last_time_slice_ty_sample
        self.last_time_slice_dg_sample = last_time_slice_dg_sample
        self.last_time_slice_dy_sample = last_time_slice_dy_sample


class ModelBuilder:

    def estimate_parameters(self, evidence_set, number_of_samples, burn_in):
        # lt_evidence = evidence_set.lt_evidence
        # rm_evidence = evidence_set.rm_evidence
        # tg_evidence = evidence_set.tg_evidence
        # ty_evidence = evidence_set.ty_evidence
        # dg_evidence = evidence_set.dg_evidence
        # dy_evidence = evidence_set.dy_evidence

        parameter_priors = self.get_parameter_priors()

        # Create tensors to store the samples
        theta_s_samples = np.zeros((number_of_samples, NUMBER_OF_STATES, NUMBER_OF_STATES))
        pi_lt_samples = np.zeros((number_of_samples, NUMBER_OF_STATES, 2))
        sigma_dg_samples = np.zeros((number_of_samples, NUMBER_OF_GREEN_VICTIMS, NUMBER_OF_STATES, 1))
        sigma_dy_samples = np.zeros((number_of_samples, NUMBER_OF_YELLOW_VICTIMS, NUMBER_OF_STATES, 1))

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
            sigma_dg_samples[i] = distributions.sigma_dg
            sigma_dy_samples[i] = distributions.sigma_dy

        distributions.theta_s = np.mean(theta_s_samples, axis=0)
        distributions.pi_lt = np.mean(pi_lt_samples, axis=0)
        distributions.sigma_dg = np.mean(sigma_dg_samples, axis=0)
        distributions.sigma_dy = np.mean(sigma_dy_samples, axis=0)

        return distributions

    def get_parameter_priors(self):
        fixed_model_distributions = FixedModelDistributions()
        theta_s_priors = fixed_model_distributions.get_theta_s_priors()  # P(theta_s|state = s) one per row
        pi_lt_priors = fixed_model_distributions.get_pi_lt_priors()  # P(pi_lt|state = s) one per row
        sigma_dg_priors = np.zeros((NUMBER_OF_GREEN_VICTIMS, NUMBER_OF_STATES, 2))
        for v in range(NUMBER_OF_GREEN_VICTIMS):
            sigma_dg_priors[
                v] = fixed_model_distributions.get_sigma_priors()  # for victim v: P(sigma_dg|state = s) one per row
        sigma_dy_priors = np.zeros((NUMBER_OF_YELLOW_VICTIMS, NUMBER_OF_STATES, 2))
        for v in range(NUMBER_OF_YELLOW_VICTIMS):
            sigma_dy_priors[
                v] = fixed_model_distributions.get_sigma_priors()  # for victim v: P(sigma_dg|state = s) one per row

        return ParameterPriors(theta_s_priors, pi_lt_priors, sigma_dg_priors, sigma_dy_priors)

    def sample(self, evidence_set, parameter_priors, distributions, states_sample):
        posteriors_theta_s = parameter_priors.theta_s_priors
        posteriors_pi_lt = parameter_priors.pi_lt_priors
        posteriors_sigma_dg = np.zeros((NUMBER_OF_GREEN_VICTIMS, NUMBER_OF_STATES, 2))
        for v in range(NUMBER_OF_GREEN_VICTIMS):
            posteriors_sigma_dg[v] = parameter_priors.sigma_dg_priors[v]
        posteriors_sigma_dy = np.zeros((NUMBER_OF_YELLOW_VICTIMS, NUMBER_OF_STATES, 2))
        for v in range(NUMBER_OF_YELLOW_VICTIMS):
            posteriors_sigma_dy[v] = parameter_priors.sigma_dy_priors[v]

        for t in range(1, evidence_set.time_slices):
            for d in range(evidence_set.number_of_data_points):
                # Incrementing the prior parameters
                posteriors_theta_s[states_sample[d][t - 1]][states_sample[d][t]] += 1
                posteriors_pi_lt[states_sample[d][t]][1 - evidence_set.lt_evidence[d][t]] += 1
                for v in range(NUMBER_OF_GREEN_VICTIMS):
                    posteriors_sigma_dg[v][states_sample[d][t]] += [1 / 2, evidence_set.dg_evidence[v][d][t] / 2]
                for v in range(NUMBER_OF_YELLOW_VICTIMS):
                    posteriors_sigma_dy[v][states_sample[d][t]] += [1 / 2, evidence_set.dy_evidence[v][d][t] / 2]

        # Sample parameters
        ini = time.time()
        distributions.theta_s, distributions.pi_lt, distributions.sigma_dg, distributions.sigma_dy = self.sample_parameters(
            evidence_set, parameter_priors, states_sample)
        print("Time to sample parameters: {} seconds".format(time.time() - ini))
        ini = time.time()
        self.sample_states(evidence_set, distributions, states_sample)
        print("Time to sample states: {} seconds".format(time.time() - ini))

        return distributions, states_sample

        # theta_s_sample = np.zeros((NUMBER_OF_STATES, NUMBER_OF_STATES))
        # pi_lt_sample = np.zeros((NUMBER_OF_STATES, 2))
        # sigma_dg_sample = np.zeros((NUMBER_OF_GREEN_VICTIMS, NUMBER_OF_STATES, 1))
        # sigma_dy_sample = np.zeros((NUMBER_OF_YELLOW_VICTIMS, NUMBER_OF_STATES, 1))
        #
        # for state in range(NUMBER_OF_STATES):
        #     sample = dirichlet(posteriors_theta_s[state]).rvs()[0]
        #     theta_s_sample[state] = sample
        #
        #     sample = beta(*posteriors_pi_lt[state]).rvs()
        #     pi_lt_sample[state] = [1 - sample, sample]
        #
        #     for v in range(NUMBER_OF_GREEN_VICTIMS):
        #         sample = invgamma(*posteriors_sigma_dg[v][state]).rvs()
        #         sigma_dg_sample[v][state] = sample
        #
        #     for v in range(NUMBER_OF_YELLOW_VICTIMS):
        #         sample = invgamma(*posteriors_sigma_dy[v][state]).rvs()
        #         sigma_dy_sample[v][state] = sample

        # Sample State
        # posterior_state = np.zeros((number_of_data_points, NUMBER_OF_STATES))
        # for t in range(1, time_slices):
        #     posterior_state += self.log(theta_s_sample[states_sample[:, t - 1]]) + self.log(
        #         pi_lt_sample[:, lt_evidence[:, t]]).T + self.log(
        #         theta_rm[:, rm_evidence[:, t]]).T + self.log(
        #         pi_tg[:, tg_evidence[:, t]]).T + self.log(pi_ty[:, ty_evidence[:, t]].T)
        #
        #     for v in range(NUMBER_OF_GREEN_VICTIMS):
        #         gaussian = norm(0, sigma_dg_sample[v].flatten())
        #         pdfs = gaussian.pdf([[distance] for distance in dg_evidence[v][:, t]])
        #         posterior_state += self.log(pdfs)
        #
        #     for v in range(NUMBER_OF_YELLOW_VICTIMS):
        #         gaussian = norm(0, sigma_dy_sample[v].flatten())
        #         pdfs = gaussian.pdf([[distance] for distance in dy_evidence[v][:, t]])
        #         posterior_state += self.log(pdfs)
        #
        #     max_value_per_row = np.max(posterior_state, axis=1)[:, np.newaxis]
        #     posterior_state -= max_value_per_row
        #     posterior_state = np.exp(posterior_state)
        #     posterior_state /= np.sum(posterior_state, axis=1)[:, np.newaxis]
        #
        #     for d, posterior in enumerate(posterior_state):
        #         states_sample[d, t] = np.random.choice(NUMBER_OF_STATES, p=posterior)

    def sample_parameters(self, evidence_set, parameter_priors, state_sample):
        # Preprocessing to sample parameters faster
        posteriors_theta_s = parameter_priors.theta_s_priors
        posteriors_pi_lt = parameter_priors.pi_lt_priors
        posteriors_sigma_dg = np.zeros((NUMBER_OF_GREEN_VICTIMS, NUMBER_OF_STATES, 2))
        for v in range(NUMBER_OF_GREEN_VICTIMS):
            posteriors_sigma_dg[v] = parameter_priors.sigma_dg_priors[v]
        posteriors_sigma_dy = np.zeros((NUMBER_OF_YELLOW_VICTIMS, NUMBER_OF_STATES, 2))
        for v in range(NUMBER_OF_YELLOW_VICTIMS):
            posteriors_sigma_dy[v] = parameter_priors.sigma_dy_priors[v]

        for t in range(1, evidence_set.time_slices):
            for d in range(evidence_set.number_of_data_points):
                # Incrementing the prior parameters
                posteriors_theta_s[state_sample[d][t - 1]][state_sample[d][t]] += 1
                posteriors_pi_lt[state_sample[d][t]][1 - evidence_set.lt_evidence[d][t]] += 1
                for v in range(NUMBER_OF_GREEN_VICTIMS):
                    posteriors_sigma_dg[v][state_sample[d][t]] += [1 / 2, evidence_set.dg_evidence[v][d][t] / 2]
                for v in range(NUMBER_OF_YELLOW_VICTIMS):
                    posteriors_sigma_dy[v][state_sample[d][t]] += [1 / 2, evidence_set.dy_evidence[v][d][t] / 2]

        # Sample parameters
        theta_s_sample = np.zeros((NUMBER_OF_STATES, NUMBER_OF_STATES))
        pi_lt_sample = np.zeros((NUMBER_OF_STATES, 2))
        sigma_dg_sample = np.zeros((NUMBER_OF_GREEN_VICTIMS, NUMBER_OF_STATES, 1))
        sigma_dy_sample = np.zeros((NUMBER_OF_YELLOW_VICTIMS, NUMBER_OF_STATES, 1))

        for state in range(NUMBER_OF_STATES):
            sample = dirichlet(posteriors_theta_s[state]).rvs()[0]
            theta_s_sample[state] = sample

            sample = beta(*posteriors_pi_lt[state]).rvs()
            pi_lt_sample[state] = [1 - sample, sample]

            for v in range(NUMBER_OF_GREEN_VICTIMS):
                sample = invgamma(*posteriors_sigma_dg[v][state]).rvs()
                sigma_dg_sample[v][state] = sample

            for v in range(NUMBER_OF_YELLOW_VICTIMS):
                sample = invgamma(*posteriors_sigma_dy[v][state]).rvs()
                sigma_dy_sample[v][state] = sample

        return theta_s_sample, pi_lt_sample, sigma_dg_sample, sigma_dy_sample

    def sample_states(self, evidence_set, distributions, states_sample):
        posterior_state = np.zeros((evidence_set.number_of_data_points, NUMBER_OF_STATES))

        for t in range(1, evidence_set.time_slices):
            posterior_state += self.log(distributions.theta_s[states_sample[:, t - 1]]) + self.log(
                distributions.pi_lt[:, evidence_set.lt_evidence[:, t]]).T + self.log(
                distributions.theta_rm[:, evidence_set.rm_evidence[:, t]]).T + self.log(
                distributions.pi_tg[:, evidence_set.tg_evidence[:, t]]).T + self.log(
                distributions.pi_ty[:, evidence_set.ty_evidence[:, t]].T)

            for v in range(NUMBER_OF_GREEN_VICTIMS):
                gaussian = norm(0, distributions.sigma_dg[v].flatten())
                pdfs = gaussian.pdf([[distance] for distance in evidence_set.dg_evidence[v][:, t]])
                posterior_state += self.log(pdfs)

            for v in range(NUMBER_OF_YELLOW_VICTIMS):
                gaussian = norm(0, distributions.sigma_dy[v].flatten())
                pdfs = gaussian.pdf([[distance] for distance in evidence_set.dy_evidence[v][:, t]])
                posterior_state += self.log(pdfs)

            max_value_per_row = np.max(posterior_state, axis=1)[:, np.newaxis]
            posterior_state -= max_value_per_row
            posterior_state = np.exp(posterior_state)
            posterior_state /= np.sum(posterior_state, axis=1)[:, np.newaxis]

            for d, posterior in enumerate(posterior_state):
                states_sample[d, t] = np.random.choice(NUMBER_OF_STATES, p=posterior)

        return states_sample

    def log(self, values):
        values[values == 0] = ZERO
        return np.log(values)

    def get_initial_states_sample_from_parameters_priors(self, theta_s_priors, number_of_data_points, time_slices):
        state_samples = np.zeros((number_of_data_points, time_slices), dtype=np.int)
        theta_s_sampled = np.zeros((NUMBER_OF_STATES, NUMBER_OF_STATES))
        for state in range(NUMBER_OF_STATES):
            theta_s_sampled[state] = dirichlet(theta_s_priors[state]).rvs()[0]

        for d in range(number_of_data_points):
            sample = self.sample_states_over_time(theta_s_sampled, time_slices)
            state_samples[d] = sample
            # state_samples[d][0] = 0
            # for t in range(1, time_slices):
            #     state = np.random.choice(NUMBER_OF_STATES, p=theta_s_sampled[state_samples[d][t - 1]])
            #     state_samples[d][t] = state

        return state_samples

    def sample_states_over_time(self, theta_s, time_slices):
        states_sample = np.zeros(time_slices, dtype=np.int)

        for t in range(1, time_slices):
            states_sample[t] = np.random.choice(NUMBER_OF_STATES, p=theta_s[states_sample[t - 1]])

        return states_sample

    def predict(self, distributions, evidence_set, time_slice_to_predict, number_of_samples, burn_in):
        mini_evidence_set = self.truncate_evidence_set(evidence_set, time_slice_to_predict)
        prediction_sample = self.get_initial_samples_for_prediction(distributions, time_slice_to_predict)

        lt_samples = np.zeros((evidence_set.number_of_data_points, number_of_samples), dtype=np.int)
        rm_samples = np.zeros((evidence_set.number_of_data_points, number_of_samples), dtype=np.int)
        tg_samples = np.zeros((evidence_set.number_of_data_points, number_of_samples), dtype=np.int)
        ty_samples = np.zeros((evidence_set.number_of_data_points, number_of_samples), dtype=np.int)
        dg_samples = np.zeros((evidence_set.number_of_data_points, number_of_samples, NUMBER_OF_GREEN_VICTIMS))
        dy_samples = np.zeros((evidence_set.number_of_data_points, number_of_samples, NUMBER_OF_YELLOW_VICTIMS))

        for _ in range(burn_in):
            prediction_sample = self.sample_for_prediction(distributions, mini_evidence_set, time_slice_to_predict,
                                                           prediction_sample)

        for i in range(number_of_samples):
            prediction_sample = self.sample_for_prediction(distributions, mini_evidence_set, time_slice_to_predict,
                                                           prediction_sample)
            lt_samples[:, i] = prediction_sample.last_time_slice_lt_sample
            rm_samples[:, i] = prediction_sample.last_time_slice_rm_sample
            tg_samples[:, i] = prediction_sample.last_time_slice_tg_sample
            ty_samples[:, i] = prediction_sample.last_time_slice_ty_sample
            dg_samples[:, i] = prediction_sample.last_time_slice_dg_sample
            dy_samples[:, i] = prediction_sample.last_time_slice_dy_sample

        return lt_samples, rm_samples, tg_samples, ty_samples, dg_samples, dy_samples

    def sample_for_prediction(self, distributions, mini_evidence_set, time_slice_to_predict, prediction_sample):
        mini_evidence_set.lt_evidence[:, time_slice_to_predict] = prediction_sample.last_time_slice_lt_sample
        mini_evidence_set.rm_evidence[:, time_slice_to_predict] = prediction_sample.last_time_slice_rm_sample
        mini_evidence_set.tg_evidence[:, time_slice_to_predict] = prediction_sample.last_time_slice_tg_sample
        mini_evidence_set.ty_evidence[:, time_slice_to_predict] = prediction_sample.last_time_slice_ty_sample
        mini_evidence_set.dg_evidence[:, :, time_slice_to_predict] = prediction_sample.last_time_slice_dg_sample
        mini_evidence_set.dy_evidence[:, :, time_slice_to_predict] = prediction_sample.last_time_slice_dy_sample

        prediction_sample.states_sample = self.sample_states(mini_evidence_set, distributions,
                                                             prediction_sample.states_sample)
        lt_sample, rm_sample, tg_sample, ty_sample, dg_sample, dy_sample = self.sample_observable_nodes_at(
            time_slice_to_predict,
            distributions, prediction_sample.states_sample)
        prediction_sample.lt_sample = lt_sample
        prediction_sample.rm_sample = rm_sample
        prediction_sample.tg_sample = tg_sample
        prediction_sample.ty_sample = ty_sample
        prediction_sample.dg_sample = dg_sample
        prediction_sample.dy_sample = dy_sample

        return prediction_sample

    def get_initial_samples_for_prediction(self, distributions, time_slices):
        states_sample = self.sample_states_over_time(distributions.theta_s, time_slices+1)
        lt_sample, rm_sample, tg_sample, ty_sample, dg_sample, dy_sample = self.sample_observable_nodes_at(time_slices,
                                                                                                           distributions,
                                                                                                           states_sample)

        return PredictionSample(states_sample, lt_sample, rm_sample, tg_sample, ty_sample, dg_sample, dy_sample)

    def sample_observable_nodes_at(self, time_slice, distributions, states_sample):
        lt_sample = np.random.choice(2, p=distributions.pi_lt[states_sample[time_slice]])
        rm_sample = np.random.choice(NUMBER_OF_ROOMS, p=distributions.theta_rm[states_sample[time_slice]])
        tg_sample = np.random.choice(2, p=distributions.pi_tg[states_sample[time_slice]])
        ty_sample = np.random.choice(2, p=distributions.pi_ty[states_sample[time_slice]])
        dg_sample = np.abs(norm(0, distributions.sigma_dg[:, states_sample[time_slice]]).rvs())
        dy_sample = np.abs(norm(0, distributions.sigma_dy[:, states_sample[time_slice]]).rvs())

        return lt_sample, rm_sample, tg_sample, ty_sample, dg_sample, dy_sample

    def truncate_evidence_set(self, evidence_set, last_time_slice):
        lt_evidence = evidence_set.lt_evidence.copy()[:, 0:last_time_slice+1]
        rm_evidence = evidence_set.rm_evidence.copy()[:, 0:last_time_slice+1]
        tg_evidence = evidence_set.tg_evidence.copy()[:, 0:last_time_slice+1]
        ty_evidence = evidence_set.ty_evidence.copy()[:, 0:last_time_slice+1]
        dg_evidence = evidence_set.dg_evidence.copy()[:, :, 0:last_time_slice+1]
        dy_evidence = evidence_set.dy_evidence.copy()[:, :, 0:last_time_slice+1]

        return data_processing.EvidenceDataSet(lt_evidence, rm_evidence, tg_evidence, ty_evidence, dg_evidence,
                                               dy_evidence)


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
        dp.save_sigma_dg(parameters_folder, distributions.sigma_dg, filename_suffix)
        dp.save_sigma_dy(parameters_folder, distributions.sigma_dy, filename_suffix)

    def estimate_parameters_from_cv(self, evidence_folder, number_of_samples, burn_in, parameters_folder, k):
        # Cross Validation
        pass

    def predict_over_time(self, distributions, validation_set, number_of_samples, burn_in, predictions_folder, id):
        lt_samples = np.zeros((validation_set.number_of_data_points, number_of_samples, validation_set.time_slices - 2))
        rm_samples = np.zeros((validation_set.number_of_data_points, number_of_samples, validation_set.time_slices - 2))
        tg_samples = np.zeros((validation_set.number_of_data_points, number_of_samples, validation_set.time_slices - 2))
        ty_samples = np.zeros((validation_set.number_of_data_points, number_of_samples, validation_set.time_slices - 2))
        dg_samples = np.zeros((validation_set.number_of_data_points, NUMBER_OF_GREEN_VICTIMS, number_of_samples,
                               validation_set.time_slices - 2))
        dy_samples = np.zeros((validation_set.number_of_data_points, NUMBER_OF_YELLOW_VICTIMS, number_of_samples,
                               validation_set.time_slices - 2))

        for t in range(2, validation_set.time_slices):
            lt_samples[:, :, t], rm_samples[:, :, t], tg_samples[:, :, t], ty_samples[:, :, t], dg_samples[:, :, :,
                                                                                                t], dy_samples[:, :, :,
                                                                                                    t] = self.model_builder.predict(
                distributions, validation_set, t, number_of_samples, burn_in)

        filename_suffix = '{}dp_{}s_{}_{}bi'.format(validation_set.number_of_data_points, number_of_samples, burn_in,
                                                    id)
        data_processing.DataProcessing().save_predictions(predictions_folder, filename_suffix, lt_samples, rm_samples,
                                                          tg_samples, ty_samples, dg_samples, dy_samples)

    def compute_accuracy_for_triaging(self, tg_predictions, ty_predictions, validation_set):
        pass
        """
        We only want to compute the accuracy for TG and TY at this point
        """
        # ylog(p(y)) + (1-y)log(1-p(y))
        # for d in range(validation_set.number_of_data_points):
        #     samples_over_time = pd.DataFrame(tg_predictions[d])
        #
        # for t in range(2, validation_set.time_slices - 1):
        #     predictions = self.model_builder.predict(distributions, validation_set, t, number_of_samples, burn_in)
        #     # Diff between predictions.tg and validation_set.tg_evidence[t+1]


if __name__ == '__main__':
    NUMBER_OF_SAMPLES = 3
    BURN_IN = 1

    experimentation = Experimentation()
    # experimentation.estimate_parameters_on_full_data('data/evidence/internal', 'data/parameters/internal', 4, 0)

    dp = data_processing.DataProcessing()
    evidence_set = dp.load_evidence_set('data/evidence/internal')
    distributions = Distributions()

    filename_suffix = 'full_data_6dp_4s_0bi'
    distributions.theta_s = dp.load_theta_s('data/parameters/internal', filename_suffix)
    distributions.pi_lt = dp.load_pi_lt('data/parameters/internal', filename_suffix)
    distributions.sigma_dg = dp.load_sigma_dg('data/parameters/internal', filename_suffix)
    distributions.sigma_dy = dp.load_sigma_dy('data/parameters/internal', filename_suffix)

    experimentation.predict_over_time(distributions, evidence_set, NUMBER_OF_SAMPLES, BURN_IN,
                                      'data/predictions/internal', 0)





    # processing = DataProcessing()
    # processing.convert_experiments_data_to_evidence('data/experiments/internal/formatted', 'data/evidence/internal/')

    # np.random.seed(42)
    # random.seed(42)
    # builder = ModelBuilder()
    # builder.estimate_parameters('data/evidence/internal', 'data/parameters/internal', 100, 100)

    # processing = DataProcessing()
    # theta_s = processing.load_theta_s('data/parameters/internal', NUMBER_OF_DATA_POINTS, NUMBER_OF_SAMPLES, BURN_IN)
    # pi_lt = processing.load_pi_lt('data/parameters/internal', NUMBER_OF_DATA_POINTS, NUMBER_OF_SAMPLES, BURN_IN)
    # sigma_dg = processing.load_sigma_dg('data/parameters/internal', NUMBER_OF_DATA_POINTS, NUMBER_OF_SAMPLES, BURN_IN)
    # sigma_dy = processing.load_sigma_dy('data/parameters/internal', NUMBER_OF_DATA_POINTS, NUMBER_OF_SAMPLES, BURN_IN)
    #
    # print('Stop')
    #
    # self.data_processing.save_theta_s(parameters_folder, estimated_theta_s, number_of_data_points,
    #                                   number_of_samples, burn_in)
    # self.data_processing.save_pi_lt(parameters_folder, estimated_pi_lt, number_of_data_points, number_of_samples,
    #                                 burn_in)
    # self.data_processing.save_sigma_dg(parameters_folder, estimated_sigma_dg, number_of_data_points,
    #                                    number_of_samples, burn_in)
    # self.data_processing.save_sigma_dy(parameters_folder, estimated_sigma_dy, number_of_data_points,
    #                                    number_of_samples, burn_in)
