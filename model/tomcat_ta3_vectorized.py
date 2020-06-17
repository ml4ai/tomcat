import numpy as np
from scipy.stats import dirichlet, beta, invgamma, norm
from tqdm import tqdm
import adapter.data_adapter as adapter

NUMBER_OF_STATES = 115
NUMBER_OF_ROOMS = len(adapter.ROOMS)
NUMBER_OF_GREEN_VICTIMS = 1
NUMBER_OF_YELLOW_VICTIMS = 1
ZERO = 10 ** -16
TIME_SLICES = 10


class DataProcessing():
    LT_EVIDENCE_FILENAME = 'lights'
    RM_EVIDENCE_FILENAME = 'rooms'
    TG_EVIDENCE_FILENAME = 'triaging_green'
    TY_EVIDENCE_FILENAME = 'triaging_yellow'
    DG_EVIDENCE_FILENAME = 'distance_green_{}'
    DY_EVIDENCE_FILENAME = 'distance_yellow_{}'

    THETA_S_FILENAME = 'theta_s'
    PI_LT_FILENAME = 'pi_lt'
    THETA_RM_FILENAME = 'theta_rm'
    PI_TG_FILENAME = 'pi_tg'
    PI_TY_FILENAME = 'pi_ty'
    SIGMA_DG_FILENAME = 'sigma_dg'
    SIGMA_DY_FILENAME = 'sigma_dy'

    def get_lt_evidence(self, evidence_folder):
        return np.load('{}/{}.npy'.format(evidence_folder, DataProcessing.LT_EVIDENCE_FILENAME))

    def get_rm_evidence(self, evidence_folder):
        return np.load('{}/{}.npy'.format(evidence_folder, DataProcessing.RM_EVIDENCE_FILENAME))

    def get_tg_evidence(self, evidence_folder):
        return np.load('{}/{}.npy'.format(evidence_folder, DataProcessing.TG_EVIDENCE_FILENAME))

    def get_ty_evidence(self, evidence_folder):
        return np.load('{}/{}.npy'.format(evidence_folder, DataProcessing.TY_EVIDENCE_FILENAME))

    def get_dg_evidence(self, evidence_folder, v):
        return np.load('{}/{}.npy'.format(evidence_folder, DataProcessing.DG_EVIDENCE_FILENAME.format(v)))

    def get_dy_evidence(self, evidence_folder, v):
        return np.load('{}/{}.npy'.format(evidence_folder, DataProcessing.DY_EVIDENCE_FILENAME.format(v)))

    def save_theta_s(self, parameter_folder, theta_s, number_of_data_points=None, number_of_samples=None, burn_in=None):
        filepath = self.get_parameter_filepath(parameter_folder, DataProcessing.THETA_S_FILENAME,
                                               number_of_data_points, number_of_samples, burn_in)
        np.save(filepath, theta_s)

    def save_pi_lt(self, parameter_folder, pi_lt, number_of_data_points=None, number_of_samples=None, burn_in=None):
        filepath = self.get_parameter_filepath(parameter_folder, DataProcessing.PI_LT_FILENAME,
                                               number_of_data_points, number_of_samples, burn_in)
        np.save(filepath, pi_lt)

    def save_theta_rm(self, parameter_folder, theta_rm, number_of_data_points=None, number_of_samples=None, burn_in=None):
        filepath = self.get_parameter_filepath(parameter_folder, DataProcessing.THETA_RM_FILENAME,
                                               number_of_data_points, number_of_samples, burn_in)
        np.save(filepath, theta_rm)

    def save_pi_tg(self, parameter_folder, pi_tg, number_of_data_points=None, number_of_samples=None, burn_in=None):
        filepath = self.get_parameter_filepath(parameter_folder, DataProcessing.PI_TG_FILENAME,
                                               number_of_data_points, number_of_samples, burn_in)
        np.save(filepath, pi_tg)

    def save_pi_ly(self, parameter_folder, pi_ty, number_of_data_points=None, number_of_samples=None, burn_in=None):
        filepath = self.get_parameter_filepath(parameter_folder, DataProcessing.PI_TY_FILENAME,
                                               number_of_data_points, number_of_samples, burn_in)
        np.save(filepath, pi_ty)

    def save_sigma_dg(self, parameter_folder, sigma_dg, number_of_data_points=None, number_of_samples=None, burn_in=None):
        filepath = self.get_parameter_filepath(parameter_folder, DataProcessing.SIGMA_DG_FILENAME,
                                               number_of_data_points, number_of_samples, burn_in)
        np.save(filepath, sigma_dg)

    def save_sigma_dy(self, parameter_folder, sigma_dy, number_of_data_points=None, number_of_samples=None, burn_in=None):
        filepath = self.get_parameter_filepath(parameter_folder, DataProcessing.SIGMA_DY_FILENAME,
                                               number_of_data_points, number_of_samples, burn_in)
        np.save(filepath, sigma_dy)

    def load_theta_s(self, parameter_folder, number_of_data_points=None, number_of_samples=None, burn_in=None):
        filepath = self.get_parameter_filepath(parameter_folder, DataProcessing.THETA_S_FILENAME,
                                               number_of_data_points, number_of_samples, burn_in)
        return np.load(filepath)

    def load_pi_lt(self, parameter_folder, number_of_data_points=None, number_of_samples=None, burn_in=None):
        filepath = self.get_parameter_filepath(parameter_folder, DataProcessing.PI_LT_FILENAME,
                                               number_of_data_points, number_of_samples, burn_in)
        return np.load(filepath)

    def load_theta_rm(self, parameter_folder, number_of_data_points=None, number_of_samples=None, burn_in=None):
        filepath = self.get_parameter_filepath(parameter_folder, DataProcessing.THETA_RM_FILENAME,
                                               number_of_data_points, number_of_samples, burn_in)
        return np.load(filepath)

    def load_pi_tg(self, parameter_folder, number_of_data_points=None, number_of_samples=None, burn_in=None):
        filepath = self.get_parameter_filepath(parameter_folder, DataProcessing.PI_TG_FILENAME,
                                               number_of_data_points, number_of_samples, burn_in)
        return np.load(filepath)

    def load_pi_ly(self, parameter_folder, number_of_data_points=None, number_of_samples=None, burn_in=None):
        filepath = self.get_parameter_filepath(parameter_folder, DataProcessing.PI_TY_FILENAME,
                                               number_of_data_points, number_of_samples, burn_in)
        return np.load(filepath)

    def load_sigma_dg(self, parameter_folder, number_of_data_points=None, number_of_samples=None, burn_in=None):
        filepath = self.get_parameter_filepath(parameter_folder, DataProcessing.SIGMA_DG_FILENAME,
                                               number_of_data_points, number_of_samples, burn_in)
        return np.load(filepath)

    def load_sigma_dy(self, parameter_folder, number_of_data_points=None, number_of_samples=None, burn_in=None):
        filepath = self.get_parameter_filepath(parameter_folder, DataProcessing.SIGMA_DY_FILENAME,
                                               number_of_data_points, number_of_samples, burn_in)
        return np.load(filepath)

    def get_parameter_filepath(self, parameter_folder, parameter_filename, number_of_data_points=None,
                               number_of_samples=None, burn_in=None):
        if number_of_data_points == None:
            return '{}/{}.npy'.format(parameter_folder, parameter_filename)
        else:
            return '{}/{}-{}dp-{s}-{}b.npy'.format(parameter_folder, parameter_filename, number_of_data_points,
                                               number_of_samples, burn_in)


class ModelDistribution:

    def get_pi_ty(self):
        pi_ty = np.zeros(NUMBER_OF_STATES, 2)

        # No triaging in a hallway
        for state in range(7):
            pi_ty[state] = [1, ZERO]

        for state in range(7, NUMBER_OF_STATES):
            # P(TY|State) = 1 if State = Triaging Yellow, 0 otherwise
            pi_ty[state] = [ZERO, 1] if int((state - 9) % 6) == 0 else [1, ZERO]

        return pi_ty

    def get_pi_tg(self):
        pi_tg = np.zeros(NUMBER_OF_STATES, 2)

        # No triaging in a hallway
        for state in range(7):
            pi_tg[state] = [1, ZERO]

        for state in range(7, NUMBER_OF_STATES):
            # P(TG|State) = 1 if State = Triaging Green, 0 otherwise
            pi_tg[state] = [ZERO, 1] if int((state - 8) % 6) == 0 else [1, ZERO]

        return pi_tg

    def get_theta_rm(self):
        """
        Predefined distribution because we can always tell the room by the state
        """

        # In the hallways, the nuber of the room in the number of the state
        theta_rm = np.zeros(NUMBER_OF_STATES, NUMBER_OF_ROOMS)
        for state in range(7):
            theta_rm[state] = self.one_hot_encode(NUMBER_OF_ROOMS, [state])

        for s in range(7, NUMBER_OF_STATES):
            room_index = int((s - 7) / 6) + 7
            theta_rm[state] = self.one_hot_encode(NUMBER_OF_ROOMS, [room_index])

        return theta_rm

    def get_sigma_priors(self):
        return np.ones((NUMBER_OF_STATES, 2))

    def get_pi_lt_priors(self):
        priors = np.zeros((NUMBER_OF_STATES, 2))

        # Lights in the hallway states (exception below) are always on
        priors[0:7, 0] = ZERO
        priors[0:7, 1] = 1

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
        priors = np.array((6, NUMBER_OF_STATES))

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


class ModelBuilder:

    def __init__(self):
        self.distribution = ModelDistribution()
        self.data_processing = DataProcessing()

    def estimate_parameters(self, evidence_folder, number_of_samples, burn_in):
        theta_s_priors = self.distribution.get_theta_s_priors()  # P(theta_s|state = s) one per row
        pi_lt_priors = self.distribution.get_pi_lt_priors()  # P(pi_lt|state = s) one per row
        sigma_dg_priors = np.zeros((NUMBER_OF_GREEN_VICTIMS, NUMBER_OF_STATES, 2))
        for v in range(NUMBER_OF_GREEN_VICTIMS):
            sigma_dg_priors[v] = self.distribution.get_sigma_priors()  # for victim v: P(sigma_dg|state = s) one per row
        sigma_dy_priors = np.zeros((NUMBER_OF_YELLOW_VICTIMS, NUMBER_OF_STATES, 2))
        for v in range(NUMBER_OF_YELLOW_VICTIMS):
            sigma_dy_priors[v] = self.distribution.get_sigma_priors()  # for victim v: P(sigma_dg|state = s) one per row

        lt_evidence = self.data_processing.get_lt_evidence(evidence_folder)
        rm_evidence = self.data_processing.get_lt_evidence(evidence_folder)
        tg_evidence = self.data_processing.get_lt_evidence(evidence_folder)
        ty_evidence = self.data_processing.get_lt_evidence(evidence_folder)

        number_of_data_points, time_slices = lt_evidence.shape
        dg_evidence = np.zeros((NUMBER_OF_GREEN_VICTIMS, number_of_data_points, time_slices))
        for v in range(NUMBER_OF_GREEN_VICTIMS):
            dg_evidence[v] = self.data_processing.get_dg_evidence(evidence_folder, v)
        dy_evidence = np.zeros((NUMBER_OF_YELLOW_VICTIMS, number_of_data_points, time_slices))
        for v in range(NUMBER_OF_YELLOW_VICTIMS):
            dy_evidence[v] = self.data_processing.get_dy_evidence(evidence_folder, v)

        # Create tensors to store the samples
        theta_s_samples = np.zeros((number_of_samples, NUMBER_OF_STATES, NUMBER_OF_STATES))
        pi_lt_samples = np.zeros((number_of_samples, NUMBER_OF_STATES, 2))
        sigma_dg_samples = np.zeros((NUMBER_OF_GREEN_VICTIMS, number_of_samples, NUMBER_OF_STATES, 1))
        sigma_dy_samples = np.zeros((NUMBER_OF_YELLOW_VICTIMS, number_of_samples, NUMBER_OF_STATES, 1))

        # Sample S using ancestral sampling to begin with
        state_sample = self.get_initial_state_sample(theta_s_priors, number_of_data_points, time_slices)

        # Fixed distributions
        theta_rm = self.distribution.get_theta_rm()
        pi_tg = self.distribution.get_pi_tg()
        pi_ty = self.distribution.get_pi_ty()

        for i in tqdm(range(number_of_samples + burn_in), desc='Estimating parameters'):
            posteriors_theta_s = theta_s_priors
            posteriors_pi_lt = pi_lt_priors
            posteriors_sigma_dg = np.zeros((NUMBER_OF_GREEN_VICTIMS, NUMBER_OF_STATES, 2))
            for v in range(NUMBER_OF_GREEN_VICTIMS):
                posteriors_sigma_dg[v] = sigma_dg_priors[v]
            posteriors_sigma_dy = np.zeros((NUMBER_OF_YELLOW_VICTIMS, NUMBER_OF_STATES, 2))
            for v in range(NUMBER_OF_YELLOW_VICTIMS):
                posteriors_sigma_dy[v] = sigma_dy_priors[v]

            for t in range(1, time_slices):
                for d in range(number_of_data_points):
                    # Incrementing the prior parameters
                    posteriors_theta_s[state_sample[d][t - 1]][state_sample[d][t]] += 1
                    posteriors_pi_lt[state_sample[d][t]][1 - state_sample[d][t]] += 1
                    for v in range(NUMBER_OF_GREEN_VICTIMS):
                        posteriors_sigma_dg[v][state_sample[d][t]] += [1 / 2, dg_evidence[v][d][t] / 2]
                    for v in range(NUMBER_OF_YELLOW_VICTIMS):
                        posteriors_sigma_dy[v][state_sample[d][t]] += [1 / 2, dy_evidence[v][d][t] / 2]

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

            # Sample State
            posterior_state = np.zeros((number_of_data_points, NUMBER_OF_STATES))
            for t in range(1, time_slices):
                posterior_state += log(theta_s_sample[state_sample[:, t - 1]]) + self.log(
                    pi_lt_sample[:, lt_evidence[:, t]]).T + self.log(
                    theta_rm[:, rm_evidence[:, t]]).T + self.log(
                    pi_tg[:, tg_evidence[:, t]]).T + self.log(pi_ty[:, ty_evidence[:, t]].T)

                for v in range(NUMBER_OF_GREEN_VICTIMS):
                    gaussian = norm(0, sigma_dg_sample[v].flatten())
                    pdfs = gaussian.pdf([[distance] for distance in dg_evidence[:, t]])
                    posterior_state += self.log(pdfs)

                for v in range(NUMBER_OF_YELLOW_VICTIMS):
                    gaussian = norm(0, sigma_dy_sample[v].flatten())
                    pdfs = gaussian.pdf([[distance] for distance in dy_evidence[:, t]])
                    posterior_state += self.log(pdfs)

                max_value_per_row = np.max(posterior_city, axis=1)[:, np.newaxis]
                posterior_city -= max_value_per_row
                posterior_city = np.exp(posterior_city)
                posterior_city /= np.sum(posterior_city, axis=1)[:, np.newaxis]

                for d, posterior in enumerate(posterior_city):
                    state_sample[d, t] = np.random.choice(NUMBER_OF_STATES, p=posterior)

            if i >= burn_in:
                theta_s_samples[i - burn_in] = theta_s_sample
                pi_lt_samples[i - burn_in] = pi_lt_sample
                sigma_dg_samples[i - burn_in] = sigma_dg_sample
                sigma_dy_samples[i - burn_in] = sigma_dy_sample

        estimated_theta_s = np.mean(theta_s_samples, axis=0)
        estimated_pi_lt = np.mean(pi_lt_samples, axis=0)
        estimated_sigma_dg = np.zeros((NUMBER_OF_GREEN_VICTIMS, NUMBER_OF_STATES, 1))
        for v in range(NUMBER_OF_GREEN_VICTIMS):
            estimated_sigma_dg[v] = np.mean(sigma_dg_samples[v], axis=0)
        estimated_sigma_dy = np.zeros((NUMBER_OF_YELLOW_VICTIMS, NUMBER_OF_STATES, 1))
        for v in range(NUMBER_OF_YELLOW_VICTIMS):
            estimated_sigma_dy[v] = np.mean(sigma_dy_samples[v], axis=0)

        self.data_processing.save_theta_s(estimated_theta_s)
        self.data_processing.save_pi_lt(estimated_pi_lt)
        self.data_processing.save_sigma_dg(estimated_sigma_dg)
        self.data_processing.save_sigma_dy(estimated_sigma_dy)

    def log(values):
        values[values == 0] = ZERO
        return np.log(values)

    def get_initial_state_sample(self, theta_s_priors, number_of_data_points, time_slices):
        state_samples = np.zeros((number_of_data_points, time_slices), dtype=np.int8)
        theta_s_sampled = np.zeros(NUMBER_OF_STATES, NUMBER_OF_STATES)
        for state in range(NUMBER_OF_STATES):
            theta_s_sampled[state] = dirichlet(theta_s_priors).rvs()[0]

        for d in range(number_of_data_points):
            state_samples[d][0] = 0
            for t in range(1, time_slices):
                state = np.random.choice(NUMBER_OF_STATES, p=theta_s_sampled[state_samples[d][t - 1]])
                state_samples[d][t] = state

        return state_samples

if __name__ == '__main__':
    builder = ModelBuilder()

    builder.estimate_parameters('data/input/evidence/testbed', 1000, 100)
