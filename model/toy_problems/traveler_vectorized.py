import numpy as np
from scipy.stats import dirichlet, beta
from tqdm import tqdm

NUMBER_OF_CITIES = 4
# NUMBER_OF_ROOMS = 3
ZERO = 10 ** -16
TIME_SLICES = 20


def generate_synthetic_data(number_of_data_points, time_slices):
    transition_matrix = np.array([[0, 1, 0, 0],
                                  [1 / 3, 0, 1 / 3, 1 / 3],
                                  [0, 0.5, 0, 0.5],
                                  [0, 0.5, 0.5, 0]])

    emission_matrix_a = np.array([[0.15, 0.85],
                                  [0.75, 0.25],
                                  [0.75, 0.25],
                                  [0.75, 0.25]])

    emission_matrix_b = np.array([[0.75, 0.25],
                                  [0.15, 0.85],
                                  [0.75, 0.25],
                                  [0.75, 0.25]])

    emission_matrix_c = np.array([[0.75, 0.25],
                                  [0.75, 0.25],
                                  [0.15, 0.85],
                                  [0.75, 0.25]])

    emission_matrix_d = np.array([[0.75, 0.25],
                                  [0.75, 0.25],
                                  [0.75, 0.25],
                                  [0.15, 0.85]])
    cities = np.zeros((number_of_data_points, time_slices), dtype=np.int8)
    sensors_a = np.zeros((number_of_data_points, time_slices), dtype=np.int8)
    sensors_b = np.zeros((number_of_data_points, time_slices), dtype=np.int8)
    sensors_c = np.zeros((number_of_data_points, time_slices), dtype=np.int8)
    sensors_d = np.zeros((number_of_data_points, time_slices), dtype=np.int8)

    for d in tqdm(range(number_of_data_points), desc='Generating data'):
        cities[d][0] = 0
        sensors_a[d][0] = -1
        sensors_b[d][0] = -1
        sensors_c[d][0] = -1
        sensors_d[d][0] = -1
        for t in range(1, time_slices):
            city = np.random.choice(NUMBER_OF_CITIES, p=transition_matrix[cities[d][t - 1]])
            cities[d][t] = city
            sensors_a[d][t] = np.random.choice(2, p=emission_matrix_a[city])
            sensors_b[d][t] = np.random.choice(2, p=emission_matrix_b[city])
            sensors_c[d][t] = np.random.choice(2, p=emission_matrix_c[city])
            sensors_d[d][t] = np.random.choice(2, p=emission_matrix_d[city])

    np.save('../data/cities.npy', cities)
    np.save('../data/sensors_a.npy', sensors_a)
    np.save('../data/sensors_b.npy', sensors_b)
    np.save('../data/sensors_c.npy', sensors_c)
    np.save('../data/sensors_d.npy', sensors_d)


def estimate_parameters(number_of_samples, burn_in):
    theta_city_prior = np.array([[ZERO, 1, ZERO, ZERO],  # Dirichlet
                                 [1, ZERO, 1, 1],
                                 [ZERO, 1, ZERO, 1],
                                 [ZERO, 1, 1, ZERO]])

    pi_sa_prior = np.array([[3, 1],  # Beta
                            [1, 3],
                            [1, 3],
                            [1, 3]])

    pi_sb_prior = np.array([[1, 3],  # Beta
                            [3, 1],
                            [1, 3],
                            [1, 3]])

    pi_sc_prior = np.array([[1, 3],  # Beta
                            [1, 3],
                            [3, 1],
                            [1, 3]])

    pi_sd_prior = np.array([[1, 3],  # Beta
                            [1, 3],
                            [1, 3],
                            [3, 1]])

    cities_evidence = np.load('../data/cities.npy')
    sensors_a_evidence = np.load('../data/sensors_a.npy')
    sensors_b_evidence = np.load('../data/sensors_b.npy')
    sensors_c_evidence = np.load('../data/sensors_c.npy')
    sensors_d_evidence = np.load('../data/sensors_d.npy')

    number_of_data_points, time_slices = cities_evidence.shape

    theta_city_samples = np.zeros((number_of_samples, NUMBER_OF_CITIES, NUMBER_OF_CITIES))
    pi_sa_samples = np.zeros((number_of_samples, NUMBER_OF_CITIES, 2))
    pi_sb_samples = np.zeros((number_of_samples, NUMBER_OF_CITIES, 2))
    pi_sc_samples = np.zeros((number_of_samples, NUMBER_OF_CITIES, 2))
    pi_sd_samples = np.zeros((number_of_samples, NUMBER_OF_CITIES, 2))

    for i in tqdm(range(number_of_samples + burn_in), desc='Estimating parameters'):
        posterior_theta_city = theta_city_prior
        posterior_pi_sa = pi_sa_prior
        posterior_pi_sb = pi_sb_prior
        posterior_pi_sc = pi_sc_prior
        posterior_pi_sd = pi_sd_prior
        for t in range(1, time_slices):
            for d in range(number_of_data_points):
                # Incrementing the prior parameters
                posterior_theta_city[cities_evidence[d][t - 1]][cities_evidence[d][t]] += 1
                posterior_pi_sa[cities_evidence[d][t]][1 - sensors_a_evidence[d][t]] += 1
                posterior_pi_sb[cities_evidence[d][t]][1 - sensors_b_evidence[d][t]] += 1
                posterior_pi_sc[cities_evidence[d][t]][1 - sensors_c_evidence[d][t]] += 1
                posterior_pi_sd[cities_evidence[d][t]][1 - sensors_d_evidence[d][t]] += 1

        # Sample parameters
        theta_city_sample = np.zeros((NUMBER_OF_CITIES, NUMBER_OF_CITIES))
        pi_sa_sample = np.zeros((NUMBER_OF_CITIES, 2))
        pi_sb_sample = np.zeros((NUMBER_OF_CITIES, 2))
        pi_sc_sample = np.zeros((NUMBER_OF_CITIES, 2))
        pi_sd_sample = np.zeros((NUMBER_OF_CITIES, 2))

        for s in range(NUMBER_OF_CITIES):
            theta_city_sample[s] = dirichlet(posterior_theta_city[s]).rvs()[0]

            sample = beta(*posterior_pi_sa[s]).rvs()
            pi_sa_sample[s] = [1 - sample, sample]

            sample = beta(*posterior_pi_sb[s]).rvs()
            pi_sb_sample[s] = [1 - sample, sample]

            sample = beta(*posterior_pi_sc[s]).rvs()
            pi_sc_sample[s] = [1 - sample, sample]

            sample = beta(*posterior_pi_sd[s]).rvs()
            pi_sd_sample[s] = [1 - sample, sample]

        # Sample City
        posterior_city = np.zeros((number_of_data_points, NUMBER_OF_CITIES))
        for t in range(1, time_slices):
            posterior_city += log(theta_city_sample[cities_evidence[:, t - 1]]) + log(
                pi_sa_sample[:, sensors_a_evidence[:, t]]).T + log(
                pi_sb_sample[:, sensors_b_evidence[:, t]]).T + log(
                pi_sc_sample[:, sensors_c_evidence[:, t]]).T + log(pi_sd_sample[:, sensors_d_evidence[:, t]].T)

            max_value_per_row = np.max(posterior_city, axis=1)[:, np.newaxis]
            posterior_city -= max_value_per_row
            posterior_city = np.exp(posterior_city)
            posterior_city /= np.sum(posterior_city, axis=1)[:, np.newaxis]

            for d, posterior in enumerate(posterior_city):
                cities_evidence[d,t] = np.random.choice(NUMBER_OF_CITIES, p=posterior)

        if i >= burn_in:
            theta_city_samples[i - burn_in] = theta_city_sample
            pi_sa_samples[i - burn_in] = pi_sa_sample
            pi_sb_samples[i - burn_in] = pi_sb_sample
            pi_sc_samples[i - burn_in] = pi_sc_sample
            pi_sd_samples[i - burn_in] = pi_sd_sample

    np.save('../data/theta_city.npy', np.mean(theta_city_samples, axis=0))
    np.save('../data/pi_sa.npy', np.mean(pi_sa_samples, axis=0))
    np.save('../data/pi_sb.npy', np.mean(pi_sb_samples, axis=0))
    np.save('../data/pi_sc.npy', np.mean(pi_sc_samples, axis=0))
    np.save('../data/pi_sd.npy', np.mean(pi_sd_samples, axis=0))

def log(values):
    values[values == 0] = ZERO
    return np.log(values)

if __name__ == '__main__':
    generate_synthetic_data(1000, TIME_SLICES)
    estimate_parameters(1500, 100)

    theta_city_samples = np.load('../data/theta_city.npy')
    pi_sa_samples = np.load('../data/pi_sa.npy')
    pi_sb_samples = np.load('../data/pi_sb.npy')
    pi_sc_samples = np.load('../data/pi_sc.npy')
    pi_sd_samples = np.load('../data/pi_sd.npy')

    print(theta_city_samples)
    print(pi_sa_samples)
    print(pi_sb_samples)
    print(pi_sc_samples)
    print(pi_sd_samples)
