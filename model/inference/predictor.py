from model.pgm import PGM
from sampling.gibbs_sampling import GibbsSampling

class Predictor:
    """
    In this class we receive observations from the current state and some of the next one and
    we aim to generate samples for the latent variables of the next T states so we can "predict"
    the future
    """

    def predict(self, pgm_metadata, current_time_slice, time_slices_in_the_future, observations, number_of_samples=500, burn_in_periods=50):
        pgm = PGM(pgm_metadata, current_time_slice+time_slices_in_the_future)
        sampling = GibbsSampling(pgm)
        return sampling.sample(number_of_samples, burn_in_periods, observations)