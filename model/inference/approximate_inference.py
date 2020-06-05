from inference.predictor import Predictor

class ApproximateInference:
    """
    In this class we receive observations from the current state and some of the next one and
    we aim to generate samples for the latent variables of the next T states so we can "predict"
    the future
    """

    def infer(self, pgm_metadata, current_time_slice, observations):
        predictor = Predictor()
        return predictor(pgm_metadata, current_time_slice, 0, observations)

