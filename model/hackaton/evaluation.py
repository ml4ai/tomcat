import numpy as np
from sklearn.model_selection import KFold
from hackaton.evidence_extraction import EvidenceSet, MissionMap, load_evidence_set
from hackaton.learning import ParameterLearning
from hackaton.inference import ModelInference
from hackaton import utils as utils
from hackaton.model_representation import Model
import random

def fit_and_evaluate(original_model, evidence_set, number_of_samples, burn_in, number_of_folds):
    """
    This function fits and evaluates the model using k-fold cross validation. For each fold, the training set is
    used ti estimate the parameters of the model and the test set is then used to estimate the one-time-step-ahead
    marginals all time steps for the nodes TG and TY. Then, the estimated marginals are compared against the true values
    in the test set using log-loss. We also use log-loss to compare the relative frequencies of the values obtained
    for TG and TY over the training data for each time step with the test set, and we use this as a baseline to
    compare with the ToMCAT model.
    """
    folds = shuffle_and_split_data(evidence_set, number_of_folds)
    learning = ParameterLearning(original_model)

    tg_baseline_log_losses = []
    ty_baseline_log_losses = []
    tg_model_log_losses = []
    ty_model_log_losses = []

    for training_set, test_set in folds:
        trained_model = learning.estimate_parameters(training_set, number_of_samples, burn_in)
        inference = ModelInference(trained_model)
        tg_marginals, ty_marginals = inference.get_triaging_marginals_over_time(test_set)
        tg_baseline_frequencies, ty_baseline_frequencies = get_triaging_normalized_frequencies(training_set)

        tg_baseline_log_loss, ty_baseline_log_loss = compute_accuracy_for_triaging(tg_baseline_frequencies,
                                                                                   ty_baseline_frequencies,
                                                                                   test_set)
        tg_model_log_loss, ty_model_log_loss = compute_accuracy_for_triaging(tg_marginals, ty_marginals, test_set)

        tg_baseline_log_losses.append(tg_baseline_log_loss)
        ty_baseline_log_losses.append(ty_baseline_log_loss)
        tg_model_log_losses.append(tg_model_log_loss)
        ty_model_log_losses.append(ty_model_log_loss)

    return np.array([tg_baseline_log_losses, ty_baseline_log_losses]), np.array(
        [tg_model_log_losses, ty_model_log_losses])

def shuffle_and_split_data(evidence_set, number_of_folds):
    """
    This function shuffles and splits data into folds comprised of training and test data
    """
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

        train_set = EvidenceSet(lt_train, rm_train, tg_train, ty_train)
        test_set = EvidenceSet(lt_test, rm_test, tg_test, ty_test)
        folds.append((train_set, test_set))

    return folds

def get_triaging_normalized_frequencies(evidence_set):
    """
    This function returns the normalized frequencies for the values of TG and TY observed in a data set.
    This will be used as baseline for comparison with the ToMCAT model
    """
    tg_frequencies = np.stack(
        [np.sum(1 - evidence_set.tg_evidence[:, 1:], axis=0), np.sum(evidence_set.tg_evidence[:, 1:], axis=0)],
        axis=1)
    tg_frequencies = tg_frequencies / np.sum(tg_frequencies, axis=1).reshape(-1, 1)

    ty_frequencies = np.stack(
        [np.sum(1 - evidence_set.ty_evidence[:, 1:], axis=0), np.sum(evidence_set.ty_evidence[:, 1:], axis=0)],
        axis=1)
    ty_frequencies = ty_frequencies / np.sum(ty_frequencies, axis=1).reshape(-1, 1)

    return tg_frequencies, ty_frequencies

def compute_accuracy_for_triaging(tg_probabilities, ty_probabilities, test_set):
    """
    We only want to compute the accuracy for TG and TY at this point
    """
    tg_log_loss = 0
    ty_log_loss = 0

    # ylog(p(y)) + (1-y)log(1-p(y))
    for d in range(test_set.number_of_data_points):
        if len(tg_probabilities.shape) == 3:
            tg_log_loss += get_log_loss(test_set.tg_evidence[d], tg_probabilities[d])
            ty_log_loss += get_log_loss(test_set.ty_evidence[d], ty_probabilities[d])
        else:
            tg_log_loss += get_log_loss(test_set.tg_evidence[d], tg_probabilities)
            ty_log_loss += get_log_loss(test_set.ty_evidence[d], ty_probabilities)

    return tg_log_loss / test_set.number_of_data_points, ty_log_loss / test_set.number_of_data_points

def get_log_loss(evidence, probabilities):
    """
    This function computes the log loss between probabilities and real observed values
    """
    log_marginals = utils.log(probabilities)
    skip_first_evidence = evidence[1:]
    return -np.mean(skip_first_evidence * log_marginals[:, 1] + (1 - skip_first_evidence) * log_marginals[:, 0])

if __name__ == '__main__':
    NUMBER_OF_SAMPLES = 2
    BURN_IN = 1
    K = 4

    np.random.seed(42)
    random.seed(42)
    model = Model(MissionMap.SPARKY)
    evidence_set = load_evidence_set('../data/evidence/asist/sparky')
    baseline_ll, model_ll = fit_and_evaluate(model, evidence_set, NUMBER_OF_SAMPLES, BURN_IN, K)

    print('Log-loss: Baseline TG|TY')
    print(baseline_ll)
    print(np.mean(baseline_ll, axis=1))
    print('')
    print('Log-Loss: Model TG|TY')
    print(model_ll)
    print(np.mean(model_ll, axis=1))

