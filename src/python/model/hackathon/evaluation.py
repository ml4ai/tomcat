import numpy as np
from sklearn.model_selection import KFold
from hackathon.evidence_extraction import (
    EvidenceSet,
    MissionMap,
    load_evidence_set,
)
from hackathon.learning import ParameterLearning
from hackathon.inference import ModelInference
from hackathon import utils as utils
from hackathon.model_representation import Model
import random
from sklearn.metrics import confusion_matrix
import pandas as pd


def fit_and_evaluate(
    original_model, evidence_set, number_of_samples, burn_in, number_of_folds, horizons
):
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

    tg_baseline_log_losses = [[] for _ in range(len(horizons))]
    ty_baseline_log_losses = [[] for _ in range(len(horizons))]
    tg_baseline_confusion_matrices = [[] for _ in range(len(horizons))]
    ty_baseline_confusion_matrices = [[] for _ in range(len(horizons))]

    tg_model_log_losses = [[] for _ in range(len(horizons))]
    ty_model_log_losses = [[] for _ in range(len(horizons))]
    tg_model_confusion_matrices = [[] for _ in range(len(horizons))]
    ty_model_confusion_matrices = [[] for _ in range(len(horizons))]

    for training_set, test_set in folds:
        trained_model = learning.estimate_parameters(
            training_set, number_of_samples, burn_in
        )
        inference = ModelInference(trained_model)

        for i, h in enumerate(horizons):

            (
                tg_baseline_frequencies,
                ty_baseline_frequencies,
            ) = inference.get_triaging_normalized_frequencies(training_set, h)

            (
                tg_baseline_ll,
                ty_baseline_ll,
                tg_baseline_cm,
                ty_baseline_cm,
            ) = compute_accuracy_for_triaging(
                tg_baseline_frequencies, ty_baseline_frequencies, test_set, h
            )
            tg_baseline_log_losses[i].append(tg_baseline_ll)
            ty_baseline_log_losses[i].append(ty_baseline_ll)
            tg_baseline_confusion_matrices[i].append(tg_baseline_cm)
            ty_baseline_confusion_matrices[i].append(ty_baseline_cm)

            (
                tg_marginals,
                ty_marginals,
            ) = inference.get_triaging_marginals_over_time(test_set, h)
            (
                tg_model_ll,
                ty_model_ll,
                tg_model_cm,
                ty_model_cm,
            ) = compute_accuracy_for_triaging(tg_marginals, ty_marginals, test_set, h)
            tg_model_log_losses[i].append(tg_model_ll)
            ty_model_log_losses[i].append(ty_model_ll)
            tg_model_confusion_matrices[i].append(tg_model_cm)
            ty_model_confusion_matrices[i].append(ty_model_cm)

    print_results (
        tg_baseline_log_losses,
        ty_baseline_log_losses,
        tg_baseline_confusion_matrices,
        ty_baseline_confusion_matrices,
        tg_model_log_losses,
        ty_model_log_losses,
        tg_model_confusion_matrices,
        ty_model_confusion_matrices,
        horizons
    )

def shuffle_and_split_data(evidence_set, number_of_folds):
    """
    This function shuffles and splits data into folds comprised of training and test data
    """
    full_data = np.hstack(
        [
            evidence_set.lt_evidence,
            evidence_set.rm_evidence,
            evidence_set.tg_evidence,
            evidence_set.ty_evidence,
        ]
    )
    kf = KFold(number_of_folds, shuffle=True)

    folds = []

    for train_index, test_index in kf.split(full_data):
        lt_train = full_data[train_index, 0 : evidence_set.time_slices]
        lt_test = full_data[test_index, 0 : evidence_set.time_slices]

        rm_train = full_data[
            train_index,
            evidence_set.time_slices : 2 * evidence_set.time_slices,
        ]
        rm_test = full_data[
            test_index, evidence_set.time_slices : 2 * evidence_set.time_slices
        ]

        tg_train = full_data[
            train_index,
            2 * evidence_set.time_slices : 3 * evidence_set.time_slices,
        ]
        tg_test = full_data[
            test_index,
            2 * evidence_set.time_slices : 3 * evidence_set.time_slices,
        ]

        ty_train = full_data[
            train_index,
            3 * evidence_set.time_slices : 4 * evidence_set.time_slices,
        ]
        ty_test = full_data[
            test_index,
            3 * evidence_set.time_slices : 4 * evidence_set.time_slices,
        ]

        train_set = EvidenceSet(lt_train, rm_train, tg_train, ty_train)
        test_set = EvidenceSet(lt_test, rm_test, tg_test, ty_test)
        folds.append((train_set, test_set))

    return folds


def compute_accuracy_for_triaging(
    tg_probabilities, ty_probabilities, test_set, h=1
):
    """
    We only want to compute the accuracy for TG and TY at this point
    """
    tg_log_loss = 0
    ty_log_loss = 0
    confusion_matrix_tg = np.zeros((2, 2))
    confusion_matrix_ty = np.zeros((2, 2))

    for d in range(test_set.number_of_data_points):
        if len(tg_probabilities.shape) > 1:
            tg_data_point_probabilities = tg_probabilities[d]
            ty_data_point_probabilities = ty_probabilities[d]
        else:
            tg_data_point_probabilities = tg_probabilities
            ty_data_point_probabilities = ty_probabilities

        tg_evidence = test_set.tg_evidence[d][1:]
        ty_evidence = test_set.ty_evidence[d][1:]

        agg_tg_evidence = np.zeros(len(tg_evidence) - h + 1, dtype=np.int)
        agg_ty_evidence = np.zeros(len(tg_evidence) - h + 1, dtype=np.int)
        for i in range(len(tg_evidence) - h + 1):
            agg_tg_evidence[i] = tg_evidence[i]
            agg_ty_evidence[i] = ty_evidence[i]
            for j in range(h - 1):
                agg_tg_evidence[i] = agg_tg_evidence[i] | tg_evidence[i + j + 1]
                agg_ty_evidence[i] = agg_ty_evidence[i] | ty_evidence[i + j + 1]
                if agg_tg_evidence[i] & agg_ty_evidence[i]:
                    break

        # ylog(p(y)) + (1-y)log(1-p(y))
        tg_log_loss += get_log_loss(agg_tg_evidence, tg_data_point_probabilities)
        tg_log_loss /= test_set.number_of_data_points
        ty_log_loss += get_log_loss(agg_ty_evidence, ty_data_point_probabilities)
        ty_log_loss /= test_set.number_of_data_points

        confusion_matrix_tg += confusion_matrix(
            agg_tg_evidence,
            np.where(tg_data_point_probabilities > 0.5, 1, 0),
        )
        confusion_matrix_ty += confusion_matrix(
            agg_ty_evidence,
            np.where(ty_data_point_probabilities > 0.5, 1, 0),
        )

    return tg_log_loss, ty_log_loss, confusion_matrix_tg, confusion_matrix_ty


def get_log_loss(evidence, probabilities):
    """
    This function computes the log loss between probabilities and real observed values
    """
    return -np.mean(
        evidence * utils.log(probabilities) + (1 - evidence) * (utils.log(1-probabilities))
    )


def get_f1_score(confusion_matrix):
    precision = confusion_matrix[1, 1] / (
        confusion_matrix[1, 1] + confusion_matrix[0, 1]
    )
    recall = confusion_matrix[1, 1] / (
        confusion_matrix[1, 1] + confusion_matrix[1, 0]
    )

    return 2 * precision * recall / (precision + recall)

def print_results(tg_baseline_ll, ty_baseline_ll, tg_baseline_cm, ty_baseline_cm, tg_model_ll, ty_model_ll, tg_model_cm, ty_model_cm, horizons):
    for i, h in enumerate(horizons):
        print('h = {}'.format(h))
        print('')

        agg_tg_baseline_cm = np.sum(tg_baseline_cm[i], axis=0)
        agg_ty_baseline_cm = np.sum(ty_baseline_cm[i], axis=0)
        agg_tg_model_cm = np.sum(tg_model_cm[i], axis=0)
        agg_ty_model_cm = np.sum(ty_model_cm[i], axis=0)

        print('Baseline')
        print('----------------')
        print('Log-loss')
        print('{:.4f} {:.4f}'.format(np.mean(tg_baseline_ll[i]), np.mean(ty_baseline_ll[i])))
        print('')
        print('Confusion Matrix')
        print(agg_tg_baseline_cm)
        print(agg_ty_baseline_cm)
        print('')
        print('F1 Score')
        print('{:.4f} {:.4f}'.format(get_f1_score(agg_tg_baseline_cm), get_f1_score(agg_ty_baseline_cm)))

        print('')
        print('Model')
        print('----------------')
        print('Log-loss')
        print('{:.4f} {:.4f}'.format(np.mean(tg_model_ll[i]), np.mean(ty_model_ll[i])))
        print('')
        print('Confusion Matrix')
        print(agg_tg_baseline_cm)
        print(agg_ty_baseline_cm)
        print('F1 Score')
        print('{:.4f} {:.4f}'.format(get_f1_score(agg_tg_model_cm), get_f1_score(agg_ty_model_cm)))

        print('########################')
        print('########################')

def toy_data():
    evidence_set = load_evidence_set("../data/evidence/toy")
    evidence_set.lt_evidence -= 1
    evidence_set.tg_evidence -= 1
    evidence_set.ty_evidence -= 1
    evidence_set.rm_evidence -= 1

    from hackathon.model_representation import  CPDTables
    theta_s = np.array([[0.2, 0.3, 0.5], [0.4, 0.1, 0.5], [0.7, 0.1, 0.2]])
    pi_lt = np.array([[0.3, 0.7], [0.2, 0.8], [0.6, 0.4]])
    pi_tg = np.array([[0.8, 0.2], [0.1, 0.9], [0.5,0.5]])
    pi_ty = np.array([[0.3, 0.7], [0.4, 0.6], [0.8, 0.2]])
    theta_rm = np.array([[0.1, 0.2, 0.7], [0.5, 0.2, 0.3], [0.4, 0.5, 0.1]])
    cpds = CPDTables(theta_s, pi_lt, theta_rm, pi_tg, pi_ty)
    model = Model()
    model.init_from_cpds(cpds)
    inference = ModelInference(model)
    tg_marginals_bl, ty_marginals_bl = inference.get_triaging_normalized_frequencies(evidence_set, h=3)
    tg_marginals, ty_marginals = inference.get_triaging_marginals_over_time(evidence_set, h=3)

    # tg_bl_ll = [[]]
    # ty_bl_ll = [[]]
    # tg_bl_cm = [[]]
    # ty_bl_cm = [[]]
    # tg_ll = [[]]
    # ty_ll = [[]]
    # tg_cm = [[]]
    # ty_cm = [[]]
    #
    # (tg_bl_ll[0],
    # ty_bl_ll[0],
    # tg_bl_cm[0],
    # ty_bl_cm[0]) = compute_accuracy_for_triaging(tg_marginals_bl, ty_marginals_bl, evidence_set, h=3)
    # (tg_ll[0],
    #  ty_ll[0],
    #  tg_cm[0],
    #  ty_cm[0]) = compute_accuracy_for_triaging(tg_marginals, ty_marginals, evidence_set, h=3)
    # print_results(tg_bl_ll, ty_bl_ll, tg_bl_cm, ty_bl_cm, tg_ll, ty_ll, tg_cm, ty_cm)

if __name__ == "__main__":
    # toy_data()
    NUMBER_OF_SAMPLES = 5000
    BURN_IN = 500

    np.random.seed(42)
    random.seed(42)
    model = Model()
    model.init_from_mission_map(MissionMap.SPARKY)
    evidence_set = load_evidence_set("../data/evidence/asist/sparky")
    fit_and_evaluate(model, evidence_set, NUMBER_OF_SAMPLES, BURN_IN, 6, [1, 3, 5])

    np.random.seed(42)
    random.seed(42)
    model = Model()
    model.init_from_mission_map(MissionMap.FALCON)
    evidence_set = load_evidence_set("../data/evidence/asist/falcon")
    fit_and_evaluate(model, evidence_set, NUMBER_OF_SAMPLES, BURN_IN, 5, [1, 3, 5])
