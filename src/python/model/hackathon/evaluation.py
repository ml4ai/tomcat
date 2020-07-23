import numpy as np
from sklearn.model_selection import KFold
from hackathon.evidence_extraction import (
    EvidenceSet,
    MissionMap,
    load_evidence_set,
    save_evidence_set
)
from hackathon.learning import ParameterLearning
from hackathon.inference import ModelInference
from hackathon import utils as utils
from hackathon.model_representation import Model
import random
from sklearn.metrics import f1_score, accuracy_score, log_loss, confusion_matrix
from hackathon.sampling import AncestralSampling
import pandas as pd


def fit_and_evaluate(
    original_model, evidence_set, number_of_samples, burn_in, number_of_folds, horizons
):
    """
    This function fits and evaluates the models using k-fold cross validation. For each fold, the training set is
    used ti estimate the parameters of the models and the test set is then used to estimate the one-time-step-ahead
    marginals all time steps for the nodes TG and TY. Then, the estimated marginals are compared against the true values
    in the test set using log-loss. We also use log-loss to compare the relative frequencies of the values obtained
    for TG and TY over the training data for each time step with the test set, and we use this as a baseline to
    compare with the ToMCAT models.
    """
    folds = shuffle_and_split_data(evidence_set, number_of_folds)
    learning = ParameterLearning(original_model)

    tg_baseline_log_losses = [[] for _ in range(len(horizons))]
    ty_baseline_log_losses = [[] for _ in range(len(horizons))]
    tg_baseline_f1_scores = [[] for _ in range(len(horizons))]
    ty_baseline_f1_scores = [[] for _ in range(len(horizons))]
    tg_baseline_accuracies = [[] for _ in range(len(horizons))]
    ty_baseline_accuracies = [[] for _ in range(len(horizons))]
    tg_baseline_conf_matrices = [[] for _ in range(len(horizons))]
    ty_baseline_conf_matrices = [[] for _ in range(len(horizons))]

    tg_model_log_losses = [[] for _ in range(len(horizons))]
    ty_model_log_losses = [[] for _ in range(len(horizons))]
    tg_model_f1_scores = [[] for _ in range(len(horizons))]
    ty_model_f1_scores = [[] for _ in range(len(horizons))]
    tg_model_accuracies = [[] for _ in range(len(horizons))]
    ty_model_accuracies = [[] for _ in range(len(horizons))]
    tg_model_conf_matrices = [[] for _ in range(len(horizons))]
    ty_model_conf_matrices = [[] for _ in range(len(horizons))]

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
                tg_baseline_f1,
                ty_baseline_f1,
                tg_baseline_acc,
                ty_baseline_acc,
                tg_baseline_cm,
                ty_baseline_cm,
            ) = compute_accuracy_for_triaging(
                tg_baseline_frequencies, ty_baseline_frequencies, test_set, h
            )
            tg_baseline_log_losses[i].append(tg_baseline_ll)
            ty_baseline_log_losses[i].append(ty_baseline_ll)
            tg_baseline_f1_scores[i].append(tg_baseline_f1)
            ty_baseline_f1_scores[i].append(ty_baseline_f1)
            tg_baseline_accuracies[i].append(tg_baseline_acc)
            ty_baseline_accuracies[i].append(ty_baseline_acc)
            tg_baseline_conf_matrices[i].append(tg_baseline_cm)
            ty_baseline_conf_matrices[i].append(ty_baseline_cm)

            (
                tg_marginals,
                ty_marginals,
            ) = inference.get_triaging_marginals_over_time(test_set, h)
            (
                tg_model_ll,
                ty_model_ll,
                tg_model_f1,
                ty_model_f1,
                tg_model_acc,
                ty_model_acc,
                tg_model_cm,
                ty_model_cm,
            ) = compute_accuracy_for_triaging(tg_marginals, ty_marginals, test_set, h)
            tg_model_log_losses[i].append(tg_model_ll)
            ty_model_log_losses[i].append(ty_model_ll)
            tg_model_f1_scores[i].append(tg_model_f1)
            ty_model_f1_scores[i].append(ty_model_f1)
            tg_model_accuracies[i].append(tg_model_acc)
            ty_model_accuracies[i].append(ty_model_acc)
            tg_model_conf_matrices[i].append(tg_model_cm)
            ty_model_conf_matrices[i].append(ty_model_cm)

    print_results (
        tg_baseline_log_losses,
        ty_baseline_log_losses,
        tg_baseline_f1_scores,
        ty_baseline_f1_scores,
        tg_baseline_accuracies,
        ty_baseline_accuracies,
        tg_baseline_conf_matrices,
        ty_baseline_conf_matrices,
        tg_model_log_losses,
        ty_model_log_losses,
        tg_model_f1_scores,
        ty_model_f1_scores,
        tg_model_accuracies,
        ty_model_accuracies,
        tg_model_conf_matrices,
        ty_model_conf_matrices,
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
    tg_f1_score = 0
    ty_f1_score = 0
    tg_accuracy = 0
    ty_accuracy = 0
    tg_conf_matrix = np.zeros((2, 2))
    ty_conf_matrix = np.zeros((2, 2))

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
        tg_log_loss += log_loss(agg_tg_evidence, tg_data_point_probabilities)
        ty_log_loss += log_loss(agg_ty_evidence, ty_data_point_probabilities)

        tg_predicted_labels = np.where(tg_data_point_probabilities > 0.5, 1, 0)
        ty_predicted_labels = np.where(ty_data_point_probabilities > 0.5, 1, 0)

        tg_f1_score += f1_score(agg_tg_evidence, tg_predicted_labels)
        ty_f1_score += f1_score(agg_ty_evidence, ty_predicted_labels)

        tg_accuracy += accuracy_score(agg_tg_evidence, tg_predicted_labels)
        ty_accuracy += accuracy_score(agg_ty_evidence, ty_predicted_labels)

        tg_conf_matrix += confusion_matrix(agg_tg_evidence, tg_predicted_labels)
        ty_conf_matrix += confusion_matrix(agg_ty_evidence, ty_predicted_labels)

    tg_log_loss /= test_set.number_of_data_points
    ty_log_loss /= test_set.number_of_data_points
    tg_f1_score /= test_set.number_of_data_points
    ty_f1_score /= test_set.number_of_data_points
    tg_accuracy /= test_set.number_of_data_points
    ty_accuracy /= test_set.number_of_data_points

    return tg_log_loss, ty_log_loss, tg_f1_score, ty_f1_score, tg_accuracy, ty_accuracy, tg_conf_matrix, ty_conf_matrix


# def get_log_loss(evidence, probabilities):
#     """
#     This function computes the log loss between probabilities and real observed values
#     """
#     return -np.mean(
#         evidence * utils.log(probabilities) + (1 - evidence) * (utils.log(1-probabilities))
#     )
#
#
# def get_f1_score(confusion_matrix):
#     if confusion_matrix[1, 1] + confusion_matrix[0, 1] == 0:
#         precision = 0
#     else:
#         precision = confusion_matrix[1, 1] / (
#             confusion_matrix[1, 1] + confusion_matrix[0, 1]
#         )
#
#     if confusion_matrix[1, 1] + confusion_matrix[1, 0]:
#         recall = 0
#     else:
#         recall = confusion_matrix[1, 1] / (
#                 confusion_matrix[1, 1] + confusion_matrix[1, 0]
#         )
#
#     return 2 * precision * recall / (precision + recall)

# def get_accuracy(confusion_matrix):
#     return confusion_matrix[1, 1] / (confusion_matrix[0, 0] + confusion_matrix[1, 1])

def print_metrics(log_losses, f1_scores, accuracies):
    print('Log-loss')
    print('{:.2f} +- {:.4f}'.format(np.mean(log_losses), np.std(log_losses)/len(log_losses)))
    print('F1 Score')
    print('{:.2f} +- {:.4f}'.format(np.mean(f1_scores), np.std(f1_scores)/len(f1_scores)))
    print('Accuracy')
    print('{:.2f} +- {:.4f}'.format(np.mean(accuracies), np.std(accuracies)/len(accuracies)))

def print_results(tg_baseline_ll, ty_baseline_ll, tg_baseline_f1, ty_baseline_f1, tg_baseline_acc, ty_baseline_acc, tg_baseline_cm, ty_baseline_cm, tg_model_ll, ty_model_ll, tg_model_f1, ty_model_f1, tg_model_acc, ty_model_acc, tg_model_cm, ty_model_cm, horizons):
    for i, h in enumerate(horizons):
        print('h = {}'.format(h))
        print('===============================')

        print('--------')
        print('Baseline')
        print('--------')
        print('TG')
        print_metrics(tg_baseline_ll[i], tg_baseline_f1[i], tg_baseline_acc[i])
        print('')
        print('TY')
        print_metrics(ty_baseline_ll[i], ty_baseline_f1[i], ty_baseline_acc[i])

        print('--------')
        print('Model')
        print('--------')
        print('TG')
        print_metrics(tg_model_ll[i], tg_model_f1[i], tg_model_acc[i])
        print('')
        print('TY')
        print_metrics(ty_model_ll[i], ty_model_f1[i], ty_model_acc[i])

        print('===============================')

    print('')
    print('VARIATION')
    print('---------')

    print('TG')
    print('Baseline')
    print('1->3')
    print((np.sum(tg_baseline_cm[1], axis=0) - np.sum(tg_baseline_cm[0], axis=0)) / np.sum(tg_baseline_cm[0], axis=0))
    print('3->5')
    print((np.sum(tg_baseline_cm[2], axis=0) - np.sum(tg_baseline_cm[1], axis=0)) / np.sum(tg_baseline_cm[1], axis=0))
    print('Model')
    print('1->3')
    print((np.sum(tg_model_cm[1], axis=0) - np.sum(tg_model_cm[0], axis=0)) / np.sum(tg_model_cm[0], axis=0))
    print('3->5')
    print((np.sum(tg_model_cm[2], axis=0) - np.sum(tg_model_cm[1], axis=0)) / np.sum(tg_model_cm[1], axis=0))

    print('TY')
    print('Baseline')
    print('1->3')
    print((np.sum(ty_baseline_cm[1], axis=0) - np.sum(ty_baseline_cm[0], axis=0)) / np.sum(ty_baseline_cm[0], axis=0))
    print('3->5')
    print((np.sum(ty_baseline_cm[2], axis=0) - np.sum(ty_baseline_cm[1], axis=0)) / np.sum(ty_baseline_cm[1], axis=0))
    print('Model')
    print('1->3')
    print((np.sum(ty_model_cm[1], axis=0) - np.sum(ty_model_cm[0], axis=0)) / np.sum(ty_model_cm[0], axis=0))
    print('3->5')
    print((np.sum(ty_model_cm[2], axis=0) - np.sum(ty_model_cm[1], axis=0)) / np.sum(ty_model_cm[1], axis=0))


def toy_data():
    evidence_set = load_evidence_set("../data/evidence/toy")
    evidence_set.lt_evidence -= 1
    evidence_set.tg_evidence -= 1
    evidence_set.ty_evidence -= 1
    evidence_set.rm_evidence -= 1

    from hackathon.model_representation import CPDTables
    theta_s = np.array([[0.2, 0.3, 0.5], [0.4, 0.1, 0.5], [0.7, 0.1, 0.2]])
    pi_lt = np.array([[0.3, 0.7], [0.2, 0.8], [0.6, 0.4]])
    pi_tg = np.array([[0.8, 0.2], [0.1, 0.9], [0.5, 0.5]])
    pi_ty = np.array([[0.3, 0.7], [0.4, 0.6], [0.8, 0.2]])
    theta_rm = np.array([[0.1, 0.2, 0.7], [0.5, 0.2, 0.3], [0.4, 0.5, 0.1]])
    cpds = CPDTables(theta_s, pi_lt, theta_rm, pi_tg, pi_ty)
    model = Model()
    model.init_from_cpds(cpds)
    model.save('../data/parameters/toy')
    #inference = ModelInference(models)
    #tg_marginals_bl, ty_marginals_bl = inference.get_triaging_normalized_frequencies(evidence_set, h=3)
    #tg_marginals, ty_marginals = inference.get_triaging_marginals_over_time(evidence_set, h=3)

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
    #toy_data()

    NUMBER_OF_SAMPLES = 5000
    BURN_IN = 500
    map_names = ['sparky', 'falcon']

    # print('*************************')
    # print('GENERATING SYNTHETIC DATA')
    # print('*************************')
    #
    # for map_name in map_names:
    #     np.random.seed(42)
    #     random.seed(42)
    #     # Train the model with full data and save its parameters
    #     evidence_set = load_evidence_set("../data/evidence/asist/{}".format(map_name))
    #     model = Model()
    #     if map_name == 'sparky':
    #         model.init_from_mission_map(MissionMap.SPARKY)
    #     elif map_name == 'falcon':
    #         model.init_from_mission_map(MissionMap.FALCON)
    #     learning = ParameterLearning(model)
    #     trained_model = learning.estimate_parameters(evidence_set, NUMBER_OF_SAMPLES, BURN_IN)
    #     model_folder = "../data/models/{}/{}s{}b".format(map_name, NUMBER_OF_SAMPLES, BURN_IN)
    #     trained_model.save(model_folder)
    #
    #     # Load models and generate samples
    #     model = Model()
    #     model.load(model_folder)
    #     sampling = AncestralSampling(model)
    #     synthetic_data = sampling.sample(0, evidence_set.time_slices, 100)
    #
    #     save_evidence_set('../data/evidence/asist/synthetic_{}_{}s{}b'.format(map_name, NUMBER_OF_SAMPLES, BURN_IN), synthetic_data)

    print('****************************')
    print('PERFORMANCE SYNTHETIC SPARKY')
    print('****************************')

    np.random.seed(42)
    random.seed(42)
    models = Model()
    models.init_from_mission_map(MissionMap.SPARKY)
    evidence_set = load_evidence_set(
        '../data/evidence/asist/synthetic_sparky_{}s{}b'.format(NUMBER_OF_SAMPLES, BURN_IN))

    # Use the same size of training set
    evidence_set.lt_evidence = evidence_set.lt_evidence[:6, :]
    evidence_set.rm_evidence = evidence_set.rm_evidence[:6, :]
    evidence_set.tg_evidence = evidence_set.tg_evidence[:6, :]
    evidence_set.ty_evidence = evidence_set.ty_evidence[:6, :]
    fit_and_evaluate(models, evidence_set, NUMBER_OF_SAMPLES, BURN_IN, 6, [1, 3, 5])

    print('****************************')
    print('PERFORMANCE SYNTHETIC FALCON')
    print('****************************')

    np.random.seed(42)
    random.seed(42)
    models = Model()
    models.init_from_mission_map(MissionMap.FALCON)
    evidence_set = load_evidence_set(
        '../data/evidence/asist/synthetic_falcon_{}s{}b'.format(NUMBER_OF_SAMPLES, BURN_IN))
    # Use the same size of training set
    evidence_set.lt_evidence = evidence_set.lt_evidence[:5, :]
    evidence_set.rm_evidence = evidence_set.rm_evidence[:5, :]
    evidence_set.tg_evidence = evidence_set.tg_evidence[:5, :]
    evidence_set.ty_evidence = evidence_set.ty_evidence[:5, :]
    fit_and_evaluate(models, evidence_set, NUMBER_OF_SAMPLES, BURN_IN, 5, [1, 3, 5])

    print('******************')
    print('PERFORMANCE SPARKY')
    print('******************')

    np.random.seed(42)
    random.seed(42)
    models = Model()
    models.init_from_mission_map(MissionMap.SPARKY)
    evidence_set = load_evidence_set("../data/evidence/asist/sparky")
    fit_and_evaluate(models, evidence_set, NUMBER_OF_SAMPLES, BURN_IN, 6, [1, 3, 5])

    print('******************')
    print('PERFORMANCE FALCON')
    print('******************')

    np.random.seed(42)
    random.seed(42)
    models = Model()
    models.init_from_mission_map(MissionMap.FALCON)
    evidence_set = load_evidence_set("../data/evidence/asist/falcon")
    fit_and_evaluate(models, evidence_set, NUMBER_OF_SAMPLES, BURN_IN, 5, [1, 3, 5])
