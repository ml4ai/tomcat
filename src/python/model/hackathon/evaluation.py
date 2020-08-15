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
import math
import matplotlib.pyplot as plt
from hackathon.model_representation import CPDTables
from enum import Enum


def fit_and_evaluate(
        original_model, evidence_set, num_gibbs_samples, gibbs_burn_in, number_of_folds, horizons, model_root_folder
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

    for fold, (training_set, test_set) in enumerate(folds):
        print('>> Fold {}'.format(fold))
        trained_model = original_model
        model_folder = "{}/{}s{}b_fold_{}".format(model_root_folder, num_gibbs_samples, gibbs_burn_in, fold)
        if not trained_model.load(model_folder):
            trained_model = learning.estimate_parameters(
                training_set, num_gibbs_samples, gibbs_burn_in
            )
            trained_model.save(model_folder)

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

    print_results(
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
        lt_train = full_data[train_index, 0: evidence_set.time_slices]
        lt_test = full_data[test_index, 0: evidence_set.time_slices]

        rm_train = full_data[
                   train_index,
                   evidence_set.time_slices: 2 * evidence_set.time_slices,
                   ]
        rm_test = full_data[
                  test_index, evidence_set.time_slices: 2 * evidence_set.time_slices
                  ]

        tg_train = full_data[
                   train_index,
                   2 * evidence_set.time_slices: 3 * evidence_set.time_slices,
                   ]
        tg_test = full_data[
                  test_index,
                  2 * evidence_set.time_slices: 3 * evidence_set.time_slices,
                  ]

        ty_train = full_data[
                   train_index,
                   3 * evidence_set.time_slices: 4 * evidence_set.time_slices,
                   ]
        ty_test = full_data[
                  test_index,
                  3 * evidence_set.time_slices: 4 * evidence_set.time_slices,
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
                agg_tg_evidence[i] |= tg_evidence[i + j + 1]
                agg_ty_evidence[i] |= ty_evidence[i + j + 1]
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


def get_formatted_metric(metric):
    return '${:.2f} \\pm {:.4f}$'.format(np.mean(metric), np.std(metric) / len(metric))


def format_rate(rate, error):
    if math.isnan(rate) or math.isnan(error):
        return '--'
    else:
        return '${}\\% \\pm {}\\%$'.format(int(rate), int(error))


def get_formatted_rates(matrices_1, matrices_2):
    rates = (np.array(matrices_2) - np.array(matrices_1)) / np.array(matrices_1)
    means = np.mean(rates, axis=0) * 100
    errors = 100 * np.std(rates, axis=0) / np.sqrt(rates.shape[0])

    tn = format_rate(means[0, 0], errors[0, 0])
    tp = format_rate(means[1, 1], errors[1, 1])
    fn = format_rate(means[1, 0], errors[1, 0])
    fp = format_rate(means[0, 1], errors[0, 1])

    return tn, tp, fn, fp


def print_results(tg_baseline_ll, ty_baseline_ll, tg_baseline_f1, ty_baseline_f1, tg_baseline_acc, ty_baseline_acc,
                  tg_baseline_cm, ty_baseline_cm, tg_model_ll, ty_model_ll, tg_model_f1, ty_model_f1, tg_model_acc,
                  ty_model_acc, tg_model_cm, ty_model_cm, horizons):
    for i, h in enumerate(horizons):
        r1 = get_formatted_metric(tg_baseline_ll[i])
        r2 = get_formatted_metric(tg_model_ll[i])
        r3 = get_formatted_metric(ty_baseline_ll[i])
        r4 = get_formatted_metric(ty_model_ll[i])
        print('\\multirow{{3}}{{*}}{{{}}} & Average Log-Loss & {} & {} & {} & {} \\\\'.format(h, r1, r2, r3, r4))

        r1 = get_formatted_metric(tg_baseline_f1[i])
        r2 = get_formatted_metric(tg_model_f1[i])
        r3 = get_formatted_metric(ty_baseline_f1[i])
        r4 = get_formatted_metric(ty_model_f1[i])
        print('& F1 Score & {} & {} & {} & {} \\\\'.format(r1, r2, r3, r4))

        r1 = get_formatted_metric(tg_baseline_acc[i])
        r2 = get_formatted_metric(tg_model_acc[i])
        r3 = get_formatted_metric(ty_baseline_acc[i])
        r4 = get_formatted_metric(ty_model_acc[i])
        print('& Accuracy & {} & {} & {} & {} \\\\'.format(r1, r2, r3, r4))
        print('\\hline')

    print('---------')
    print('VARIATION')
    print('---------')

    for i in range(1, len(horizons)):
        (tn_bas_tg, tp_bas_tg, fn_bas_tg, fp_bas_tg) = get_formatted_rates(tg_baseline_cm[i - 1], tg_baseline_cm[i])
        (tn_mod_tg, tp_mod_tg, fn_mod_tg, fp_mod_tg) = get_formatted_rates(tg_baseline_cm[i - 1], tg_baseline_cm[i])
        (tn_bas_ty, tp_bas_ty, fn_bas_ty, fp_bas_ty) = get_formatted_rates(ty_baseline_cm[i - 1], ty_baseline_cm[i])
        (tn_mod_ty, tp_mod_ty, fn_mod_ty, fp_mod_ty) = get_formatted_rates(ty_baseline_cm[i - 1], ty_baseline_cm[i])
        h1 = horizons[i - 1]
        h2 = horizons[i]

        r1 = tn_bas_tg
        r2 = tn_mod_tg
        r3 = tn_bas_ty
        r4 = tn_mod_ty

        print(
            '\\multirow{{4}}{{*}}{{${} \\rightarrow {}$}} & TN & {} & {} & {} & {} \\\\'.format(h1, h2, r1, r2, r3, r4))

        r1 = tp_bas_tg
        r2 = tp_mod_tg
        r3 = tp_bas_ty
        r4 = tp_mod_ty
        print(
            '& TP & {} & {} & {} & {} \\\\'.format(r1, r2, r3, r4))

        r1 = fn_bas_tg
        r2 = fn_mod_tg
        r3 = fn_bas_ty
        r4 = fn_mod_ty
        print(
            '& FN & {} & {} & {} & {} \\\\'.format(r1, r2, r3, r4))

        r1 = fp_bas_tg
        r2 = fp_mod_tg
        r3 = fp_bas_ty
        r4 = fp_mod_ty
        print(
            '& FP & {} & {} & {} & {} \\\\'.format(r1, r2, r3, r4))
        print('\\hline')


def get_toy_model():
    theta_s = np.array([[0.2, 0.3, 0.5], [0.4, 0.1, 0.5], [0.7, 0.1, 0.2]])
    pi_lt = np.array([[0.3, 0.7], [0.2, 0.8], [0.6, 0.4]])
    pi_tg = np.array([[0.8, 0.2], [0.1, 0.9], [0.5, 0.5]])
    pi_ty = np.array([[0.3, 0.7], [0.4, 0.6], [0.8, 0.2]])
    theta_rm = np.array([[0.1, 0.2, 0.7], [0.5, 0.2, 0.3], [0.4, 0.5, 0.1]])
    cpds = CPDTables(theta_s, pi_lt, theta_rm, pi_tg, pi_ty)
    model = Model()
    model.init_from_cpds(cpds)

    return model


def generate_synthetic_toy_data(num_samples, even_until):
    model = get_toy_model()
    sampler = AncestralSampling(model)
    np.random.seed(42)
    random.seed(42)
    initial_state = 0
    time_steps = 10
    data = sampler.sample(initial_state, time_steps, num_samples, even_until)
    folder_name_template = '../data/evidence/toy/synthetic_{}eu_{}s'
    folder_name = folder_name_template.format(even_until, num_samples)
    save_evidence_set(folder_name, data)


def load_trained_asist_model(mission_map, gibbs_samples, gibbs_burn_in):
    map_name = ''
    if mission_map == MissionMap.SPARKY:
        map_name = 'sparky'
    elif mission_map == MissionMap.FALCON:
        map_name = 'falcon'

    model_folder = "../data/models/{}/not_shared_params/{}s{}b".format(map_name, gibbs_samples, gibbs_burn_in)
    model = Model()
    if model.load(model_folder):
        return model
    else:
        evidence_set = load_evidence_set("../data/evidence/asist/{}".format(map_name))
        model = Model()
        model.init_from_mission_map(mission_map)
        learning = ParameterLearning(model)
        trained_model = learning.estimate_parameters(evidence_set, gibbs_samples, gibbs_burn_in)
        model_folder = "../data/models/{}/{}s{}b".format(map_name, gibbs_samples, gibbs_burn_in)
        trained_model.save(model_folder)
        return trained_model


def generate_asist_synthetic_data(model, mission_map, num_samples, time_steps, gibbs_samples, gibbs_burn_in,
                                  even_until=0):
    map_name = ''
    if mission_map == MissionMap.SPARKY:
        map_name = 'sparky'
    elif mission_map == MissionMap.FALCON:
        map_name = 'falcon'

    sampling = AncestralSampling(model)
    initial_state = 0

    synthetic_data = sampling.sample(initial_state, time_steps, num_samples, even_until)
    filepath_template = '../data/evidence/asist/non_shared/synthetic_{}_{}eu_{}s_{}gs{}gbi'
    filepath = filepath_template.format(map_name, even_until, num_samples, gibbs_samples, gibbs_burn_in)
    save_evidence_set(filepath, synthetic_data)


def experiment_generate_synthetic_data(num_samples, gibbs_samples, gibbs_burn_in, even_until):
    print('*************************')
    print('GENERATING SYNTHETIC DATA')
    print('*************************')

    time_steps = 600

    print('>> Sparky')
    np.random.seed(42)
    random.seed(42)
    model = load_trained_asist_model(MissionMap.SPARKY, gibbs_samples, gibbs_burn_in)
    generate_asist_synthetic_data(model, MissionMap.SPARKY, num_samples, time_steps, gibbs_samples, gibbs_burn_in,
                                  even_until)

    print('>> Falcon')
    np.random.seed(42)
    random.seed(42)
    model = load_trained_asist_model(MissionMap.FALCON, gibbs_samples, gibbs_burn_in)
    generate_asist_synthetic_data(model, MissionMap.FALCON, num_samples, time_steps, gibbs_samples, gibbs_burn_in,
                                  even_until)


def experiment_eval_performance_synthetic(num_samples, gibbs_samples, gibbs_burn_in, horizons):
    print('****************************************')
    print('EVALUATING PERFORMANCE ON SYNTHETIC DATA')
    print('****************************************')

    print('>> Sparky')
    np.random.seed(42)
    random.seed(42)
    model = Model()
    model.init_from_mission_map(MissionMap.SPARKY)
    evidence_set = load_evidence_set(
        '../data/evidence/asist/synthetic_sparky_0eu_{}s_{}gs{}gbi'.format(num_samples, gibbs_samples, gibbs_burn_in))

    # Use the same size of training set
    evidence_set.lt_evidence = evidence_set.lt_evidence[:6, :]
    evidence_set.rm_evidence = evidence_set.rm_evidence[:6, :]
    evidence_set.tg_evidence = evidence_set.tg_evidence[:6, :]
    evidence_set.ty_evidence = evidence_set.ty_evidence[:6, :]
    evidence_set.number_of_data_points = 6
    num_folds = 6
    fit_and_evaluate(model, evidence_set, gibbs_samples, gibbs_burn_in, num_folds, horizons,
                     "../data/models/synthetic_sparky")

    print('>> Falcon')
    np.random.seed(42)
    random.seed(42)
    model = Model()
    model.init_from_mission_map(MissionMap.FALCON)
    evidence_set = load_evidence_set(
        '../data/evidence/asist/synthetic_falcon_0eu_{}s_{}gs{}gbi'.format(num_samples, gibbs_samples, gibbs_burn_in))

    # Use the same size of training set
    evidence_set.lt_evidence = evidence_set.lt_evidence[:5, :]
    evidence_set.rm_evidence = evidence_set.rm_evidence[:5, :]
    evidence_set.tg_evidence = evidence_set.tg_evidence[:5, :]
    evidence_set.ty_evidence = evidence_set.ty_evidence[:5, :]
    evidence_set.number_of_data_points = 5
    num_folds = 5
    fit_and_evaluate(model, evidence_set, gibbs_samples, gibbs_burn_in, num_folds, horizons,
                     "../data/models/synthetic_falcon")


def experiment_eval_performance_real(gibbs_samples, gibbs_burn_in, horizons):
    print('***********************************')
    print('EVALUATING PERFORMANCE ON REAL DATA')
    print('***********************************')

    print('>> Sparky')
    np.random.seed(42)
    random.seed(42)
    models = Model()
    models.init_from_mission_map(MissionMap.SPARKY)
    evidence_set = load_evidence_set("../data/evidence/asist/sparky")
    num_folds = 6
    fit_and_evaluate(models, evidence_set, gibbs_samples, gibbs_burn_in, num_folds, horizons, "../data/models/sparky")

    print('>> Falcon')
    np.random.seed(42)
    random.seed(42)
    models = Model()
    models.init_from_mission_map(MissionMap.FALCON)
    evidence_set = load_evidence_set("../data/evidence/asist/falcon")
    num_folds = 5
    fit_and_evaluate(models, evidence_set, gibbs_samples, gibbs_burn_in, num_folds, horizons, "../data/models/falcon")


def compare_empirical_vs_projected_probabilities_for(model, data, horizons, even_until, t, output_folder,
                                                     model_generation_info):
    tg_empirical_probs = []
    tg_projected_probs = []
    ty_empirical_probs = []
    ty_projected_probs = []
    inference = ModelInference(model)

    for h in horizons:
        (empirical_tg_prob, empirical_ty_prob) = inference.get_triaging_normalized_frequencies(data, h)
        (projected_tg_prob, projected_ty_prob) = inference.get_triaging_marginals_over_time(data, h)
        if even_until < 1:
            pro_tg = np.mean(projected_tg_prob[:, t])
            emp_tg = empirical_tg_prob[t]
        else:
            pro_tg = projected_tg_prob[0][even_until]
            emp_tg = empirical_tg_prob[even_until]
        tg_empirical_probs.append(emp_tg)
        tg_projected_probs.append(pro_tg)

        emp_ty = empirical_ty_prob[even_until]
        pro_ty = projected_ty_prob[0][even_until]
        ty_empirical_probs.append(emp_ty)
        ty_projected_probs.append(pro_ty)

        print('\nh = {}'.format(h))
        print('TG Empirical: {}, TG Derived: {}'.format(emp_tg, pro_tg))
        print('TY Empirical: {}, TY Derived: {}'.format(emp_ty, pro_ty))

    tg_filepath_template = output_folder + '/tg_{}eu_{}s_{}.{}'
    ty_filepath_template = output_folder + '/ty_{}eu_{}s_{}.{}'

    # Plot results
    fig, ax = plt.subplots()
    ax.scatter(horizons, tg_projected_probs, label='Projected')
    ax.scatter(horizons, tg_empirical_probs, label='Empirical')
    ax.set_title('Projected vs Empirical Forecasting for TG')
    ax.set_xticks(horizons)
    ax.set_ylim(-0.1, 1.1)
    ax.legend()
    filepath = tg_filepath_template.format(even_until, data.number_of_data_points, model_generation_info, 'png')
    plt.savefig(filepath)

    fig, ax = plt.subplots()
    ax.scatter(horizons, ty_projected_probs, label='Projected')
    ax.scatter(horizons, ty_empirical_probs, label='Empirical')
    ax.set_title('Projected vs Empirical Forecasting for TY')
    ax.set_xticks(horizons)
    ax.set_ylim(-0.1, 1.1)
    ax.legend()

    filepath = ty_filepath_template.format(even_until, data.number_of_data_points, model_generation_info, 'png')
    plt.savefig(filepath)

    # Save to file
    tg_results = np.column_stack([horizons, tg_empirical_probs, tg_projected_probs])
    filepath = tg_filepath_template.format(even_until, data.number_of_data_points, model_generation_info, 'csv')
    pd.DataFrame(tg_results, columns=['h', 'Empirical', 'Projected']).to_csv(filepath)

    ty_results = np.column_stack([horizons, ty_empirical_probs, ty_projected_probs])
    filepath = ty_filepath_template.format(even_until, data.number_of_data_points, model_generation_info, 'csv')
    pd.DataFrame(ty_results, columns=['h', 'Empirical', 'Projected']).to_csv(filepath)


def experiment_check_with_asist_model():
    num_samples = 100
    even_until_list = [100, 0]
    t = 100
    horizons = [1, 3, 5, 10, 15, 30]
    num_gibbs_samples = 5000
    gibbs_burn_in = 500
    model = load_trained_asist_model(MissionMap.SPARKY, num_gibbs_samples, gibbs_burn_in)

    for even_until in even_until_list:
        # experiment_generate_synthetic_data(num_samples, num_gibbs_samples, gibbs_burn_in, even_until)

        data_folder_name = '../data/evidence/asist/synthetic_sparky_{}eu_{}s_{}gs{}gbi'.format(even_until, num_samples,
                                                                                               num_gibbs_samples,
                                                                                               gibbs_burn_in)
        data = load_evidence_set(data_folder_name)

        output_folder_name = '../data/plots/asist/sparky'
        compare_empirical_vs_projected_probabilities_for(model, data, horizons, even_until, t, output_folder_name,
                                                         '{}gs{}gbi'.format(num_gibbs_samples, gibbs_burn_in))


def experiment_check_with_toy_model():
    num_samples = 10000
    even_until_list = [3, 0]
    t = even_until_list[0]
    horizons = [1, 2, 3, 4, 5]

    for even_until in even_until_list:
        # generate_synthetic_toy_data(num_samples, even_until)

        model = get_toy_model()
        data_folder_name = '../data/evidence/toy/synthetic_{}eu_{}s'.format(even_until, num_samples)
        data = load_evidence_set(data_folder_name)

        output_folder_name = '../data/plots/toy'
        compare_empirical_vs_projected_probabilities_for(model, data, horizons, even_until, t, output_folder_name,
                                                         'manual')


def evaluate_transitions(data, first_room_index):
    class States(Enum):
        HW = 0
        LRW = 1
        LTG = 2
        LTY = 3
        DRW = 4
        DTG = 5
        DTY = 6

    transitions_per_mission = []

    for d in range(data.number_of_data_points):
        curr_state = States.HW
        transitions = np.zeros((7, 7))
        for t in range(1, data.time_slices):
            if data.rm_evidence[d, t] >= first_room_index:
                if data.lt_evidence[d, t] == 1:
                    if data.tg_evidence[d, t] == 1:
                        new_state = States.LTG
                    elif data.ty_evidence[d, t] == 1:
                        new_state = States.LTY
                    else:
                        new_state = States.LRW
                else:
                    if data.tg_evidence[d, t] == 1:
                        new_state = States.DTG
                    elif data.ty_evidence[d, t] == 1:
                        new_state = States.DTY
                    else:
                        new_state = States.DRW
            else:
                new_state = States.HW

            transitions[curr_state.value][new_state.value] += 1
            curr_state = new_state

        z = np.sum(transitions, 1)[:, np.newaxis]
        transitions_per_mission.append(np.divide(transitions, z, out=np.zeros_like(transitions), where=z != 0))

    transitions_per_mission = np.array(transitions_per_mission)

    for i in range(7):
        row = []
        for j in range(7):
            row.append(get_formatted_metric(transitions_per_mission[:, i, j]))
        print(' & '.join(row))

    # np.set_printoptions(formatter={'float': lambda x: "{0:0.2f}".format(x)})
    # for d, transitions in enumerate(transitions_per_mission):
    #     print('Transition in Mission {}'.format(d))
    #     print(transitions)


def experiment_transitions():
    print('**********************')
    print('EVALUATING TRANSITIONS')
    print('**********************')

    print('>> Synthetic Sparky')
    data = load_evidence_set("../data/evidence/asist/non_shared/synthetic_sparky_0eu_100s_5000gs500gbi")
    evaluate_transitions(data, 8)

    print('\n>> Sparky')
    data = load_evidence_set("../data/evidence/asist/sparky")
    evaluate_transitions(data, 8)

    print('\n>> Synthetic Falcon')
    data = load_evidence_set("../data/evidence/asist/non_shared/synthetic_falcon_0eu_100s_5000gs500gbi")
    evaluate_transitions(data, 10)

    print('\n>> Falcon')
    data = load_evidence_set("../data/evidence/asist/falcon")
    evaluate_transitions(data, 10)


"""
Check to see what is the player behavior while inside a room in each one of the mission trials 
"""


def evaluate_common_behavior(data, first_room_index, output_folder, filename):
    times_hallway = np.zeros(data.number_of_data_points)
    times_rw_light = np.zeros(data.number_of_data_points)
    times_tg_light = np.zeros(data.number_of_data_points)
    times_ty_light = np.zeros(data.number_of_data_points)
    times_tg_dark = np.zeros(data.number_of_data_points)
    times_ty_dark = np.zeros(data.number_of_data_points)
    times_rw_dark = np.zeros(data.number_of_data_points)

    for d in range(data.number_of_data_points):
        for t in range(data.time_slices):
            if data.rm_evidence[d, t] >= first_room_index:
                if data.lt_evidence[d, t] == 1:
                    if data.tg_evidence[d, t] == 1:
                        times_tg_light[d] += 1
                    elif data.ty_evidence[d, t] == 1:
                        times_ty_light[d] += 1
                    else:
                        times_rw_light[d] += 1
                else:
                    if data.tg_evidence[d, t] == 1:
                        times_tg_dark[d] += 1
                    elif data.ty_evidence[d, t] == 1:
                        times_ty_dark[d] += 1
                    else:
                        times_rw_dark[d] += 1
            else:
                times_hallway[d] += 1

    # Normalize
    times_hallway /= data.time_slices
    times_rw_light /= data.time_slices
    times_rw_dark /= data.time_slices
    times_tg_light /= data.time_slices
    times_tg_dark /= data.time_slices
    times_ty_light /= data.time_slices
    times_ty_dark /= data.time_slices

    # Plot
    categories = list(range(7))
    cat_names = ['HW', 'LRW', 'DRW', 'LTG', 'DTG', 'LTY', 'DTY']
    cat_values = [np.mean(times_hallway), np.mean(times_rw_light), np.mean(times_rw_dark), np.mean(times_tg_light),
                  np.mean(times_tg_dark), np.mean(times_ty_light), np.mean(times_ty_dark)]

    fig, ax = plt.subplots()
    ax.bar(categories, cat_values, tick_label=cat_names)
    ax.set_title("States' Frequencies")
    filepath = '{}/{}.png'.format(output_folder, filename)
    plt.savefig(filepath)

    print('Hallway:')
    print(get_formatted_metric(times_hallway))
    print('RW in a light room:')
    print(get_formatted_metric(times_rw_light))
    print('RW in a dark room:')
    print(get_formatted_metric(times_rw_dark))
    print('TG in a light room:')
    print(get_formatted_metric(times_tg_light))
    print('TG in a dark room:')
    print(get_formatted_metric(times_tg_dark))
    print('TY in a light room:')
    print(get_formatted_metric(times_ty_light))
    print('TY in a dark room:')
    print(get_formatted_metric(times_ty_dark))


def experiment_common_behavior():
    print('**************************')
    print('EVALUATING COMMON BEHAVIOR')
    print('**************************')

    print('>> Synthetic Sparky')
    data = load_evidence_set("../data/evidence/asist/non_shared/synthetic_sparky_0eu_100s_5000gs500gbi")
    evaluate_common_behavior(data, 8, '../data/plots/asist/sparky/non_shared', 'state_freq_synthetic_sparky_0eu_100s_5000gs500gbi')

    print('\n>> Sparky')
    data = load_evidence_set("../data/evidence/asist/sparky")
    evaluate_common_behavior(data, 8, '../data/plots/asist/sparky', 'state_freq_sparky_0eu_6s_5000gs500gbi')

    print('\n>> Synthetic Falcon')
    data = load_evidence_set("../data/evidence/asist/non_shared/synthetic_falcon_0eu_100s_5000gs500gbi")
    evaluate_common_behavior(data, 10, '../data/plots/asist/falcon/non_shared', 'state_freq_synthetic_falcon_0eu_100s_5000gs500gbi')

    print('\n>> Falcon')
    data = load_evidence_set("../data/evidence/asist/falcon")
    evaluate_common_behavior(data, 10, '../data/plots/asist/falcon', 'state_freq_falcon_0eu_5s_5000gs500gbi')


if __name__ == "__main__":
    NUM_SAMPLES = 100
    NUM_GIBBS_SAMPLES = 5000
    GIBBS_BURN_IN = 500
    HORIZONS = [1, 3, 5, 10, 15, 30]

    experiment_common_behavior()
    experiment_transitions()
    # experiment_generate_synthetic_data(100, NUM_GIBBS_SAMPLES, GIBBS_BURN_IN, 0)
    # experiment_eval_performance_synthetic(NUM_SAMPLES, NUM_GIBBS_SAMPLES, GIBBS_BURN_IN, HORIZONS)
    # experiment_eval_performance_real(NUM_GIBBS_SAMPLES, GIBBS_BURN_IN, HORIZONS)
    # experiment_check_with_toy_model()
    # experiment_check_with_asist_model()

    # model = Model()
    # model.init_from_mission_map(MissionMap.SPARKY)
    # prior = model.get_theta_s_priors()
    # print('Stop')
