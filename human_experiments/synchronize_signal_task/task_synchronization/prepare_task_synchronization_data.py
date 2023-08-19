from read_data import read_task_db_all


def prepare_task_synchronization_data(synchronized_signals: list[dict[str, any]],
                                      db_path: str,
                                      num_processes: int = 1) -> list[dict[str, any]]:
    # Get experiment sessions
    experiments = []
    for experiment in synchronized_signals:
        experiments.append(experiment['experiment_name'])

    # Get task data
    task_data = read_task_db_all(db_path, experiments, num_processes)

    pairing = []
    for signal_dict in synchronized_signals:
        experiment_name = signal_dict['experiment_name']

        for task_dict in task_data:
            if task_dict['experiment_name'] == experiment_name:
                merged_dict = {
                    'experiment_name': experiment_name,
                    'signal': signal_dict,
                    'task': task_dict
                }
                pairing.append(merged_dict)
                break

    return pairing
