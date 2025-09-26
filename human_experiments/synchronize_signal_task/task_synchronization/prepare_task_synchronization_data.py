from read_data import read_task_db_all


def prepare_task_synchronization_data(synchronized_signals: list[dict[str, any]],
                                      num_processes: int = 1) -> list[dict[str, any]]:
    # Get experiment sessions
    experiments = []
    for experiment in synchronized_signals:
        experiments.append(experiment['experiment_name'])

    # Get task data
    task_data = read_task_db_all(experiments, num_processes)

    pairing = []
    for signal_dict in synchronized_signals:
        experiment_name = signal_dict['experiment_name']
        signals = signal_dict['signals']
        signal_types = signal_dict['signal_types']

        for task_dict in task_data:
            if task_dict['experiment_name'] == experiment_name:
                merged_dict = {
                    'experiment_name': experiment_name,
                    'signals': signals,
                    'signal_types': signal_types,
                    'tasks': task_dict["task_data"]
                }
                pairing.append(merged_dict)
                break

    pairing.sort(key=lambda x: x['experiment_name'])

    return pairing
