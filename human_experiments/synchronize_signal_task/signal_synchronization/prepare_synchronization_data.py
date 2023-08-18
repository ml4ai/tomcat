def _common_experiment_names(signal_type_info: list[dict[str, any]]):
    experiment_name_sets = []

    for signal_info in signal_type_info:
        names = {experiment["experiment_name"] for experiment in signal_info["experiment_signals"]}
        experiment_name_sets.append(names)

    common_names = set.intersection(*experiment_name_sets)

    return sorted(list(common_names))


def prepare_synchronization_data(signal_type_info: list[dict[str, any]], desired_freq: int) -> list[dict[str, any]]:
    experiments_to_process = _common_experiment_names(signal_type_info)

    sync_experiments_info = []
    for experiment_to_process in experiments_to_process:
        sync_experiment_info = {
            "experiment_name": experiment_to_process,
            "desired_freq": desired_freq,
        }

        sync_signals_info = []
        for signal_info in signal_type_info:
            sync_signal_info = {
                "signal_type": signal_info["signal_type"],
                "frequency": signal_info["recording_frequency"]
            }

            for experiment_signal in signal_info["experiment_signals"]:
                if experiment_signal["experiment_name"] == experiment_to_process:
                    for station in ["lion", "tiger", "leopard"]:
                        if station in experiment_signal:
                            sync_signal_info[station] = experiment_signal[station]

            sync_signals_info.append(sync_signal_info)

        sync_experiment_info["signals"] = sync_signals_info

        sync_experiments_info.append(sync_experiment_info)

    return sync_experiments_info
