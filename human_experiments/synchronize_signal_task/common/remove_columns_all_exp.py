from .remove_columns import remove_columns


def remove_columns_all_exp(experiments: list[dict[str, any]], columns: list[str]):
    for experiment in experiments:
        for station in ["lion", "tiger", "leopard"]:
            if station not in experiment:
                continue

            experiment[station] = remove_columns(experiment[station], columns)
