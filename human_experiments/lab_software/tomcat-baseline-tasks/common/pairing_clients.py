from .chunking import chunking


def pairing_clients(to_client_connections: dict, from_client_connections: dict) -> tuple:
    client_names = list(from_client_connections.values())
    assert len(client_names) % 2 == 0

    name_pairings = chunking(client_names, 2)

    client_pairs = []
    for (name_1, name_2) in name_pairings:
        to_client_connection_team_1 = {}
        to_client_connection_team_2 = {}
        from_client_connection_team_1 = {}
        from_client_connection_team_2 = {}
        for from_client_connection, client_name in from_client_connections.items():
            if client_name == name_1:
                from_client_connection_team_1[from_client_connection] = client_name
                to_client_connection_team_1[client_name] = to_client_connections[client_name]
            elif client_name == name_2:
                from_client_connection_team_2[from_client_connection] = client_name
                to_client_connection_team_2[client_name] = to_client_connections[client_name]

        from_client_connection_pair = (from_client_connection_team_1, from_client_connection_team_2)
        to_client_connection_pair = (to_client_connection_team_1, to_client_connection_team_2)

        client_pair = (to_client_connection_pair, from_client_connection_pair)
        client_pairs.append(client_pair)

    return client_pairs
