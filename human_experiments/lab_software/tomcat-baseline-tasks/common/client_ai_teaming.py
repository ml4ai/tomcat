def client_ai_teaming(to_client_connections: dict, from_client_connections: dict) -> tuple:
    client_names = list(from_client_connections.values())

    ai_team_names = []
    for client_name in client_names:
        if "ai" in client_name:
            ai_team_names.append(client_name)

    client_pairs = []

    to_client_connection_team_ai = {}
    from_client_connection_team_ai = {}
    to_client_connection_team_non_ai = {}
    from_client_connection_team_non_ai = {}
    for from_client_connection, name in from_client_connections.items():
        if name in ai_team_names:
            from_client_connection_team_ai[from_client_connection] = name
            to_client_connection_team_ai[name] = to_client_connections[name]
        else:
            from_client_connection_team_non_ai[from_client_connection] = name
            to_client_connection_team_non_ai[name] = to_client_connections[name]

    from_client_connection_teams = (from_client_connection_team_non_ai, from_client_connection_team_ai)
    to_client_connection_teams = (to_client_connection_team_non_ai, to_client_connection_team_ai)

    client_pair = (to_client_connection_teams, from_client_connection_teams)
    client_pairs.append(client_pair)

    return client_pairs
