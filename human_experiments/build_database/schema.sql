CREATE TABLE participant (
  id TEXT PRIMARY KEY,
  is_confederate INTEGER DEFAULT 0
) STRICT;
CREATE TABLE group_session (
  id TEXT PRIMARY KEY,
  lion_participant_id TEXT NOT NULL,
  tiger_participant_id TEXT NOT NULL,
  leopard_participant_id TEXT NOT NULL,
  FOREIGN KEY(lion_participant_id) REFERENCES participant(id),
  FOREIGN KEY(tiger_participant_id) REFERENCES participant(id),
  FOREIGN KEY(leopard_participant_id) REFERENCES participant(id)
) STRICT;
CREATE TABLE mission (
  id INTEGER PRIMARY KEY,
  group_session_id TEXT,
  name TEXT,
  testbed_version TEXT,
  final_team_score TEXT,
  FOREIGN KEY(group_session_id) REFERENCES group_session(id)
) STRICT;
CREATE TABLE minecraft_event (
  timestamp TEXT,
  group_session_id TEXT,
  mission TEXT,
  testbed_version TEXT,
  final_team_score TEXT,
  raw_message TEXT,
  FOREIGN KEY(group_session_id) REFERENCES group_session(id)
) STRICT;

