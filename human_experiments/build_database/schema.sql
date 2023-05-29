CREATE TABLE trial_info (
	trial_uuid VARCHAR NOT NULL,
    team_id INTEGER NOT NULL,
	mission VARCHAR NOT NULL,
	trial_start_timestamp VARCHAR NOT NULL,
	trial_stop_timestamp VARCHAR NOT NULL,
	testbed_version VARCHAR NOT NULL,
	final_team_score INTEGER,
	PRIMARY KEY (trial_uuid)
    FOREIGN KEY(team_id) REFERENCES participant(team_id)
);

CREATE TABLE participant (
    id INTEGER NOT NULL,
    team_id INTEGER,
    PRIMARY KEY (id)
);
