CREATE TABLE trial_info (
	trial_uuid VARCHAR NOT NULL,
	mission VARCHAR NOT NULL,
	trial_start_timestamp VARCHAR NOT NULL,
	trial_stop_timestamp VARCHAR NOT NULL,
	testbed_version VARCHAR NOT NULL,
	final_team_score INTEGER,
	PRIMARY KEY (trial_uuid)
);
