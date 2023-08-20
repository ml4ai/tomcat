CREATE VIEW rest_state AS
SELECT dv.group_session, dv.participant, dv.station, rst.start_timestamp_unix, rst.stop_timestamp_unix from data_validity as dv
JOIN rest_state_task as rst
ON rst.group_session = dv.group_session
WHERE dv.modality='gaze'
  AND task = 'rest_state'


SELECT gaze.*, rest_state.participant FROM gaze_raw AS gaze
INNER JOIN rest_state
 ON gaze.group_session = rest_state.group_session
AND gaze.station = rest_state.station
WHERE
    timestamp_unix >= rest_state.start_timestamp_unix
    AND timestamp_unix < rest_state.stop_timestamp_unix



UPDATE gaze_raw
SET
    task='rest_state',
    participant=rest_state.participant
FROM
    gaze_raw AS gaze
INNER JOIN rest_state
 ON gaze.group_session = rest_state.group_session
AND gaze.station = rest_state.station
WHERE
    timestamp_unix >= rest_state.start_timestamp_unix
    AND timestamp_unix < rest_state.stop_timestamp_unix


exp_2022_10_07_15	Saturn_A	3
exp_2023_02_07_14	Hands-on Training	2
exp_2023_02_10_10	Hands-on Training	2

560d4c45-dc45-4e19-bdb3-e4e15021728a
a48f475f-40b0-46b9-8284-0db267dddb67
171a8713-a554-4d8e-a4b1-3ec1b728d0a2
9cde1985-1179-4aac-8b67-1fc60ed65243


