from entity.task.rest_state_task import RestStateTask
from entity.task.affective_task_event import AffectiveTaskEvent
from entity.task.finger_tapping_task_observation import FingerTappingTaskObservation
from entity.task.ping_pong_competitive_task_observation import PingPongCompetitiveTaskObservation
from entity.task.ping_pong_cooperative_task_observation import PingPongCooperativeTaskObservation
from entity.task.minecraft_task import MinecraftMission

from sqlalchemy import func
from sqlalchemy import delete
from sqlalchemy import update

from logging import error


def label_signals_with_task(signal_modality_class, task, station, participant_id, start_timestamp, stop_timestamp,
                            database_session):
    stmt = (
        update(signal_modality_class)
        .where(signal_modality_class.timestamp_unix >= start_timestamp)
        .where(signal_modality_class.timestamp_unix < stop_timestamp)
        .where(signal_modality_class.station_id == station)
        .values(task_id=task, participant_id=participant_id)
    )

    database_session.execute(stmt)


def label_rest_state_task_data(signal_modality_class, group_session, station, participant_id, database_session):
    start_timestamp, stop_timestamp = database_session.query(RestStateTask.start_timestamp_unix,
                                                             RestStateTask.stop_timestamp_unix).filter(
        RestStateTask.group_session_id == group_session).first()

    # Update participant ID and label task.
    label_signals_with_task(signal_modality_class=signal_modality_class,
                            task="rest_state",
                            station=station,
                            participant_id=participant_id,
                            start_timestamp=start_timestamp,
                            stop_timestamp=stop_timestamp,
                            database_session=database_session)


def label_affective_task_individual_data(signal_modality_class, group_session, station, participant_id,
                                         database_session):
    start_timestamp, stop_timestamp = database_session.query(func.min(AffectiveTaskEvent.timestamp_unix),
                                                             func.max(AffectiveTaskEvent.timestamp_unix)).filter(
        AffectiveTaskEvent.group_session_id == group_session,
        AffectiveTaskEvent.task_type == "individual",
        AffectiveTaskEvent.participant_id == participant_id,
        AffectiveTaskEvent.event_type.in_(["start_affective_task", "final_submission"])
    ).first()

    # Update participant ID and label task.
    label_signals_with_task(signal_modality_class=signal_modality_class,
                            task="affective_individual",
                            station=station,
                            participant_id=participant_id,
                            start_timestamp=start_timestamp,
                            stop_timestamp=stop_timestamp,
                            database_session=database_session)


def label_affective_task_team_data(signal_modality_class, group_session, station, participant_id, database_session):
    start_timestamp, stop_timestamp = database_session.query(func.min(AffectiveTaskEvent.timestamp_unix),
                                                             func.max(AffectiveTaskEvent.timestamp_unix)).filter(
        AffectiveTaskEvent.group_session_id == group_session,
        AffectiveTaskEvent.task_type == "team",
        AffectiveTaskEvent.event_type.in_(["start_affective_task", "final_submission"])
    ).first()

    # Update participant ID and label task.
    label_signals_with_task(signal_modality_class=signal_modality_class,
                            task="affective_team",
                            station=station,
                            participant_id=participant_id,
                            start_timestamp=start_timestamp,
                            stop_timestamp=stop_timestamp,
                            database_session=database_session)


def label_finger_tapping_data(signal_modality_class, group_session, station, participant_id, database_session):
    start_timestamp, stop_timestamp = database_session.query(func.min(FingerTappingTaskObservation.timestamp_unix),
                                                             func.max(
                                                                 FingerTappingTaskObservation.timestamp_unix)).filter(
        FingerTappingTaskObservation.group_session_id == group_session
    ).first()

    # Update participant ID and label task.
    label_signals_with_task(signal_modality_class=signal_modality_class,
                            task="finger_tapping",
                            station=station,
                            participant_id=participant_id,
                            start_timestamp=start_timestamp,
                            stop_timestamp=stop_timestamp,
                            database_session=database_session)


def label_ping_pong_competitive_data(signal_modality_class, group_session, station, participant_id, database_session):
    start_timestamp, stop_timestamp = database_session.query(
        func.min(PingPongCompetitiveTaskObservation.timestamp_unix),
        func.max(
            PingPongCompetitiveTaskObservation.timestamp_unix)).filter(
        PingPongCompetitiveTaskObservation.group_session_id == group_session
    ).first()

    # Update participant ID and label task.
    label_signals_with_task(signal_modality_class=signal_modality_class,
                            task="ping_pong_competitive",
                            station=station,
                            participant_id=participant_id,
                            start_timestamp=start_timestamp,
                            stop_timestamp=stop_timestamp,
                            database_session=database_session)


def label_ping_pong_cooperative_data(signal_modality_class, group_session, station, participant_id, database_session):
    start_timestamp, stop_timestamp = database_session.query(
        func.min(PingPongCooperativeTaskObservation.timestamp_unix),
        func.max(
            PingPongCooperativeTaskObservation.timestamp_unix)).filter(
        PingPongCooperativeTaskObservation.group_session_id == group_session
    ).first()

    # Update participant ID and label task.
    label_signals_with_task(signal_modality_class=signal_modality_class,
                            task="ping_pong_cooperative",
                            station=station,
                            participant_id=participant_id,
                            start_timestamp=start_timestamp,
                            stop_timestamp=stop_timestamp,
                            database_session=database_session)


def label_minecraft_data(signal_modality_class, group_session, station, participant_id, task, database_session):
    if task == "saturn_a":
        mission = "Saturn_A"
    elif task == "saturn_b":
        mission = "Saturn_B"
    elif task == "hands_on_training":
        mission = "Hands-on Training"
    else:
        raise ValueError(f"Bad task: {task}!")

    start_timestamp, stop_timestamp = database_session.query(MinecraftMission.mission_start_timestamp_unix,
                                                             MinecraftMission.mission_stop_timestamp_unix).filter(
        MinecraftMission.group_session_id == group_session,
        MinecraftMission.name == mission,
    ).first()

    # Update participant ID and label task.
    label_signals_with_task(signal_modality_class=signal_modality_class,
                            task=task,
                            station=station,
                            participant_id=participant_id,
                            start_timestamp=start_timestamp,
                            stop_timestamp=stop_timestamp,
                            database_session=database_session)


def label_signals(signal_modality_class, group_session, task, station, participant_id, database_session):
    if task == "rest_state":
        if group_session == "exp_2022_12_05_12":
            error(
                f"""
                [MISSING DATA] There is no rest state data for
                {group_session}, due to technical issues. See Rick's email
                from 2023-07-11 for details."""
            )
        else:
            label_rest_state_task_data(
                signal_modality_class=signal_modality_class,
                group_session=group_session,
                station=station,
                participant_id=participant_id,
                database_session=database_session
            )

    elif "affective" in task:
        if task == "affective_individual":
            label_affective_task_individual_data(
                signal_modality_class=signal_modality_class,
                group_session=group_session,
                station=station,
                participant_id=participant_id,
                database_session=database_session
            )
        elif task == "affective_team":
            label_affective_task_team_data(
                signal_modality_class=signal_modality_class,
                group_session=group_session,
                station=station,
                participant_id=participant_id,
                database_session=database_session
            )
        else:
            raise ValueError(f"Bad task: {task}!")

    elif task == "finger_tapping":
        label_finger_tapping_data(
            signal_modality_class=signal_modality_class,
            group_session=group_session,
            station=station,
            participant_id=participant_id,
            database_session=database_session
        )

    elif task == "ping_pong_competitive":
        label_ping_pong_competitive_data(
            signal_modality_class=signal_modality_class,
            group_session=group_session,
            station=station,
            participant_id=participant_id,
            database_session=database_session
        )

    elif task == "ping_pong_cooperative":
        label_ping_pong_cooperative_data(
            signal_modality_class=signal_modality_class,
            group_session=group_session,
            station=station,
            participant_id=participant_id,
            database_session=database_session
        )

    else:
        label_minecraft_data(
            signal_modality_class=signal_modality_class,
            group_session=group_session,
            task=task,
            station=station,
            participant_id=participant_id,
            database_session=database_session
        )


def delete_invalid_signals(signal_modality_class, group_session, station, task, database_session):
    stmt = (
        delete(signal_modality_class)
        .where(signal_modality_class.group_session_id == group_session)
        .where(signal_modality_class.station_id == station)
        .where(signal_modality_class.task_id == task)
    )

    database_session.execute(stmt)
