import math

import numpy as np
from sqlalchemy import func, select
from sqlalchemy.orm import Session

from datasette_interface.database.entity.task.minecraft_task import \
    MinecraftMission
from datasette_interface.database.entity.task.rest_state_task import \
    RestStateTask
from datasette_interface.database.entity.task.ping_pong_cooperative_task_observation import \
    PingPongCooperativeTaskObservation


def get_main_clock_timestamps(
        group_session: str, clock_frequency: int, buffer: int, db: Session
) -> np.ndarray:
    """
    Gets a time scale for a main clock with a fixed frequency. The main clock will be shared among
    all data synced to it. Therefore, it must begin and end encompassing all relevant experiment
    data. It's start timestamp is determined by the beginning of the first task (rest state) and
    end time by the end of the last task in the experiment (minecraft), with a buffer as a
    safeguard.

    :param group_session: group session of the main clock.
    :param clock_frequency: frequency of the main clock.
    :param buffer: a buffer in seconds to be sure we don't lose any signal data.
    :param db: active database session.
    """

    start_time = db.scalar(
        select(RestStateTask.start_timestamp_unix).where(
            RestStateTask.group_session_id == group_session
        )
    )

    end_time = db.scalar(
        select(func.max(MinecraftMission.trial_stop_timestamp_unix)).where(
            MinecraftMission.group_session_id == group_session
        )
    )

    if end_time is None:
        # There was no time to run the Minecraft task. Get last timestamp from the ping-pong
        # cooperative task.
        end_time = db.scalar(
            select(func.max(PingPongCooperativeTaskObservation.timestamp_unix)).where(
                PingPongCooperativeTaskObservation.group_session_id == group_session
            )
        )

    if start_time is None or end_time is None:
        return None

    start_time = float(start_time) - buffer
    end_time = float(end_time) + buffer

    # The arange function is not numerically stable with small float numbers. So we use the
    # number of points to create a sequence and scale the sequence by the inverse of the
    # frequency.
    num_time_points = math.ceil((end_time - start_time) * clock_frequency) + 1
    evenly_spaced_timestamps = start_time + np.arange(num_time_points) / clock_frequency

    return evenly_spaced_timestamps
