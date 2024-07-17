#!/usr/bin/env python

from datasette_interface.database.config import Base, engine  # noqa f401
from datasette_interface.database.entity.base.data_validity import DataValidity  # noqa f401
from datasette_interface.database.entity.base.eeg_device import EEGDevice  # noqa f401
from datasette_interface.database.entity.base.group_session import GroupSession  # noqa f401
from datasette_interface.database.entity.base.modality import Modality  # noqa f401
from datasette_interface.database.entity.base.participant import Participant  # noqa f401
from datasette_interface.database.entity.base.station import Station  # noqa f401
from datasette_interface.database.entity.base.task import Task  # noqa f401
from datasette_interface.database.entity.derived.eeg_sync import EEGSync  # noqa f401
from datasette_interface.database.entity.derived.ekg_sync import EKGSync  # noqa f401
from datasette_interface.database.entity.derived.fnirs_sync import FNIRSSync  # noqa f401
from datasette_interface.database.entity.derived.gsr_sync import GSRSync  # noqa f401
from datasette_interface.database.entity.signal.audio_vocalics import AudioVocalics  # noqa f401
from datasette_interface.database.entity.signal.eeg import EEGRaw  # noqa f401
from datasette_interface.database.entity.signal.fnirs import FNIRSRaw  # noqa f401
from datasette_interface.database.entity.signal.gaze import GAZERaw  # noqa f401
from datasette_interface.database.entity.signal.screen_capture import ScreenCapture  # noqa f401
from datasette_interface.database.entity.task.affective_task_event import (
    AffectiveTaskEvent,
)  # noqa f401
from datasette_interface.database.entity.task.finger_tapping_task_observation import (
    FingerTappingTaskObservation,
)  # noqa f401
from datasette_interface.database.entity.task.minecraft_task import (  # noqa f401
    MinecraftMission,
    MinecraftTestbedMessage,
)
from datasette_interface.database.entity.task.ping_pong_competitive_task_observation import (
    PingPongCompetitiveTaskObservation,
)  # noqa f401
from datasette_interface.database.entity.task.ping_pong_cooperative_task_observation import (
    PingPongCooperativeTaskObservation,
)  # noqa f401
from datasette_interface.database.entity.task.rest_state_task import RestStateTask  # noqa f401

Base.metadata.create_all(engine, checkfirst=True)
