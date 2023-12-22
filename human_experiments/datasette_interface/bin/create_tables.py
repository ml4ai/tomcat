#!/usr/bin/env python

from datasette_interface.database.entity.base.data_validity import DataValidity
from datasette_interface.database.entity.base.eeg_device import EEGDevice
from datasette_interface.database.entity.base.group_session import GroupSession
from datasette_interface.database.entity.base.modality import Modality
from datasette_interface.database.entity.base.participant import Participant
from datasette_interface.database.entity.base.station import Station
from datasette_interface.database.entity.base.task import Task
from datasette_interface.database.entity.signal.eeg import EEGRaw
from datasette_interface.database.entity.signal.fnirs import FNIRSRaw
from datasette_interface.database.entity.signal.gaze import GAZERaw
from datasette_interface.database.entity.signal.screen_capture import ScreenCapture
from datasette_interface.database.entity.task.affective_task_event import AffectiveTaskEvent
from datasette_interface.database.entity.task.finger_tapping_task_observation import \
    FingerTappingTaskObservation
from datasette_interface.database.entity.task.minecraft_task import MinecraftMission
from datasette_interface.database.entity.task.ping_pong_competitive_task_observation import \
    PingPongCompetitiveTaskObservation
from datasette_interface.database.entity.task.ping_pong_cooperative_task_observation import \
    PingPongCooperativeTaskObservation
from datasette_interface.database.entity.task.rest_state_task import RestStateTask
from datasette_interface.database.entity.derived.audio_vocalics import AudioVocalics
from datasette_interface.database.entity.base.base import Base
from datasette_interface.database.config import engine
from datasette_interface.common.config import settings
from datasette_interface.raw.common.process_raw_signals import create_indices


Base.metadata.create_all(engine, checkfirst=True)
