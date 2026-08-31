#!/usr/bin/env python

from dataset_pipeline.database.config import Base, engine  # noqa: F401
from dataset_pipeline.database.entity.base.data_validity import DataValidity  # noqa: F401
from dataset_pipeline.database.entity.base.eeg_device import EEGDevice  # noqa: F401
from dataset_pipeline.database.entity.base.group_session import GroupSession  # noqa: F401
from dataset_pipeline.database.entity.base.modality import Modality  # noqa: F401
from dataset_pipeline.database.entity.base.participant import Participant  # noqa: F401
from dataset_pipeline.database.entity.base.post_game_survey import PostGameSurvey  # noqa: F401
from dataset_pipeline.database.entity.base.station import Station  # noqa: F401
from dataset_pipeline.database.entity.base.task import Task  # noqa: F401
from dataset_pipeline.database.entity.derived.eeg_sync import EEGSync  # noqa: F401
from dataset_pipeline.database.entity.derived.ekg_sync import EKGSync  # noqa: F401
from dataset_pipeline.database.entity.derived.fnirs_sync import FNIRSSync  # noqa: F401
from dataset_pipeline.database.entity.derived.gsr_sync import GSRSync  # noqa: F401
from dataset_pipeline.database.entity.signal.audio_vocalics import AudioVocalics  # noqa: F401
from dataset_pipeline.database.entity.signal.eeg import EEGRaw  # noqa: F401
from dataset_pipeline.database.entity.signal.fnirs import FNIRSRaw  # noqa: F401
from dataset_pipeline.database.entity.signal.gaze import GAZERaw  # noqa: F401
from dataset_pipeline.database.entity.signal.screen_capture import ScreenCapture  # noqa: F401
from dataset_pipeline.database.entity.task.minecraft_task import (  # noqa: F401
    MinecraftMission,
    MinecraftTestbedMessage,
)
from dataset_pipeline.database.entity.task.rest_state_task import RestStateTask  # noqa: F401
from dataset_pipeline.database.entity.task.affective_task_event import (  # noqa: F401
    AffectiveTaskEvent
)
from dataset_pipeline.database.entity.task.finger_tapping_task_observation import (  # noqa: F401
    FingerTappingTaskObservation
)
from dataset_pipeline.database.entity.task.ping_pong_competitive_task_observation import (  # noqa: F401
    PingPongCompetitiveTaskObservation
)
from dataset_pipeline.database.entity.task.ping_pong_cooperative_task_observation import (  # noqa: F401
    PingPongCooperativeTaskObservation
)

Base.metadata.create_all(engine, checkfirst=True)
