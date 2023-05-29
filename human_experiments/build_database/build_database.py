#!/usr/bin/env python

import contextlib
import os
import json
from dataclasses import dataclass
from glob import glob
import logging
from logging import info, warning
from typing import List, Optional
from sqlalchemy import create_engine, String
from sqlalchemy.orm import (
    DeclarativeBase,
    Mapped,
    mapped_column,
    relationship,
    MappedAsDataclass,
    Session,
)
from tqdm import tqdm

logging.basicConfig(level=logging.INFO)


@contextlib.contextmanager
def cd(path):
    old_path = os.getcwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(old_path)


class Base(DeclarativeBase, MappedAsDataclass):
    pass


class TrialInfo(Base):
    __tablename__ = "trial_info"
    trial_uuid: Mapped[str] = mapped_column(primary_key=True)
    mission: Mapped[str]
    trial_start_timestamp: Mapped[str]
    trial_stop_timestamp: Mapped[str]
    testbed_version: Mapped[str]
    final_team_score: Mapped[Optional[int]]


def process_metadata_file(filepath, engine):
    trial_uuid = None
    mission = None
    trial_start_timestamp = None
    trial_stop_timestamp = None
    testbed_version = None
    final_team_score = None
    scores = []

    with open(filepath) as f:
        for line in f:
            message = json.loads(line)
            topic = message["topic"]
            if topic == "trial":
                trial_uuid = message["msg"]["trial_id"]
                if message["msg"]["sub_type"] == "start":
                    mission = message["data"]["experiment_mission"]
                    trial_start_timestamp = message["header"]["timestamp"]
                    testbed_version = message["data"]["testbed_version"]
                elif message["msg"]["sub_type"] == "stop":
                    trial_stop_timestamp = message["header"]["timestamp"]

            if topic == "observations/events/scoreboard":
                score = message["data"]["scoreboard"]["TeamScore"]
                scores.append(score)

    if len(scores) != 0:
        warning(f"No scoreboard messages found in {filepath}!")
        final_team_score = scores[-1]

    trial_info = TrialInfo(
        trial_uuid,
        mission,
        trial_start_timestamp,
        trial_stop_timestamp,
        testbed_version,
        final_team_score,
    )

    with Session(engine) as session:
        session.add(trial_info)
        session.commit()


if __name__ == "__main__":
    info("Processing directories...")
    engine = create_engine("sqlite:///test.db")
    Base.metadata.create_all(engine)
    with cd("/tomcat/data/raw/LangLab/experiments/study_3_pilot/group"):
        for session in tqdm(sorted(os.listdir("."))):
            year, month, day, hour = [int(x) for x in session.split("_")[1:]]

            if year == 2022 and ((month < 9) or (month == 9 and day < 30)):
                info(
                    f"Ignoring {session} since our first pilot with real "
                    "participants was on 9/30/2022"
                )
                continue

            try:
                with cd(session + "/minecraft"):
                    info(f"Processing directory {session}")
                    metadata_files = sorted(glob("*.metadata"))
                    # print(metadata_files)
                    for metadata_file in metadata_files:
                        info(f"Processing file {metadata_file}")
                        process_metadata_file(metadata_file, engine)

            except FileNotFoundError:
                warning(f"minecraft directory not in {session}")
