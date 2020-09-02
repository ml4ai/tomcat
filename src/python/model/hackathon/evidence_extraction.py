import os
from tqdm import tqdm
import numpy as np
import json
from hackathon import ta3_data_adapter as ta3_data_adapter
from hackathon.ta3_data_adapter import MissionMap
import datetime
from hackathon.utils import to_datetime, read_data_from_file

# Observable nodes
LT_EVIDENCE_FILENAME = "lights"
RM_EVIDENCE_FILENAME = "rooms"
TG_EVIDENCE_FILENAME = "triaging_green"
TY_EVIDENCE_FILENAME = "triaging_yellow"

# Parameter nodes
THETA_S_FILENAME = "theta_s"
PI_LT_FILENAME = "pi_lt"
THETA_RM_FILENAME = "theta_rm"
PI_TG_FILENAME = "pi_tg"
PI_TY_FILENAME = "pi_ty"

SINGLEPLAYER_AREA_ORDER = [
    "as",
    "achl",
    "ach",
    "alha",
    "arha",
    "alhb",
    "arhb",
    "awb",
    "amb",
    "ae1",
    "ae2",
    "ar201",
    "ar203",
    "ar205",
    "ar207",
    "ajc",
    "ar208",
    "ar210",
    "ar209",
    "ar211",
    "ar213",
    "ar215",
    "ar216",
    "ar218",
    "ar220",
]
SPARKY_AREA_ORDER = [
    "sr",
    "chl",
    "chlb",
    "ch",
    "lh1",
    "rh1",
    "lh2",
    "rh2",
    "wb",
    "mb",
    "e1",
    "e2",
    "r201",
    "r203",
    "r205",
    "r207",
    "jc",
    "r208",
    "r210",
    "r209",
    "r211",
    "r213",
    "r215",
    "r216",
    "r218",
    "r220",
]
FALCON_AREA_ORDER = [
    "ew",
    "fy1",
    "fy2",
    "el",
    "lh1",
    "lh2",
    "cht",
    "rh",
    "chb",
    "chm",
    "so",
    "br",
    "es1",
    "es2",
    "kcoe",
    "kco1",
    "kco2",
    "tkt",
    "r101",
    "r102",
    "r103",
    "r104",
    "r105",
    "r106",
    "r107",
    "r109",
    "r109",
    "r110",
    "r111",
    "cf",
    "jc",
    "mb",
    "wb",
    "acr",
    "mkcr",
    "hcr",
]


class EvidenceSet:
    """
    This class stores evidence data for the observable nodes used in the models
    """

    def __init__(self, lt_evidence, rm_evidence, tg_evidence, ty_evidence):
        self.lt_evidence = lt_evidence
        self.rm_evidence = rm_evidence
        self.tg_evidence = tg_evidence
        self.ty_evidence = ty_evidence

        if len(self.rm_evidence.shape) == 2:
            self.number_of_data_points, self.time_slices = rm_evidence.shape
        else:
            self.number_of_data_points = 1
            self.time_slices = len(rm_evidence)

    def cut(self, num_samples_to_keep):
        self.lt_evidence = self.lt_evidence[0:num_samples_to_keep, :]
        self.rm_evidence = self.rm_evidence[0:num_samples_to_keep, :]
        self.tg_evidence = self.tg_evidence[0:num_samples_to_keep, :]
        self.ty_evidence = self.ty_evidence[0:num_samples_to_keep, :]
        self.number_of_data_points = num_samples_to_keep

def convert_experiments_data_to_evidence_set(
    experiments_folder,
    evidence_set_folder,
    mission_map_id,
    time_gap,
    time_slices,
    binary_area,
    show_progress=True,
):
    """
    This function converts the raw data from the testbed to a format that can be handled by the ToMCAT models.
    Each row is composed by observations from a complete mission and each column represents the time step when
    that observation took place.
    """
    lt_evidence_set = []
    rm_evidence_set = []
    tg_evidence_set = []
    ty_evidence_set = []

    for experiment_id in tqdm(
        os.listdir(experiments_folder),
        desc="Extracting evidence set from data",
        disable=not show_progress,
    ):
        if experiment_id.startswith("."):
            continue

        lt, rm, tg, ty = get_evidence_from_experiment(
            os.path.join(experiments_folder, experiment_id),
            mission_map_id,
            time_gap,
            time_slices,
        )

        if binary_area:
            if mission_map_id == MissionMap.SPARKY:
                rm[rm < 8] = 0
                rm[rm >= 8] = 1
            elif mission_map_id == MissionMap.FALCON:
                rm[rm < 10] = 0
                rm[rm >= 10] = 1

        lt_evidence_set.append(lt)
        rm_evidence_set.append(rm)
        tg_evidence_set.append(tg)
        ty_evidence_set.append(ty)

    save_individual_evidence_set(
        evidence_set_folder, LT_EVIDENCE_FILENAME, lt_evidence_set
    )
    save_individual_evidence_set(
        evidence_set_folder, RM_EVIDENCE_FILENAME, rm_evidence_set
    )
    save_individual_evidence_set(
        evidence_set_folder, TG_EVIDENCE_FILENAME, tg_evidence_set
    )
    save_individual_evidence_set(
        evidence_set_folder, TY_EVIDENCE_FILENAME, ty_evidence_set
    )


def get_evidence_from_experiment(
    experiment_folder, mission_map_id, time_gap, time_slices
):
    """
    This function extracts observations along time for a single experiment. The observations are measured from time to
    time, defined by the time_gap variable. Since the timestamps in the data may not match perfectly with the time gap
    defined, the last measurement before a given timestamp is used as observed value for that specific timestamp.
    """
    observations = read_data_from_file(
        os.path.join(experiment_folder, ta3_data_adapter.OBSERVATIONS_FILENAME)
    )
    lever_events = read_data_from_file(
        os.path.join(experiment_folder, ta3_data_adapter.LEVER_FILENAME)
    )
    room_events = read_data_from_file(
        os.path.join(experiment_folder, ta3_data_adapter.ROOM_FILENAME)
    )
    triage_events = read_data_from_file(
        os.path.join(experiment_folder, ta3_data_adapter.TRIAGE_FILENAME)
    )

    areas = get_list_of_areas(mission_map_id)
    area_lit = get_initial_light_area_map(mission_map_id)
    current_time_slice = get_mission_start_timestamp(experiment_folder)

    # most_recent_observation = observations[0]
    area_index = 0
    triaging_green = False
    triaging_yellow = False

    observation_counter = 0
    lever_counter = 0
    room_counter = 0
    triage_counter = 0

    lt_evidence = np.zeros(time_slices, dtype=np.int)
    rm_evidence = np.zeros(time_slices, dtype=np.int)
    tg_evidence = np.zeros(time_slices, dtype=np.int)
    ty_evidence = np.zeros(time_slices, dtype=np.int)

    for t in range(time_slices):
        while (
            observation_counter < len(observations)
            and to_datetime(
                observations[observation_counter]["header"]["timestamp"]
            )
            <= current_time_slice
        ):
            # todo - so far we don't need to extract information from the observations. Later this will be necessary.
            #  However, keep this loop to check whether the mission was executed completely. There will be an error
            #  when there's no observations to read but the number of time slices is still small
            # most_recent_observation = observations[observation_counter]
            observation_counter += 1

        # Get room event in the time slice
        while (
            room_counter < len(room_events)
            and to_datetime(room_events[room_counter]["header"]["timestamp"])
            <= current_time_slice
        ):
            area_id = room_events[room_counter]["data"]["entered_area_id"]
            if area_id in areas:
                area_index = areas.index(
                    room_events[room_counter]["data"]["entered_area_id"]
                )

            room_counter += 1

        # Get light event in the time slice
        while (
            lever_counter < len(lever_events)
            and to_datetime(lever_events[lever_counter]["header"]["timestamp"])
            <= current_time_slice
        ):
            area_lit[area_index] = lever_events[lever_counter]["data"][
                "powered"
            ]
            lever_counter += 1

        # Get triaging event in the time slice
        while (
            triage_counter < len(triage_events)
            and to_datetime(
                triage_events[triage_counter]["header"]["timestamp"]
            )
            <= current_time_slice
        ):
            if (
                triage_events[triage_counter]["data"]["triage_state"]
                == "IN_PROGRESS"
            ):
                if triage_events[triage_counter]["data"]["color"] == "Green":
                    triaging_green = True
                else:
                    triaging_yellow = True
            else:
                triaging_green = False
                triaging_yellow = False

            triage_counter += 1

        lt_evidence[t] = 1 if area_lit[area_index] else 0
        rm_evidence[t] = area_index
        tg_evidence[t] = 1 if triaging_green else 0
        ty_evidence[t] = 1 if triaging_yellow else 0

        current_time_slice = current_time_slice + datetime.timedelta(
            0, time_gap
        )

    return lt_evidence, rm_evidence, tg_evidence, ty_evidence


def get_initial_light_area_map(mission_map_id):
    """
    This function returns the initial mapping for the areas that are light when the mission starts
    """
    light_areas = []

    if mission_map_id == MissionMap.SINGLEPLAYER:
        light_areas = [False] * get_number_of_areas(mission_map_id)
        light_areas[0] = True  # staging room
        light_areas[2:11] = [True] * 9  # These rooms are always light
    elif mission_map_id == MissionMap.SPARKY:
        light_areas = [False] * get_number_of_areas(mission_map_id)
        light_areas[0] = True  # staging room
        light_areas[3:12] = [True] * 9  # These rooms are always light
    elif mission_map_id == MissionMap.FALCON:
        light_areas = [False] * get_number_of_areas(mission_map_id)
        light_areas[0:10] = [
            True
        ] * 10  # Hallways and external areas are always light
        light_areas[10:12] = [
            True,
            True,
        ]  # Open break area and security office don't have a light switch
        light_areas[
            29
        ] = True  # The computer farms doesn't have a light switch
        light_areas[31:33] = [
            True,
            True,
        ]  # The bathrooms don't have a light switch

    return light_areas


def get_number_of_areas(mission_map_id):
    """
    This function returns the number of areas based on the mission map
    """
    if mission_map_id == MissionMap.SINGLEPLAYER:
        return len(SINGLEPLAYER_AREA_ORDER)
    elif mission_map_id == MissionMap.SPARKY:
        return len(SPARKY_AREA_ORDER)
    elif mission_map_id == MissionMap.FALCON:
        return len(FALCON_AREA_ORDER)
    elif mission_map_id == MissionMap.SHARED:
        return 6

    return 0


def get_mission_start_timestamp(experiment_folder):
    """
    This function checks the timestamp when the player left the starting area which is when the mission effectively
    starts
    """
    metadata_filepath = os.path.join(
        experiment_folder, ta3_data_adapter.METADATA_FILENAME
    )
    with open(metadata_filepath, "r") as metadata_file:
        metadata = json.load(metadata_file)

    return to_datetime(metadata["start_time"])


def get_list_of_areas(mission_map_id):
    """
    This function returns the list of ordered areas for a mission map
    """
    if mission_map_id == MissionMap.SINGLEPLAYER:
        return SINGLEPLAYER_AREA_ORDER
    elif mission_map_id == MissionMap.SPARKY:
        return SPARKY_AREA_ORDER
    elif mission_map_id == MissionMap.FALCON:
        return FALCON_AREA_ORDER

    return None


def get_area_by_player_position(areas, x, y):
    """
    This function checks the area the player is in given its position in the maps
    """
    epsilon = 1  # Variance of 1 block
    for i, area in areas:
        if (
            area["x1"] - epsilon <= x <= area["x2"] + epsilon
            and area["y1"] - epsilon <= y <= area["y2"] + epsilon
        ):
            return i, area["id"]


def save_individual_evidence_set(destiny_folder, filename, evidence_set):
    """
    This function saves an evidence set to a file
    """
    np.savetxt(
        get_evidence_file_absolute_path(destiny_folder, filename),
        evidence_set,
        fmt="%i",
    )

def save_evidence_set(destiny_folder, evidence_set):
    if not os.path.isdir(destiny_folder):
        os.mkdir(destiny_folder)

    save_individual_evidence_set(
        destiny_folder, LT_EVIDENCE_FILENAME, evidence_set.lt_evidence
    )
    save_individual_evidence_set(
        destiny_folder, RM_EVIDENCE_FILENAME, evidence_set.rm_evidence
    )
    save_individual_evidence_set(
        destiny_folder, TG_EVIDENCE_FILENAME, evidence_set.tg_evidence
    )
    save_individual_evidence_set(
        destiny_folder, TY_EVIDENCE_FILENAME, evidence_set.ty_evidence
    )

def get_evidence_file_absolute_path(destiny_folder, filename):
    """
    Retrieves the absolute path for an evidence file
    """
    return os.path.join(destiny_folder, filename + ".csv")


def load_evidence_set(evidence_folder, no_light=False):
    """
    Loads evidence from files to matrices
    """
    lt_evidence_set = np.array([])
    if not no_light:
        lt_evidence_set = np.loadtxt(
            get_evidence_file_absolute_path(evidence_folder, LT_EVIDENCE_FILENAME),
            dtype=np.int,
        )
    rm_evidence_set = np.loadtxt(
        get_evidence_file_absolute_path(evidence_folder, RM_EVIDENCE_FILENAME),
        dtype=np.int,
    )
    tg_evidence_set = np.loadtxt(
        get_evidence_file_absolute_path(evidence_folder, TG_EVIDENCE_FILENAME),
        dtype=np.int,
    )
    ty_evidence_set = np.loadtxt(
        get_evidence_file_absolute_path(evidence_folder, TY_EVIDENCE_FILENAME),
        dtype=np.int,
    )

    return EvidenceSet(
        lt_evidence_set, rm_evidence_set, tg_evidence_set, ty_evidence_set
    )



if __name__ == "__main__":
    TIME_GAP = 1
    TIME_SLICES = 600
    # convert_experiments_data_to_evidence_set('../data/experiments/asist/formatted/sparky',
    #                                          '../data/evidence/asist/sparky', MissionMap.SINGLEPLAYER, TIME_GAP,
    #                                          TIME_SLICES)
    #
    # convert_experiments_data_to_evidence_set('../data/experiments/asist/formatted/falcon',
    #                                          '../data/evidence/asist/falcon', MissionMap.FALCON, TIME_GAP,
    #                                          TIME_SLICES)

    convert_experiments_data_to_evidence_set('../data/experiments/asist/formatted/sparky',
                                             '../data/evidence/asist/sparky/binary_area', MissionMap.SPARKY, TIME_GAP,
                                             TIME_SLICES, True)

    convert_experiments_data_to_evidence_set('../data/experiments/asist/formatted/falcon',
                                             '../data/evidence/asist/falcon/binary_area', MissionMap.FALCON, TIME_GAP,
                                             TIME_SLICES, True)
