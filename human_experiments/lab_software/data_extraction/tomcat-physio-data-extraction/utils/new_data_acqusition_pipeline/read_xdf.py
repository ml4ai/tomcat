import shutil
from termcolor import colored
import pyxdf

def read_xdf(
    xdf_file_paths,
    extract_pkl,
    extract_csv,
    extract_hdf5,
    exclude,
    output_path,
    rootdir_xdf,
):
    from utils import read_nirs, read_eeg, read_gaze, create_baseline_task_directory, read_rest_state_timestamps, read_finger_tapping_time, read_affective_task_timestamps_team, read_affective_task_timestamps_individual, read_ping_pong_timestamps, save_EEG, save_NIRS, save_Gaze, tasks_merge

    """
    Read the XDF files.
    """
    columns = shutil.get_terminal_size().columns
    for path in xdf_file_paths:
        if "block_1" in path:
            print(
                colored("block_1 ", "magenta", attrs=["bold", "blink"]).center(columns)
            )
            block_1, _ = pyxdf.load_xdf(path)

            (
                lion_0297_block_1_NIRS,
                tiger_0239_block_1_NIRS,
                leopard_0171_block_1_NIRS,
            ) = read_nirs(
                block_1
            )  # 1.1 Read NIRS timerseries data and its timestamps

            (
                lion_0297_block_1_EEG,
                tiger_0239_block_1_EEG,
                leopard_0171_block_1_EEG,
            ) = read_eeg(
                block_1
            )  # 1.2 Read EEG timerseries data and its timestamps

            (
                lion_0297_block_1_Gaze,
                tiger_0239_block_1_Gaze,
                leopard_0171_block_1_Gaze,
            ) = read_gaze(
                block_1
            )  # 1.3 Read Gaze timerseries data and its timestamps

            create_baseline_task_directory(
                rootdir_xdf, output_path
            )  # 0. Create directories for each task
            read_rest_state_timestamps(
                block_1, rootdir_xdf, output_path
            )  # 2. Read RestState timestamps
            read_finger_tapping_time(
                block_1, rootdir_xdf, output_path
            )  # 3. Read FingerTapping timestamps
            read_affective_task_timestamps_individual(
                block_1, rootdir_xdf, output_path
            )  # 4. Read AffectiveTask timestamps
            read_affective_task_timestamps_team(
                block_1, rootdir_xdf, output_path
            )  # 5. Read AffectiveTask timestamps
            read_ping_pong_timestamps(
                block_1, rootdir_xdf, output_path
            )  # 6. Read PingPong timestamps

        elif "block_2" in path:
            print(
                colored("block_2 ", "magenta", attrs=["bold", "blink"]).center(columns)
            )
            block_2, _ = pyxdf.load_xdf(path)
            (
                lion_0297_block_2_NIRS,
                tiger_0239_block_2_NIRS,
                leopard_0171_block_2_NIRS,
            ) = read_nirs(
                block_2
            )  # 1.1 Read NIRS data

            (
                lion_0297_block_2_EEG,
                tiger_0239_block_2_EEG,
                leopard_0171_block_2_EEG,
            ) = read_eeg(
                block_2
            )  # 1.2 Read EEG data

            (
                lion_0297_block_2_Gaze,
                tiger_0239_block_2_Gaze,
                leopard_0171_block_2_Gaze,
            ) = read_gaze(
                block_2
            )  # 1.3 Read Gaze data

            # minecraft_markers = read_minecraft_timestamps(
            #     block_2, PingPong_markers
            # )  # 2. Read Minecraft timestamps

    # Merge NIRS block 1 and block 2
    lion_0297_block_NIRS, tiger_0239_block_NIRS, leopard_0171_block_NIRS = tasks_merge(
        lion_0297_block_1_NIRS,
        tiger_0239_block_1_NIRS,
        leopard_0171_block_1_NIRS,
        lion_0297_block_2_NIRS,
        tiger_0239_block_2_NIRS,
        leopard_0171_block_2_NIRS,
    )

    # Merge EEG block 1 and block 2
    lion_0297_block_EEG, tiger_0239_block_EEG, leopard_0171_block_EEG = tasks_merge(
        lion_0297_block_1_EEG,
        tiger_0239_block_1_EEG,
        leopard_0171_block_1_EEG,
        lion_0297_block_2_EEG,
        tiger_0239_block_2_EEG,
        leopard_0171_block_2_EEG,
    )

    # Merge Gaze block 1 and block 2
    lion_0297_block_Gaze, tiger_0239_block_Gaze, leopard_0171_block_Gaze = tasks_merge(
        lion_0297_block_1_Gaze,
        tiger_0239_block_1_Gaze,
        leopard_0171_block_1_Gaze,
        lion_0297_block_2_Gaze,
        tiger_0239_block_2_Gaze,
        leopard_0171_block_2_Gaze,
    )

    # Filter and save the NIRS data
    save_NIRS(
        lion_0297_block_NIRS,
        tiger_0239_block_NIRS,
        leopard_0171_block_NIRS,
        rootdir_xdf,
        output_path,
        extract_pkl,
        extract_csv,
        extract_hdf5,
    )

    # Filter and save the EEG data
    save_EEG(
        lion_0297_block_EEG,
        tiger_0239_block_EEG,
        leopard_0171_block_EEG,
        rootdir_xdf,
        output_path,
        extract_pkl,
        extract_csv,
        extract_hdf5,
    )

    # Filter and save the Gaze data
    save_Gaze(
        lion_0297_block_Gaze,
        tiger_0239_block_Gaze,
        leopard_0171_block_Gaze,
        rootdir_xdf,
        output_path,
        extract_pkl,
        extract_csv,
        extract_hdf5,
    )