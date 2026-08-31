#!/usr/bin/env python3
"""
Aggregate raw human-authored notes for each group session into a CSV.

Sources:
  - experiment_tracker.xlsx | Session Notes sheet (Rick's notes + REDCap team notes)
  - experiment_tracker.xlsx | DB Summary sheet (Participant/Equipment/Additional notes)
  - group/{session}/redcap_data/team_data.csv (participants_issues_details, etc.)

Output columns: group_session, raw_notes, provenance, source_type
"""

import argparse
import csv
import os

import pandas as pd

STUDY_DIR = "/tomcat/data/raw/LangLab/experiments/study_3_pilot"
TRACKER_PATH = os.path.join(STUDY_DIR, "experiment_tracker.xlsx")
GROUP_DIR = os.path.join(STUDY_DIR, "group")

SKIP_VALUES = {"nan", "no notes.", "no notes", "unknown", "none", ""}

SOURCE_TYPES = {
    "Rick's Experiment Notes": "experimenter_notes",
    "REDCap Team - Equipment Related Issues Notes": "equipment_issues",
    "REDCap Team - Additional Notes": "additional_notes",
    "Participant Issues": "participant_issues",
    "Equipment Issues": "equipment_issues",
    "Additional Notes": "additional_notes",
    "participants_issues_details": "participant_issues",
    "equipment_issues_details": "equipment_issues",
    "additional_notes": "additional_notes",
}


def clean(text: str) -> str:
    """Strip Excel Windows line-ending artifacts and normalize whitespace."""
    return text.replace("_x000D_", "\n").replace("\r\n", "\n").replace("\r", "\n").strip()


def is_empty(text: str) -> bool:
    return clean(text).lower() in SKIP_VALUES


def collect_rows(rows: list, session_id: str, raw: str, provenance: str, source_type: str):
    text = clean(str(raw))
    if not is_empty(text):
        rows.append(
            {
                "group_session": session_id,
                "raw_notes": text,
                "provenance": provenance,
                "source_type": source_type,
            }
        )


def collect_from_session_notes(rows: list, tracker_path: str):
    xl = pd.ExcelFile(tracker_path)
    df = xl.parse("Session Notes", header=0)
    note_cols = [
        "Rick's Experiment Notes",
        "REDCap Team - Equipment Related Issues Notes",
        "REDCap Team - Additional Notes",
    ]
    for _, row in df.iterrows():
        exp_id = str(row.get("Experiment ID", "")).strip()
        if not exp_id or exp_id.lower() == "nan":
            continue
        for col in note_cols:
            collect_rows(
                rows,
                exp_id,
                row.get(col, ""),
                f"experiment_tracker.xlsx | Session Notes | {col}",
                SOURCE_TYPES[col],
            )


def collect_from_db_summary(rows: list, tracker_path: str):
    xl = pd.ExcelFile(tracker_path)
    df = xl.parse("DB Summary", header=0)
    note_cols = ["Participant Issues", "Equipment Issues", "Additional Notes"]
    for _, row in df.iterrows():
        exp_id = str(row.get("Experiment ID", "")).strip()
        if not exp_id or exp_id.lower() == "nan":
            continue
        for col in note_cols:
            collect_rows(
                rows,
                exp_id,
                row.get(col, ""),
                f"experiment_tracker.xlsx | DB Summary | {col}",
                SOURCE_TYPES[col],
            )


def collect_from_redcap_csvs(rows: list, group_dir: str):
    note_cols = ["participants_issues_details", "equipment_issues_details", "additional_notes"]
    for session_id in sorted(os.listdir(group_dir)):
        csv_path = os.path.join(group_dir, session_id, "redcap_data", "team_data.csv")
        if not os.path.exists(csv_path):
            continue
        df = pd.read_csv(csv_path, dtype=str)
        for _, row in df.iterrows():
            for col in note_cols:
                collect_rows(
                    rows,
                    session_id,
                    row.get(col, ""),
                    f"group/{session_id}/redcap_data/team_data.csv | {col}",
                    SOURCE_TYPES[col],
                )


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--output",
        default="group_session_raw_notes.csv",
        help="Output CSV path (default: group_session_raw_notes.csv)",
    )
    parser.add_argument(
        "--study-dir",
        default=STUDY_DIR,
        help=f"Path to study_3_pilot directory (default: {STUDY_DIR})",
    )
    args = parser.parse_args()

    tracker = os.path.join(args.study_dir, "experiment_tracker.xlsx")
    group_dir = os.path.join(args.study_dir, "group")

    rows: list[dict] = []
    collect_from_session_notes(rows, tracker)
    collect_from_db_summary(rows, tracker)
    collect_from_redcap_csvs(rows, group_dir)

    rows.sort(key=lambda r: (r["group_session"], r["source_type"], r["provenance"]))

    with open(args.output, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(
            f, fieldnames=["group_session", "raw_notes", "provenance", "source_type"]
        )
        writer.writeheader()
        writer.writerows(rows)

    print(f"Wrote {len(rows)} rows to {args.output}")


if __name__ == "__main__":
    main()
