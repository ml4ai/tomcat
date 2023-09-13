import os
import sys
import logging

USER = os.getenv("USER")
# os.makedirs(f"/space/{USER}/tomcat", exist_ok=True)

EXP_DIR = "/tomcat/data/raw/LangLab/experiments/study_3_pilot/group"
OUT_DIR = f"/space/{USER}/tomcat/preprocessed"
