import os

USER = os.getenv("USER")

os.makedirs(f"/space/{USER}/tomcat", exist_ok=True)
SQLITE_DB_PATH = f"/space/{USER}/tomcat/tomcat.db"

IMAGE_URL_ROOT_DIR = "https://ivilab.cs.arizona.edu/data/tomcat/group"
