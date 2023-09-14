import os

USER = os.getenv("USER")

os.makedirs(f"/space/{USER}/tomcat", exist_ok=True)
SQLITE_DB_PATH = f"/space/{USER}/tomcat/tomcat.db"
