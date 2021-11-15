#!/usr/bin/env python

import os
from configparser import ConfigParser
from flask import Flask, render_template

config = ConfigParser()
config.read("config.ini")

# We check if the WEBMIC_WS_URL environment variable is set. If so, we use
# that, and otherwise we fall back to the value defined in the config.ini file.
ws_url = os.getenv("WEBMIC_WS_URL", config["websockets"]["ws_url"])

app = Flask(__name__)

@app.route("/")
def main():
    return render_template("task1_intro.html", ws_url=ws_url)

@app.route("/task1_intro.html")
def first_page():
    return render_template("task1_intro.html" ,ws_url = ws_url)

@app.route("/task1_page.html")
def second_page():
    return render_template("task1_page.html",ws_url=ws_url)

@app.route("/task2_intro.html")
def third_page():
    return render_template("task2_intro.html",ws_url=ws_url)

@app.route("/task2_page.html")
def fourth_page():
    return render_template("task2_page.html",ws_url=ws_url)

@app.route("/end_page.html")
def fifth_page():
    return render_template("end_page.html",ws_url=ws_url)





if __name__ == "__main__":
    app.run(host=config["flask"]["host"], port=config["flask"]["port"])
