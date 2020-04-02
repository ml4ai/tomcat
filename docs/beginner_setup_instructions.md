# Setup instructions for people new to the command line.

This is a set of instructions for macOS users who are new to the command line.

## Launching the terminal

macOS comes with a built-in terminal application, named 'Terminal'. You
should be able to search for it using Spotlight Search (which you can bring up
with `<Cmd>+<Space>`).

## Installing ToMCAT

Open a new terminal window. By default, the starting directory is your home
directory. If you would like to download and install ToMCAT in another
directory, change the working directory to that directory using the 'cd' (short
for 'change directory') command. If the directory does not exist, you can create
it using the 'mkdir' (short for 'make directory') command.

For example, if you wanted to create a new directory called 'Research', and
change the working directory to it, you would enter the two following commands
(to execute a command that has been typed into the terminal, press the `<Return>`
key - in the code chunk below, each line break corresponds to hitting the
`<Return>` key.)

    mkdir Research
    cd Research

Once you are in your desired working directory, run the following commands in
the terminal:

    git clone https://github.com/ml4ai/tomcat
    cd tomcat && ./tools/install.sh

You may be presented with a prompt to input a password during the installation,
in which case enter the password you use to log in to your account (the
password you use to install any software on your computer). You will not see
any letters appearing as you type in your password - they will be hidden for
privacy - so it is fine if it appears blank. Just type in your password
normally and press the enter/return key on your keyboard.

When you run the `run_session.sh` script, you might be asked to give the
terminal permissions to control system events - this is so that the script can
automatically make the Minecraft window full screen. Additionally, you might be
asked for permissions to use the microphone and webcam - this is for recording
audio and video of the player when the mission is running.
