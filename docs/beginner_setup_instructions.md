# Setup instructions for people new to the command line.

This is a set of instructions for macOS users who are new to the command line.

## Launching the terminal

macOS comes with a built-in terminal application, named 'Terminal'. You
should be able to search for it using Spotlight Search (which you can bring up
with `<Cmd>+<Space>`).

## Install XCode, Command Line Tools and MacPorts

Follow the instructions here: https://www.macports.org/install.php to install
XCode, the macOS command line tools, and MacPorts, which is a software package
manager for macOS.

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
