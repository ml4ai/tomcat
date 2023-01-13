# File Checker

## Description

The `fileChecker` executable monitors a specific directory and outputs the time of creation of any file that is created or added within that directory. This output can be in the form of a print statement via stdout, in a file, or a message published in a message bus via mqtt.

## Usage

To build the project, navigate to root project folder and run:
```bash
cmake -S . -B build
cd build
make
```

Navigate to the `build/` directory in the fileChecker root directory and execute:

```bash
./fileChecker
```

This will start monitoring the default path specified in the program and print out the timestamps of any files created in that particular directory. The output by default will be through stdout.

#### Command Line Arguments
```
-h [ --help ]   Show this help message
--path          To set the path to be monitored by inotify
--out           Specify the output format (0 for stdout (default), 1 for file, 2 for mqtt)
```

#### Example Usage

If you want to monitor the path `/home/user/screenshots` and have the output in the form of a file, navigate to the `build/` directory and execute the following command on the command line:

```bash
./fileChecker --path /home/user/screenshots --out 1
```

Similarly, you can have other combinations

## To rename images to their respective timestamps

Navigate to the `scripts/` folder to run the `timestampRename` program

```bash
./timestampRename.py -d /home/user/screenshots
```

