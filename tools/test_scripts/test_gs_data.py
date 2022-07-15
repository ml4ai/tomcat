#!/usr/bin/env python3

import argparse
import subprocess
import sys
import json
import ac_tamu_ta1_dialog_act_classifier

# Authors:  Joseph Astier, Adarsh Pyarelal

# Check metadata files in target directories.  No recursion.

def test_file(dataset, filename):
    test = {'file':filename}

    filepath = f'{dataset}/{filename}'

    print(f'Testing {filepath}')

    results = ac_tamu_ta1_dialog_act_classifier.test_metadata_file(filepath)
    test['results'] = results
    return test

def get_filenames(datasets):
    results = []
    for dataset in datasets:
        proc = subprocess.run(['ls', dataset], capture_output=True)
        if proc.returncode == 0:
            result = {'name':dataset}
            tests = []
            output = proc.stdout.decode('utf-8')
            output_lines = output.split('\n')
            for line in output_lines:
                if line.endswith('.metadata'):
                    tests.append(test_file(dataset, line))
            result['tests']=tests
            results.append(result)

        else:
            print(f'Problem reading from directory {dataset}')
            print(proc.stderr.decode('utf-8'))

    return results

def report_results(report, args):
    print(json.dumps(report, indent=4, sort_keys=True))

    if args.output:
        with open(args.output, "w") as output:
            output.write(json.dumps(report)+"\n")


# download metadata studies and test files
if __name__ == '__main__':

    parser = argparse.ArgumentParser()

    parser.add_argument(
        'datasets', 
        action='store', 
        nargs = '+',
        help = 'One or more dataset names, e.g. \"TM000nnn\"'
    ) 

    # Optionally write the test results to a JSON file
    parser.add_argument(
        '-o',
        '--output',
        help = 'Output filename'
    )

    args = parser.parse_args(sys.argv[1:])

    # Test all the Testbed datasets specified by the user
    test_results = get_filenames(args.datasets)

    report_results(test_results, args)
