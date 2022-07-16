#!/usr/bin/env python3

import argparse
import subprocess
import sys
import json
import importlib

# Authors:  Joseph Astier, Adarsh Pyarelal

# Check metadata files in first level of target directories

def test_file(filename, args):

    test_module = __import__(f'{args.test_module}',fromlist=[''])

    return {
      'file':filename,
      'results': test_module.test_metadata_file(filename)
    }


# Test a directory of files, one level deep
def test_dataset(dataset, args):

    d = {
        'dataset':dataset,
        'tests': []
    }

    proc = subprocess.run(['ls', dataset], capture_output=True)
    if not proc.returncode == 0:
        print(f'Problem reading from directory {dataset}')
        print(proc.stderr.decode('utf-8'))
        return d

    output = proc.stdout.decode('utf-8')
    lines = output.split('\n')
    filenames = filter(lambda x: x.endswith('.metadata'), lines)
    filepaths = map(lambda x: f'{dataset}/{x}', filenames)
    results = map(lambda x: test_file(x, args), filepaths)
    d['results'] = list(results)
    return d

def test_datasets(datasets, args):
    tests = map(lambda x: test_dataset(x, args), datasets)
    results = filter(lambda x: len(x['results']) > 0, tests)
    return list(results)

def report_results(report, args):
    print(json.dumps(report, indent=4, sort_keys=True))

    if args.output:
        with open(args.output, "w") as output:
            output.write(json.dumps(report)+"\n")


# download metadata studies and test files
if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument(
        'test_module', 
        action='store', 
        help = (
            'Name of test module.   It must contain a method called '
            'test_metadata_file that takes a filename argument. '
        )
    ) 
    parser.add_argument(
        'datasets', 
        action='store', 
        nargs = '+',
        help = 'One or more dataset names, e.g. \"TM000nnn\"'
    ) 
    parser.add_argument(
        '-o',
        '--output',
        help = 'Write the test results to a JSON file'
    )

    args = parser.parse_args(sys.argv[1:])

    # Test all the Testbed datasets specified by the user
    test_results = test_datasets(args.datasets, args)
    report_results(test_results, args)
