#!/usr/bin/env python
#
# Author: Joseph Astier
# Date: 2020 October
#
# job manager for command line tasks.
# Feel free to add new jobs
#

import os

# Jobs to perform.  If it runs on the command line, it will run here.
jobs = (
    './single_port_forwarder.py -d laplace -p 64738 start',
)


# used for reporting results
passStr = ' OK '
failStr = 'FAIL'

# run one job at a time
def run_job(t):
    print('JOB:  '+t)
    retval = (failStr, passStr)[(os.system(t)== 0)]
    print('RESULT: ' + retval + '\n')
    return (retval, t)

# run all the jobs.
results = map(run_job, jobs)
run = len(results)
ok = sum(map(lambda (a,b): (0,1)[a==passStr], results))
failed = run-ok

# report the results in a concise way
print('Summary: ')
for g in results:
    print(g[0] + ' ' + g[1])
print('Jobs run : ' + str(run))
print('OK       : ' + str(ok))
print('Failed   : ' + str(failed))
if(failed == 0):
    print('All jobs were run successfully.')
