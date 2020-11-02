#!/usr/bin/env python

""" A helper script to automate port forwarding. """

import argparse
import subprocess as sp

parser = argparse.ArgumentParser(
    description="Helper script for port forwarding."
)

# Add target server, this can be either a name or an IPV4 IP address.
parser.add_argument('-s', type=str, help='Remote server name or IP address')

# Add port to be forwarded, integer required
parser.add_argument('-p', type=int, help='Port number to forward (will be same on local and remote machine)')

# Start forwarding this port
parser.add_argument(
    '--start', action='store_true', help='Start forwarding this port'
)

# Stop forwarding this port....is that even possible?
parser.add_argument(
    '--stop', action='store_true', help='Stop forwarding this port'
)

# Query the status of this port, eg. is it already bound?
parser.add_argument('--status', action='store_true', 
    help='Query the port status, eg. is it already bound?'
)


args = parser.parse_args()


print(args)

