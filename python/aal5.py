#!/usr/bin/env python3

"""Demonstrate how to start ATM AAL5 on a Corelatus GTH.

   This doesn't actually process any AAL5 data, it just shows how
   to start the job.

"""

# Copyright (c) 2020–2025, Corelatus AB
# All rights reserved.
#
# Licensed under the BSD 3-Clause License. See the LICENSE file
# in the project root for full license information.

import sys
from sys import argv, stderr
import gth.apilib

def _usage():
    stderr.write("""
aal5.py <hostname> <span> <n_timeslots> <vpi> <vci>

    <hostname>: the hostname or IP address of a GTH
        <span>: the name of an E1/T1, e.g. 1A or 4D on a GTH 2.x
 <n_timeslots>: either 30 or 31

 Typical invocation: ./aal5.py 172.16.1.10 1A 30 0 5
""")

def _monitor_aal5(host, span, n_timeslots, vpi_vci):
    api = gth.apilib.API(host, 3)
    api.warn_if_l1_dead(span)

    if n_timeslots == 30:
        timeslots = list(range(1,15)) + list(range(17,31))
    elif n_timeslots == 31:
        timeslots = list(range(1,31))
    else:
        timeslots = [] # silence the linter; variable is not used
        _die("can only do 30 or 31 timeslot wide channels")

    aal5_id, data = api.new_atm_aal5_monitor(span, timeslots, vpi_vci)

    api.delete(aal5_id)
    data.close()

    api.bye()

def main():
    """entry point"""
    if len(sys.argv) != 6:
        _usage()
        sys.exit(-1)

    vpi_vci = (int(argv[4]), int(argv[5]))
    _monitor_aal5(argv[1], argv[2], int(argv[3]), vpi_vci)

def _die(why):
    print(why)
    sys.exit(-1)

main()
