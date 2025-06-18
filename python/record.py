#!/usr/bin/python3
#
# Title: Record E1/T1 timeslot data from a Corelatus GTH
#
# Copyright (c) 2020–2025, Corelatus AB
# All rights reserved.
#
# Licensed under the BSD 3-Clause License. See the LICENSE file
# in the project root for full license information.
#

import sys
from sys import argv, stderr
import gth.apilib

import socket

def usage():
    stderr.write("""
record.py <hostname> <span> <timeslot> <octets> <filename>

  <hostname>: the hostname or IP address of a GTH
      <span>: the name of an E1/T1, e.g. 1A or 4D on a GTH 2.x
  <timeslot>: 1--31 on an E1 or 1--24 on a T1
    <octets>: how many octets to record. There are 8000 per second
  <filename>: which file to write the data to. - means stdout

Typical invocation: ./record.py 172.16.1.10 1A 16 16000 signalling.raw
""")

def record(host, span, timeslot, octets_wanted, file):
    api = gth.apilib.API(host)
    api.warn_if_l1_dead(span, "Your recording file won't have useful data.")

    recorder_id, data = api.new_recorder(span, timeslot)
    octets_received = 0

    while octets_received < octets_wanted:
        buffer = data.recv(octets_wanted - octets_received)
        if len(buffer) == 0:
            raise Exception("unexpected eof on data socket")
        file.write(buffer)
        octets_received += len(buffer)

    if (file != sys.stdout):
        file.close()
    stderr.write("All done, wrote %d octets\n" % octets_received)

    api.delete(recorder_id)
    data.close()

    api.bye()

def main():
    if len(sys.argv) != 6:
        usage()
        sys.exit(-1)

    # We only check the number of arguments. If the GTH doesn't like
    # the contents, it'll say so.

    if (argv[5] == "-"):
        file = sys.stdout
    else:
        file = open(argv[5], "wb")

    record(argv[1], argv[2], int(argv[3]), int(argv[4]), file)


main()
