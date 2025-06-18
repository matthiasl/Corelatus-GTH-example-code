#!/usr/bin/python3
#
# Title: Play a file to an E1/T1 timeslot using a Corelatus GTH
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
import time

import socket

def usage():
    stderr.write("""
play <hostname> <span> <timeslot> <filename>

  <hostname>: the hostname or IP address of a GTH
      <span>: the name of an E1/T1, e.g. 1A or 4D on a GTH 2.x
  <timeslot>: 1--31 on an E1 or 1--24 on a T1
  <filename>: which file to write the data to. - means stdin

Typical invocation: ./play 172.16.1.10 1A 16 signalling.raw
""")

def play(host, span, timeslot, file):
    api = gth.apilib.API(host)
    api.warn_if_l1_dead(span)

    player_id, data = api.new_player(span, timeslot)
    octets_sent = 0

    # Stream out the message
    while True:
        buffer = file.read(1000)
        octets_sent += len(buffer)
        if (len(buffer) == 0):
            break
        data.sendall(buffer)

    data.close()
    stderr.write("All done, sent %d octets\n" % octets_sent)

    while api.next_event()[1] != 'message_ended':
        void

    api.bye()

def main():
    if len(sys.argv) != 5:
        usage()
        sys.exit(-1)

    # We only check the number of arguments. If the GTH doesn't like
    # the contents, it'll say so.

    if (argv[4] == "-"):
        file = sys.stdin
    else:
        file = open(argv[4], "rb")

    play(argv[1], argv[2], int(argv[3]), file)


main()
