#!/usr/bin/env python3

"""Play a file to an E1/T1 timeslot using a Corelatus GTH
"""

# Copyright (c) 2020–2025, Corelatus AB
# All rights reserved.
#
# Licensed under the BSD 3-Clause License. See the LICENSE file
# in the project root for full license information.

from sys import argv, stdin, stderr
import sys
import gth.apilib

def _usage():
    stderr.write("""
play <hostname> <span> <timeslot> <filename>

  <hostname>: the hostname or IP address of a GTH
      <span>: the name of an E1/T1, e.g. 1A or 4D on a GTH 2.x
  <timeslot>: 1--31 on an E1 or 1--24 on a T1
  <filename>: which file to read the data from. - means stdin

Typical invocation: ./play 172.16.1.10 1A 16 signalling.raw
""")

def _play(host, span, timeslot, file):
    api = gth.apilib.API(host)
    api.warn_if_l1_dead(span)

    _player_id, data = api.new_player(span, timeslot)
    octets_sent = 0

    # Stream out the message
    while True:
        buffer = file.read(1000)
        if buffer == b"":
            break
        octets_sent += len(buffer)
        data.sendall(buffer)

    data.close()

    # Wait for the message to finish playing. A timeslot on an E1/T1
    # consumes 8000 bytes per second. We make the timeout a bit longer
    # to make sure we see the event.
    expected_play_time = octets_sent / 8000
    while api.next_event(expected_play_time + 0.5)[1] != 'message_ended':
        pass

    api.bye()

def main():
    """entry point"""
    if len(argv) != 5:
        _usage()
        sys.exit(1)

    # We only check the number of arguments. If the GTH doesn't like
    # the contents, it'll say so.

    if argv[4] == "-":
        _play(argv[1], argv[2], int(argv[3]), stdin.buffer)
    else:
        with open(argv[4], "rb") as f:
            _play(argv[1], argv[2], int(argv[3]), f)

main()
