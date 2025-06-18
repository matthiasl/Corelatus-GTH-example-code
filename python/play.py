#!/usr/bin/python3
#
# Title: Play a file to an E1/T1 timeslot using a Corelatus GTH
# Author: Matthias Lang (matthias@corelatus.se)

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

# Check that a given PCM is in a state where it could give useful data.
# That means in 'OK' or 'RAI' status.
def warn_if_l1_dead(api, span):
    attrs = api.query_resource("pcm" + span)
    if attrs['status'] in ["OK", "RAI"]:
        return
    else:
        if attrs['status'] == "disabled":
            stderr.write("""
Warning: pcm%s is disabled. Your recording won't have any useful data in it.
         Hint: enable L1 with 'gth.py'
""" % span)
        else:
            stderr.write("""
Warning: pcm%s status is %s

Your recording won't have any useful data in it. Is there really a signal
 on pcm%s?
""" % (span, attrs['status'], span))

def play(host, span, timeslot, file):
    api = gth.apilib.API(host)
    warn_if_l1_dead(api, span)

    player_id, data = api.new_player(span, timeslot)
    octets_sent = 0

    # Stream out the message
    while True:
        buffer = file.read(1000)
        octets_sent += len(buffer)
        if (buffer == ""):
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
        file = open(argv[4], "r")

    play(argv[1], argv[2], int(argv[3]), file)


main()
