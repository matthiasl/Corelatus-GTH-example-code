#!/usr/bin/python
#
# Title: Record E1/T1 timeslot data from a Corelatus GTH 
# Author: Matthias Lang (matthias@corelatus.se)

import sys
from sys import argv, stderr
import gth.apilib

import socket

def usage():
    stderr.write("""
record <hostname> <span> <timeslot> <octets> <filename>

  <hostname>: the hostname or IP address of a GTH
      <span>: the name of an E1/T1, e.g. 1A or 4D on a GTH 2.x
  <timeslot>: 1--31 on an E1 or 1--24 on a T1
    <octets>: how many octets to record. There are 8000 per second
  <filename>: which file to write the data to. - means stdout

Typical invocation: ./record 172.16.1.10 1A 16 16000 signalling.raw
""")

# Check that a given PCM is in a state where it could give useful data.
# That means in 'OK' or 'RAI' status.
def warn_if_l1_dead(api, span):
    api.send("<query><resource name='pcm" + span + "'/></query>")
    answer = api.next_non_event()
    attributes = answer[0][3]

    if attributes['status'] == "OK":
        pass
    elif attributes['status'] == "RAI":
        pass
    elif attributes['status'] == "disabled":
        stderr.write("""
Warning: pcm%s is disabled. Your recording won't have any useful data in it.
         Hint: enable L1 with enable_l1.py
""" % span)
    else:
        stderr.write("""
Warning: pcm%s status is %s

Your recording won't have any useful data in it. Is there really a signal
 on pcm%s?
""" % (span, attributes['status'], span))

def record(host, span, timeslot, octets_wanted, file):
    api = gth.apilib.API(host)
    warn_if_l1_dead(api, span)

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
        file = open(argv[5], "w")

    record(argv[1], argv[2], int(argv[3]), int(argv[4]), file)


main()
