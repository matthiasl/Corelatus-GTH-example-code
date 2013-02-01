#!/usr/bin/python
#
# Title: Record an entire E1/T1 from a Corelatus SDH/SONET Monitoring Probe
#
# This program relies on an experimental feature (as at 2013-01)
#
# Author: Matthias Lang (matthias@corelatus.se)

import sys
from sys import argv, stderr
import gth.apilib

import socket
import struct

def usage():
    stderr.write("""
wide_recorder.py <hostname> <span> <filename>

  <hostname>: the hostname or IP address of a GTH
      <span>: the name of an E1/T1, e.g. 1A or 4D on a GTH 2.x
  <filename>: which file to write the data to. - means stdout

Typical invocation: ./wide_recorder.py 172.16.1.10 1A /tmp/capture.raw
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

def record(host, span, file):
    api = gth.apilib.API(host)
    warn_if_l1_dead(api, span)

    recorder_id, data = api.new_wide_recorder(span)

    while True:
        buffer = data.recv(8000)
        if len(buffer) < 100:
            raise Exception("unexpected eof/short data on socket")
        _tag, _res, seq, _ts_hi, _ts_lo = struct.unpack("!hhhhi", buffer[0:12])
        file.write(buffer[12:])
        if seq % 100 == 0:
            print "seq: ", seq

def main():
    if len(sys.argv) != 4:
        usage()
        sys.exit(-1)

    # We only check the number of arguments. If the GTH doesn't like
    # the contents, it'll say so.

    if (argv[3] == "-"):
        file = sys.stdout
    else:
        file = open(argv[3], "w")

    record(argv[1], argv[2], file)

main()
