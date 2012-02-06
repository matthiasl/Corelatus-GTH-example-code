#!/usr/bin/python
#
# Title: Sniff SS7 ISUP call setup/teardown from an E1/T1 timeslot
# Author: Matthias Lang (matthias@corelatus.se)
#
# Reference: ISUP decoding is in ITU-T Q.767 Annex C
#            MTP-2 is in ITU-T Q.703
#            MTP-3 is in ITU-T Q.704

import sys
from sys import argv, stderr
import gth.apilib

import socket
import struct

def usage():
    stderr.write("""
sniff_isup.py <hostname> <span> <timeslot>

  <hostname>: the hostname or IP address of a GTH
      <span>: the name of an E1/T1, e.g. 1A or 4D on a GTH 2.x
  <timeslot>: 1--31 on an E1 or 1--24 on a T1

Typical invocation: ./sniff_isup.py 172.16.1.10 1A 16
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
Warning: pcm%s is disabled. The GTH won't actually emit any data.
         Hint: enable L1 with enable_l1.py
""" % span)
    else:
        stderr.write("""
Warning: pcm%s status is %s

Chances are the other end of your E1 isn't plugged in (or enabled)

""" % (span, attributes['status']))

def _definite_read(socket, n):
    data = socket.recv(n)
    if (len(data) != n):
        data += _definite_read(socket, n - len(data))

    return data

def decode_mtp2(packet):
    # The first 10 octets is the GTH header, which we can ignore
    #     next 3 octets is MTP-2 FSN, BSN and LI, which we can ignore
    #     next 1 octet SIO and at least 4 octets of SIF
    #     finally 2 octets of CRC
    #
    # So ignore anything shorter than 20 octets
    if len(packet) < 20:
        return

    sio = ord(packet[13])
    sif = packet[14:-2]
    decode_mtp3(sio, sif)

# Q.704 14.2.1 and 14.2.2 defines the service codes.
def decode_mtp3(sio, sif):
    if (sio & 0x0f) == 5:     # Mask out the service indicator
        decode_isup(sif[4:])  # just skip the routing label

def decode_isup(sif):
    (CIC, type) = struct.unpack("<HB", sif[0:3])
    rest = sif[3:]
    action = {
        0x01: isup_iam,
        0x10: isup_rlc
        }
    action.get(type, isup_ignore)(type, CIC, rest)

def isup_iam(_, CIC, sif):
    # First 5 octets can be ignored
    bnum_pointer = ord(sif[5])
    anum_pointer = ord(sif[6])
    bnum = sif[5 + bnum_pointer:]
    anum = sif[7 + anum_pointer:]
    print "IAM called party: %s calling party: %s CIC=%d" \
        % (isup_number(bnum), isup_number(anum), CIC)

# Decode an ISUP number, as per C 3.7
def isup_number(num):
    length = ord(num[0]) - 2
    is_even = ((ord(num[1]) & 0x80) == 0)
    # num[2] is just the numbering plan, ignore that
    list = []
    index = 3
    while length > 0:
        if length == 1 and not is_even:
            list += [ord(num[index])]
        else:
            list += [ord(num[index]) & 0x0f, ord(num[index]) >> 4]
        index += 1
        length -= 1
    return "".join(map(str, list))

def isup_rlc(_, CIC, sif):
    print "RLC on CIC=%d" % CIC

def isup_ignore(type, CIC, rest):
    types = {
        0x02: "subsequent address",
        0x06: "address complete",
        0x09: "answer",
        0x0c: "release",
        0x2c: "call progress"
        }
    pretty_type = types.get(type, "message type %d" % type )
    print "ignoring ISUP %s" % pretty_type

def monitor_mtp2(host, span, timeslot):
    api = gth.apilib.API(host)
    warn_if_l1_dead(api, span)

    mtp2_id, data = api.new_mtp2_monitor(span, timeslot)

    while True:
        b = _definite_read(data, 2)
        length = ord(b[0]) * 256 + ord(b[1])
        packet = _definite_read(data, length)
        decode_mtp2(packet)

    api.delete(mtp2_id)
    data.close()

    api.bye()

def main():
    if len(sys.argv) != 4:
        usage()
        sys.exit(-1)

    monitor_mtp2(argv[1], argv[2], int(argv[3]))

main()
