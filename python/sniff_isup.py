#!/usr/bin/env python3

"""
Sniff SS7 ISUP call setup/teardown from an E1/T1 timeslot

Reference: ISUP decoding is in ITU-T Q.767 Annex C
            MTP-2 is in ITU-T Q.703
            MTP-3 is in ITU-T Q.704
"""

# Copyright (c) 2020–2025, Corelatus AB
# All rights reserved.
#
# Licensed under the BSD 3-Clause License. See the LICENSE file
# in the project root for full license information.

import sys
import os
import struct
from sys import argv, stderr
import gth.apilib

def _usage():
    stderr.write("""
sniff_isup.py <hostname> <span> <timeslot>

  <hostname>: the hostname or IP address of a GTH
      <span>: the name of an E1/T1, e.g. 1A or 4D on a GTH 2.x
  <timeslot>: 1--31 on an E1 or 1--24 on a T1

Typical invocation: ./sniff_isup.py 172.16.1.10 1A 16
""")

def _definite_read(socket, n):
    data = socket.recv(n)
    if len(data) != n:
        data += _definite_read(socket, n - len(data))

    return data

def _decode_mtp2(packet):
    # The first 10 octets is the GTH header, which we can ignore
    #     next 3 octets is MTP-2 FSN, BSN and LI, which we can ignore
    #     next 1 octet SIO and at least 4 octets of SIF
    #     finally 2 octets of CRC
    #
    # So ignore anything shorter than 20 octets
    if len(packet) < 20:
        return

    sio = packet[13]
    sif = packet[14:-2]
    _decode_mtp3(sio, sif)

# Q.704 14.2.1 and 14.2.2 defines the service codes.
def _decode_mtp3(sio, sif):
    if (sio & 0x0f) == 5:     # Mask out the service indicator
        _decode_isup(sif[4:])  # just skip the routing label

def _decode_isup(sif):
    (cic, p_type) = struct.unpack("<HB", sif[0:3])
    rest = sif[3:]
    action = {
        0x01: _isup_iam,
        0x10: _isup_rlc
        }
    action.get(p_type, _isup_ignore)(p_type, cic, rest)

# A-number may or may not be present. Cycle through all optional parameters
# to try to find it.
def _find_caller_number(sif, pointer):
    while pointer != 0 and sif[pointer] != 0:
        typecode = sif[pointer]
        length = sif[pointer + 1]
        if typecode == 0x0a:
            return _isup_number(sif[pointer + 1:])
        pointer += length + 2

    return "unknown"

def _isup_iam(_, cic, sif):
    # First 5 octets can be ignored
    bnum_pointer = sif[5]
    optional_params_pointer = sif[6]
    bnum = sif[5 + bnum_pointer:]
    anum = _find_caller_number(sif[6:], optional_params_pointer)
    print(f"IAM called party: {_isup_number(bnum)} calling party: {anum} CIC={cic}")

# Decode an ISUP number, as per C 3.7
def _isup_number(num):
    length = num[0] - 2
    is_even = (num[1] & 0x80) == 0
    # num[2] is just the numbering plan, ignore that
    l = []
    index = 3
    while length > 0:
        if length == 1 and not is_even:
            l += [num[index]]
        else:
            l += [num[index] & 0x0f, num[index] >> 4]
        index += 1
        length -= 1
    return "".join(map(str, l))

def _isup_rlc(_, cic, _sif):
    print(f"RLC on CIC={cic}")

def _isup_ignore(p_type, _cic, _rest):
    types = {
        0x02: "subsequent address",
        0x06: "address complete",
        0x09: "answer",
        0x0c: "release",
        0x2c: "call progress"
        }
    pretty_type = types.get(type, f"message type {p_type}" )
    print( f"ignoring ISUP {pretty_type}")

def _monitor_mtp2(host, span, timeslot):
    api = gth.apilib.API(host)
    api.warn_if_l1_dead(span)

    mtp2_id, data = api.new_mtp2_monitor(span, timeslot)

    while True:
        b = _definite_read(data, 2)
        length = b[0] * 256 + b[1]
        packet = _definite_read(data, length)
        _decode_mtp2(packet)

    api.delete(mtp2_id)
    data.close()

    api.bye()

def main():
    """entry point"""

    if len(sys.argv) != 4:
        _usage()
        sys.exit(-1)

    sys.stdout = os.fdopen(sys.stdout.fileno(), "w", buffering=1)

    _monitor_mtp2(argv[1], argv[2], int(argv[3]))

main()
