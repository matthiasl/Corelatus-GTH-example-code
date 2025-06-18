#!/usr/bin/python3
#
# Title: Demonstrate how to transmit SS7 on 64 kbit/s MTP-2,
#        1536 kbit/s MTP-2 Annex A and 1536 kbit/s ATM AAL0.
#
# Author: Matthias Lang (matthias@corelatus.se)
#
# MTP-2 Underlying operation
# --------------------------
#
# The Corelatus hardware takes care of
#
#    1. appending the FCS (CRC).
#    2. packet delimitation (flags).
#    3. bit stuffing.
#    4. automatically repeating FISUs/LSSUs.
#
# The first three are the same as for Frame Relay, so we do this using
# an <fr_layer> and using opcode 3 when transmitting signal units.
#
# Reference: API manual, 3.16 <new_fr_layer>
#
#
# ATM AAL0 Underlying operation
# -----------------------------
#
# The Corelatus hardware takes care of
#
#    1. HEC (CRC) generation
#    2. Scrambling
#    3. Idle cell transmission
#
# Reference: API manual, 3.9 <new atm_aal0_layer>

import sys
import time
from sys import argv, stderr
import gth.apilib

import socket
import struct

def usage():
    stderr.write("""
transmit_ss7.py <hostname> <span>

  <hostname>: the hostname or IP address of a GTH
      <span>: the name of an E1/T1, e.g. 1A or 4D on a GTH 3.0

Typical invocation: ./transmit_ss7.py 172.16.1.10 1A
""")

def fisu(fsn, fib, bsn, bib):
    li = 0
    fsn_fib = ((fsn & 0x7F) << 1) | (fib & 0x01)
    bsn_bib = ((bsn & 0x7F) << 1) | (bib & 0x01)

    return bytes([bsn_bib, fsn_fib, li])

# common sf values:  (Q.703: 11.1.3, 7.2)
#    0: SIO
#    1: SIN
def lssu(fsn, fib, bsn, bib, sf):

    li = 1
    fsn_fib = ((fsn & 0x7F) << 1) | (fib & 0x01)
    bsn_bib = ((bsn & 0x7F) << 1) | (bib & 0x01)

    return bytes([bsn_bib, fsn_fib, li, sf])

def msu(fsn, fib, bsn, bib, sio, sif):
    if not isinstance(sif, bytes) or len(sif) > 63:
        raise TypeError("invalid SIF")

    li = len(sif) + 1     # +1 for SIO

    fsn_fib = ((fsn & 0x7F) << 1) | (fib & 0x01)
    bsn_bib = ((bsn & 0x7F) << 1) | (bib & 0x01)

    return bytes([bsn_bib, fsn_fib, li, sio]) + sif

def mtp2_send(socket, su):
    length = len(su) + 4   # 16 bit opcode, 16 bit reserved
    opcode = 3
    packed = struct.pack('!HHH', length, opcode, 0) + su
    result = socket.sendall(packed)

def transmit_mtp2(host, span, timeslots):
    api = gth.apilib.API(host, 0)    # 0=quiet, 3=verbose debugging
    api.warn_if_l1_dead(span)

    fr_id, data = api.new_fr_layer(span, timeslots)

    mtp2_send(data, lssu(0, 0, 0, 0, 0))   # SIO
    mtp2_send(data, lssu(0, 0, 0, 0, 1))   # SIN
    mtp2_send(data,  msu(0, 0, 0, 0, 1, b'Corelatus Stockholm'))
    mtp2_send(data, fisu(0, 0, 1, 0))

    time.sleep(2)

    api.delete(fr_id)
    data.close()

    api.bye()

def aal0_send(socket, su):
    length = 52
    opcode = 3
    packed = struct.pack('!H', length) + su
    result = socket.sendall(packed)

def transmit_aal0(host, span, timeslots):
    api = gth.apilib.API(host, 0)    # 0=quiet, 3=verbose debugging
    api.warn_if_l1_dead(span)

    # On E1, scrambling is used
    # On T1, scrambling is not used
    # (ref: ITU-T I.432.3 6.2.4.5 and 7.2.4.5)
    l1_settings = api.query_resource("pcm" + span)
    if (l1_settings['status'] != 'disabled' and l1_settings['mode'] == "E1"):
        scrambling = "yes"
    else:
        scrambling = "no"

    aal0_id, data = api.new_atm_aal0_layer(span, timeslots, \
                                           {'scrambling': scrambling})

    payload = b'Corelatus AB Stockholm this is exactly 48 octets'
    aal0_send(data, bytes([1,2,3,4]) + payload)

    time.sleep(2)

    api.delete(aal0_id)
    data.close()

    api.bye()


def main():
    if len(sys.argv) != 3:
        usage()
        sys.exit(-1)

    # 64 kbit/s MTP-2
    transmit_mtp2(argv[1], argv[2], [1])

    # 1536 kbit/s MTP-2 (i.e. Q.703 Annex A, high speed)
    transmit_mtp2(argv[1], argv[2], list(range(1, 25)))

    # 1536 kbit/s ATM AAL0
    transmit_aal0(argv[1], argv[2], list(range(1, 25)))

main()
