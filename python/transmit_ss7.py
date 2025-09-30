#!/usr/bin/python3
"""
Demonstrate how to transmit SS7 on 64 kbit/s MTP-2,
1536 kbit/s MTP-2 Annex A and 1536 kbit/s ATM AAL0.
"""

# Copyright (c) 2020–2025, Corelatus AB
# All rights reserved.
#
# Licensed under the BSD 3-Clause License. See the LICENSE file
# in the project root for full license information.
#
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

import struct

import gth.apilib

def _usage():
    stderr.write("""
transmit_ss7.py <hostname> <span>

  <hostname>: the hostname or IP address of a GTH
      <span>: the name of an E1/T1, e.g. 1A or 4D on a GTH 3.0

Typical invocation: ./transmit_ss7.py 172.16.1.10 1A
""")

def _fisu(fsn, fib, bsn, bib):
    li = 0
    fsn_fib = ((fsn & 0x7F) << 1) | (fib & 0x01)
    bsn_bib = ((bsn & 0x7F) << 1) | (bib & 0x01)

    return bytes([bsn_bib, fsn_fib, li])

# common sf values:  (Q.703: 11.1.3, 7.2)
#    0: SIO
#    1: SIN
def _lssu(fsn, fib, bsn, bib, sf):

    li = 1
    fsn_fib = ((fsn & 0x7F) << 1) | (fib & 0x01)
    bsn_bib = ((bsn & 0x7F) << 1) | (bib & 0x01)

    return bytes([bsn_bib, fsn_fib, li, sf])

def _msu(fsn, fib, bsn, bib, sio, sif):
    if not isinstance(sif, bytes) or len(sif) > 63:
        raise TypeError("invalid SIF")

    li = len(sif) + 1     # +1 for SIO

    fsn_fib = ((fsn & 0x7F) << 1) | (fib & 0x01)
    bsn_bib = ((bsn & 0x7F) << 1) | (bib & 0x01)

    return bytes([bsn_bib, fsn_fib, li, sio]) + sif

def _mtp2_send(sock, su):
    length = len(su) + 4   # 16 bit opcode, 16 bit reserved
    opcode = 3
    packed = struct.pack('!HHH', length, opcode, 0) + su
    _result = sock.sendall(packed)

def _transmit_mtp2(host, span, timeslots):
    api = gth.apilib.API(host, 0)    # 0=quiet, 3=verbose debugging
    api.warn_if_l1_dead(span)

    fr_id, data = api.new_fr_layer(span, timeslots)

    _mtp2_send(data, _lssu(0, 0, 0, 0, 0))   # SIO
    _mtp2_send(data, _lssu(0, 0, 0, 0, 1))   # SIN
    _mtp2_send(data,  _msu(0, 0, 0, 0, 1, b'Corelatus Stockholm'))
    mtp3 = [2,64,0,144,14,0,1,17,0,0,10,3,2,9,7,3,
            144,64,56,9,130,153,10,6,3,19,23,115,69,8,0,121,137]
    _mtp2_send(data,  _msu(1, 0, 0, 0, 85, bytes(mtp3)))  # 85 = National ISUP
    _mtp2_send(data, _fisu(1, 0, 1, 0))

    time.sleep(2)

    api.delete(fr_id)
    data.close()

    api.bye()

def _aal0_send(sock, su):
    length = 52
    packed = struct.pack('!H', length) + su
    _result = sock.sendall(packed)

def _transmit_aal0(host, span, timeslots):
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

    payload = [133,2,64,0,0,53,0,1,0,33,0,10,
               2,2,8,6,1,16,18,82,85,33,10,6,
               7,1,17,19,83,85,0,110,0,80,80,
               80,80,80,80,80,0,0,0,33,44,187,215,80]

    cell_header = [0, 0, 0, 82]  # VCI = 0, VPI = 5, last cell in AAL5

    _aal0_send(data, bytes(cell_header) + bytes(payload))

    time.sleep(2)

    api.delete(aal0_id)
    data.close()

    api.bye()

def _check_for_duplex_firmware(host):
    api = gth.apilib.API(host, 0)    # 0=quiet, 3=verbose debugging

    try:
        api.enable("pcm1A", {})
    except gth.apilib.SemanticError:
        print("Enable failed. Is this hardware capable of transmit? Aborting.")
        sys.exit(1)

def main():
    """Entry point"""

    if len(sys.argv) != 3:
        _usage()
        sys.exit(-1)

    _check_for_duplex_firmware(argv[1])

    # 64 kbit/s MTP-2
    _transmit_mtp2(argv[1], argv[2], [1])

    # 1536 kbit/s MTP-2 (i.e. Q.703 Annex A, high speed)
    _transmit_mtp2(argv[1], argv[2], list(range(1, 25)))

    # 1536 kbit/s ATM AAL0
    _transmit_aal0(argv[1], argv[2], list(range(1, 25)))

main()
