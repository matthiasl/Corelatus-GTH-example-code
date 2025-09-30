#!/usr/bin/env python3

"""Run commands on the GTH from python.

This module provides access to the same functions the SSH CLI
provides, except from python.
"""

# Copyright (c) 2020–2025, Corelatus AB
# All rights reserved.
#
# Licensed under the BSD 3-Clause License. See the LICENSE file
# in the project root for full license information.

import sys
from sys import stderr
import gth.apilib

def _usage():
    stderr.write("""
gth.py [-vN] <hostname> <command> [<argument> [<argument> ...]]

   <command>: disable | enable | map | query | reset | set | unmap | zero
  <hostname>: the hostname or IP address of a GTH
  <argument>: the arguments depend on the command. See 'examples' below

Examples:

     ./gth.py 172.16.1.10 disable sdh1
     ./gth.py 172.16.1.10 enable  sdh1
     ./gth.py 172.16.1.10 enable  pcm13
     ./gth.py 172.16.1.10 map     sdh1:hop1_1:lop1_1_1
     ./gth.py 172.16.1.10 query   os
     ./gth.py 172.16.1.10 reset
     ./gth.py 172.16.1.10 set     eth2 "IP4 address" 192.168.1.15
     ./gth.py 172.16.1.10 unmap   pcm13
     ./gth.py 172.16.1.10 zero    pcm13


""")
    sys.exit(-1)

def main():
    """entry point"""
    # Table of commands. The number is the expected argument count. Negative
    # means that the count is a minimum.
    commands = {"disable": (_disable, 1),
                "enable":  (_enable, -1),
                "map":     (_map, 1),
                "query":   (_query, 1),
                "reset":   (_reset, 0),
                "set":     (_set, -3),
                "unmap":   (_unmap, 1),
                "zero":    (_zero, 1)
                }

    sys.argv.pop(0)

    verbosity = 0
    if len(sys.argv) > 0 and "-v" in sys.argv[0]:
        if len(sys.argv[0]) < 3:
            _usage()
        if sys.argv[0][2].isdigit:
            verbosity = int(sys.argv[0][2])
        sys.argv.pop(0)

    if len(sys.argv) < 2:
        _usage()

    bad_command = (_usage, len(sys.argv))
    host = sys.argv.pop(0)
    f, expected_args = commands.get(sys.argv.pop(0), bad_command)

    if len(sys.argv) < abs(expected_args):
        _usage()

    if expected_args >= 0 and len(sys.argv) > expected_args:
        _usage()

    try:
        api = gth.apilib.API(host, verbosity)
        f(api , sys.argv)
        api.bye()

    except gth.apilib.SemanticError:
        _die("bad argument")

    except gth.transport.TransportError:
        _die(f"unable to connect to host: {host}")

def _die(why):
    print(why)
    sys.exit(-1)

#--------------------
# Commands

def _disable(api, args):
    api.disable(args.pop(0))

def _enable(api, args):
    name = args.pop(0)
    api.enable(name, _list_to_pairs(args))

def _map(api, args):
    api.map("pcm_source", args.pop(0))

def _query(api, args):
    name_or_id = args.pop(0)
    if not _is_resource(api, name_or_id):
        raise gth.apilib.SemanticError("no such resource")

    result = api.query_resource(name_or_id)

    if name_or_id == "inventory":
        for n in result:
            print(n)
    else:
        for k, v in result.items():
            print(f"{k}={v}")

def _reset(api, _dontcare):
    api.reset()
    _die("Reset command sent OK, terminating.")

def _set(api, args):
    name = args.pop(0)
    api.set(name, _list_to_pairs(args))

def _unmap(api, args):
    api.unmap(args.pop(0))

def _zero(api, args):
    name_or_id = args.pop(0)
    if _is_resource(api, name_or_id):
        print("zeroing a resource")
        api.zero_resource(name_or_id)
    else:
        print(f"zeroing a job {name_or_id}")
        api.zero_job(name_or_id)

#--------------------

def _is_resource(api, name):
    return name == "inventory" or name in api.query_resource("inventory")

def _list_to_pairs(seq):
    "Turn a list of alternating keys and values into list of pairs"

    it = iter(seq)
    return list(zip(it, it))

main()
