#!/usr/bin/python
#
# Title: Run commands on the GTH from python
#
# (Everything you can do with this Python example can also be done
# using the GTH's in-built SSH CLI; the point of this example is to
# show how to use 'apilib.py')
#
# Author: Matthias Lang (matthias@corelatus.se)

import sys
from sys import argv, stderr
import gth.apilib

def usage():
    stderr.write("""
gth.py [-vN] <hostname> <command> [<argument> [<argument> ...]]

   <command>: disable | enable | query | reset | set
  <hostname>: the hostname or IP address of a GTH
  <argument>: the arguments depend on the command. See 'examples' below

Examples:

     ./gth.py disable 172.16.1.10 sdh1
     ./gth.py enable  172.16.1.10 sdh1
     ./gth.py map     172.16.1.10 sdh1:hop1_1:lop1_1_1
     ./gth.py query   172.16.1.10 os
     ./gth.py reset   172.16.1.10
     ./gth.py set     172.16.1.10 eth2 "IP4 address" 192.168.1.15
     ./gth.py unmap   172.16.1.10 pcm13
     ./gth.py zero    172.16.1.10 pcm13


""")
    sys.exit(-1)

def main():
    # Table of commands. The number is the expected argument count. Negative
    # means that the count is a minimum.
    commands = {"disable": (disable, 1),
                "enable":  (enable, -1),
                "map":     (map, 1),
                "query":   (query, 1),
                "reset":   (reset, 0),
                "set":     (set, -3),
                "unmap":   (unmap, 1),
                "zero":    (zero, 1)
                };

    sys.argv.pop(0)

    verbosity = 0
    if len(sys.argv) > 0 and "-v" in sys.argv[0]:
        if len(sys.argv[0]) < 3:
            usage()
	if sys.argv[0][2].isdigit:
		verbosity = int(sys.argv[0][2])
	sys.argv.pop(0)

    if len(sys.argv) < 2:
        usage()

    bad_command = (usage, len(sys.argv))
    host = sys.argv.pop(0)
    f, expected_args = commands.get(sys.argv.pop(0), bad_command)

    if len(sys.argv) < abs(expected_args):
        usage()

    if expected_args >= 0 and len(sys.argv) > expected_args:
        usage()

    try:
        api = gth.apilib.API(host, verbosity)
        f(api , sys.argv)
        api.bye()

    except gth.apilib.SemanticError:
        die("bad argument")

    except gth.transport.TransportError:
        die("unable to connect to host: %s" % host)

def die(why):
    print why
    sys.exit(-1)

#--------------------
# Commands

def disable(api, args):
    api.disable(args.pop(0))

def enable(api, args):
    name = args.pop(0)
    api.enable(name, list_to_kvs(args))

def map(api, args):
    api.map("pcm_source", args.pop(0))

def query(api, args):
    name_or_id = args.pop(0)
    if not is_resource(api, name_or_id):
        raise gth.apilib.SemanticError("no such resource")

    result = api.query_resource(name_or_id)

    if name_or_id == "inventory":
        for n in result:
            print n
    else:
        for k, v in result.iteritems():
            print "%s=%s" % (k, v)

def reset(api, dontcare):
    api.reset()
    die("Reset command sent OK, terminating.")

def set(api, args):
    name = args.pop(0)
    api.set(name, list_to_kvs(args))

def unmap(api, args):
    api.unmap(args.pop(0))

def zero(api, args):
    name_or_id = args.pop(0)
    if is_resource(api, name_or_id):
        print "zeroing a resource"
        api.zero_resource(name_or_id)
    else:
        print "zeroing a job %s" % name_or_id
        api.zero_job(name_or_id)

#--------------------

def is_resource(api, name):
    return name == "inventory" or name in api.query_resource("inventory")

def list_to_kvs(list):
    result = []
    length = len(list)

    while (length >= 2):
        key = list.pop(0)
        value = list.pop(0)
        result.append( (key, value) )
        length -= 2

    return result

main()
