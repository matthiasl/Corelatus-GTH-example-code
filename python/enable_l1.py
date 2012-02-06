#!/usr/bin/python
#
# Title: Enable an E1 on a Corelatus GTH
# Author: Matthias Lang (matthias@corelatus.se)

import sys
from sys import argv, stderr
import gth.apilib

import socket

def usage():
    stderr.write("""
enable_l1.py <hostname> <span>

  <hostname>: the hostname or IP address of a GTH
      <span>: the name of an E1/T1, e.g. 1A or 4D on a GTH 2.x

Typical invocation: ./enable_l1.py 172.16.1.10 1A
""")

def main():
    if len(sys.argv) != 3:
        usage()
        sys.exit(-1)

    api = gth.apilib.API(sys.argv[1])
    api.send("<set name='pcm%s'><attribute name='mode' value='E1'/></set>" %
             sys.argv[2])

main()
