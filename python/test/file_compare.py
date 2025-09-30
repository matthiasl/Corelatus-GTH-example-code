#!/usr/bin/env python3

# file_compare <needle> <haystack>
#
# exit value is 0 if the file <needle> exists somewhere within <haystack>

import sys

if len(sys.argv) != 3:
    print(f"Usage: {sys.argv[0]} file1 file2")
    sys.exit(1)

with open(sys.argv[1], "rb") as f1, open(sys.argv[2], "rb") as f2:
        needle = f1.read()
        haystack = f2.read()
        if not needle in haystack:
            sys.exit(1)

sys.exit(0)
