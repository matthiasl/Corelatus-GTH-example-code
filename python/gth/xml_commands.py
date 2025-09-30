"""Functions for generating XML strings for the GTH API
"""

# Copyright (c) 2020–2025, Corelatus AB
# All rights reserved.
#
# Licensed under the BSD 3-Clause License. See the LICENSE file
# in the project root for full license information.

def attribute( key_value ):
    "Return a string with an <attribute>"

    (key, value) = key_value
    return f"<attribute name='{key}' value='{value}'/>"

def attributes(attrs):
    "Return a string with a list of attributes"

    return "".join(attribute(x) for x in attrs)

def job(job_id):
    "Return a job tag"

    tag("job", {'id': job_id})

def new(children):
    "Return a string with an XML representation of a <new> command"

    return tag("new", {}, children)

def options(kvs):
    "Return a string with an XML representation of a list of key/value opts"

    return "".join(f" {key}='{val}' " for key, val in kvs.items())

def pcm_source(span, timeslot):
    "Return a string with an XML representation of one source"

    return tag("pcm_source", {"span": span, "timeslot": timeslot})

def pcm_sources(span, timeslots):
    "Return a string with an XML representation of the sources"

    return "".join(pcm_source(span, ts) for ts in timeslots)

def query_resource(name):
    "Return a string with an XML representation of a <query> command"

    res = tag("resource", {"name": name})
    return tag("query", None, res)

def resource(name):
    "Return a string with an XML resource"

    return tag("resource", {'name': name}, None)


def tag(name, opts=None, children=None):
    "Return a string with an XML tag"

    if opts is None:
        optstring = ""
    else:
        optstring = options(opts)

    if children is None:
        return f"<{name} {optstring}/>"

    return f"<{name} {optstring}>{children}</{name}>"
