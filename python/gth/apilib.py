# Title: Utilities for talking to a Corelatus GTH from python
# Author: Matthias Lang (matthias@corelatus.se)
#

import sys
from sys import stderr

from gth.transport import API_socket
import socket

class API:
    def __init__(self, gth_ip_or_hostname, verbosity=0):
        """
        verbosity=0: keep mostly quiet
        verbosity=1: print event counts
        verbosity=2: print events
        verbosity=3: print all commands, responses and events
        """
        self.verbosity = verbosity
        self.socket = API_socket(gth_ip_or_hostname)

    def bye(self):
        self.send("<bye/>")
        self.check_ok("bye")

    def delete(self, id):
        """Delete the given job"""
        self.send(xml_tag("delete", {'id': id}))
        self.check_ok("delete")

    def disable(self, name):
        """Disable an E1/T1 or SDH/SONET interface"""
        self.send(xml_tag("disable", {'name': name}))
        self.check_ok("disable")

    def enable(self, name, attributes):
        """Enable an E1/T1 or SDH/SONET interface"""
        self.send(xml_tag("enable", {'name': name}, xml_attributes(attributes)))
        self.check_ok("enable")

    def map(self, type, name):
        """Map (assign a name) an E1/T1 carried on an SDH/SONET interface"""
        if type != "pcm_source":
            raise SemanticError("tried to map something other than a pcm_source")
        SDH = xml_tag("sdh_source", {'name': name})
        self.send(xml_tag("map", {'target_type': 'pcm_source'}, SDH))

        reply, _events = self.next_non_event()
        if reply[0] != "resource":
            stderr.write(str(reply) + "\n")
            se = ("should have returned a resource", "map", reply)
            raise SemanticError(se)
        print(reply.name)

    def new_atm_aal0_layer(self, span, timeslots, opts = {}):
        """Return a (job_id, socket) tuple. Writing to the returned
        socket results in AAL0 cells (packets) being transmitted.
        The data format on the socket is described
        in the GTH API manual, under 'new atm_aal0_layer'."""

        opts, ls = self._tcp_listen()
        AAL0 = xml_tag("atm_aal0_layer", opts, sources_sinks(span, timeslots))
        return self._new_data(ls, AAL0)

    def new_atm_aal5_monitor(self, span, timeslot_list, vpi_vci, opts = {}):
        """Return a (job_id, socket) tuple.
        Monitor ATM AAL5 on a GTH. Socket returned uses the format defined in
        the GTH API manual, under new_atm_aal5_monitor."""

        opts, ls = self._tcp_listen()
        (vpi, vci) = vpi_vci
        opts['vpi'] = "%d" % vpi
        opts['vci'] = "%d" % vci

        AAL5 = xml_tag("atm_aal5_monitor", opts, sources(span, timeslot_list))
        return self._new_data(ls, AAL5)

    def new_fr_layer(self, span, timeslots):
        """Return a (job_id, socket) tuple. Writing to the returned
        socket results in signal units (packets) being transmitted
        on the timeslot. The data format on the socket is described
        in the GTH API manual, under 'new fr_layer'."""

        opts, ls = self._tcp_listen()
        FR = xml_tag("fr_layer", opts, sources_sinks(span, timeslots))
        return self._new_data(ls, FR)

    def new_fr_monitor(self, span, timeslots):
        """Return a (job_id, socket) tuple.  Monitor Frame Relay on a
        GTH. Socket returned uses the format defined in the GTH API
        manual, under new_fr_monitor."""

        opts, ls = self._tcp_listen()
        FR = xml_tag("fr_monitor", opts, sources(span, timeslots))
        return self._new_data(ls, FR)

    def new_mtp2_monitor(self, span, timeslot):
        """Return a (job_id, socket) tuple.
        Monitor MTP-2 on a GTH. Socket returned uses the format defined in
        the GTH API manual, under new_mtp2_monitor."""

        opts, ls = self._tcp_listen()
        mtp2 = xml_tag("mtp2_monitor", opts, sources(span, [timeslot]))
        return self._new_data(ls, mtp2)

    def new_player(self, span, timeslot):
        """Return a (job_id, socket) tuple. Create a timeslot player."""

        opts, ls = self._tcp_listen()

        player = xml_tag("player", {}, \
                         xml_tag("tcp_source", opts) + sinks(span, [timeslot]))
        return self._new_data(ls, player)

    def new_recorder(self, span, timeslot):
        """Return a (job_id, socket) tuple. Create a timeslot recorder."""

        opts, ls = self._tcp_listen()
        recorder = xml_tag("recorder", {}, sources(span, [timeslot]) + xml_tag("tcp_sink", opts))
        return self._new_data(ls, recorder)

    def new_v110_monitor(self, span, timeslot, first_bit, n_bits, ra0="no"):
        """Return a (job_id, socket) tuple.
        Monitor V.110. Socket returned uses the format defined in
        the GTH API manual, under new_v110_monitor."""

        IP, _api_port = self.socket._socket.getsockname()
        opts, ls = self._tcp_listen()
        opts['rate'] = 4800 * n_bits
        opts['ra0'] = ra0

        source = sources(span, timeslot, first_bit, n_bits)
        v110 = xml_tag("v110_monitor", opts, source)
        return self._new_data(ls, v110)

    def query_resource(self, name):
        """Return a dict of attributes. Query a GTH resource"""

        self.send(xml_tag("query", {}, xml_tag("resource", {'name': name})))
        reply, _events = self.next_non_event()
        if reply[0] != "state":
            raise SemanticError( ("query failed", reply) )

        if name == "inventory":
            result = []
            reply.pop(0)
            while len(reply) >= 2:
                reply.pop(0)
                result.append(reply.pop(0)[1])
            return result

        else:
            return reply[3]

    def reset(self):
        """Reset (reboot) the GTH"""

        self.send(xml_tag("reset", {}, xml_tag("resource", {'name': 'cpu'})))
        self.check_ok("reset")

    def set(self, name, attributes):
        "Set attributes on a resource"
        self.send(xml_tag("set", {'name': name}, xml_attributes(attributes)))
        self.check_ok("set")

    def unmap(self, name):
        "Unmap a resource"
        self.send(xml_tag("unmap", {'name': name}))
        self.check_ok("unmap")

    def zero_job(self, id):
        "Clear the counters on a job"
        self.send(xml_tag("zero", {}, xml_tag("job", {'id': id})))
        self.check_ok("zero")

    def zero_resource(self, name):
        "Clear the counters on a resource"
        self.send(xml_tag("zero", {}, xml_tag("resource", {'name': name})))
        self.check_ok("zero")

    #---- The remaining functions are primarily intended for internal
    #     use. They're also useful for implementing new commands.
    def send(self, XML):
        if self.verbosity >= 3:
                stderr.write("C: %s\n" % XML)
        self.socket.send(XML)

    def next_non_event(self):
        """Return a tuple (answer, events).
        Answer is the next reply from the GTH and events is a list of all
        asynchronous data before that"""

        events = []

        while True:
            answer = self.socket.receive()
            if answer[0] == 'event':
                if self.verbosity >= 2:
                        stderr.write("G: %s\n" % answer)
                events.append(answer)
            else:
                if self.verbosity == 1:
                        stderr.write("G: skipping %d events\n" % len(events))
                if self.verbosity >= 3:
                        stderr.write("G: %s\n" % answer)
                return (answer, events)

    def next_event(self):
        """Block, waiting for an event. Return that event"""
        return self.socket.receive()

    def check_ok(self, command):
        reply, _events = self.next_non_event()
        if reply[0] != "ok":
            stderr.write("expected OK, got %s\n" % reply)
            se = ("should have returned OK", command, reply)
            raise SemanticError(se)

    def receive_job_id(self):
        """Return a tuple (id, events)
        If the next reply from the GTH is not a job-id, raise SemanticError"""

        answer, events = self.next_non_event()
        if answer[0] == 'job':
            return (answer[1][1], events)
        else:
            raise SemanticError(answer)

    def _new_data(self, ls, xml):
        """Given a listen-socket and an XML command, start a job and
        accept the socket it creates."""

        self.send(xml_tag("new", {}, xml))
        id, _ignored_events = self.receive_job_id()
        data, _remote_address = ls.accept()
        ls.close()
        return (id, data)

    def _tcp_listen(self):
        """Create a server socket, i.e. one which listens. Return
        the port and IP in an 'opts' dictionary, plus the socket itself."""

        s = socket.socket(socket.AF_INET)
        s.bind(("", 0))
        s.listen(1)
        addr, port = s.getsockname()

        IP, _api_port = self.socket._socket.getsockname()

        opts = {}
        opts['ip_addr'] = IP
        opts['ip_port'] = "%d" % port

        return (opts, s)

    def warn_if_l1_dead(self, span, message = ""):
        attrs = self.query_resource("pcm" + span)
        if attrs['status'] in ["OK", "RAI"]:
            return
        else:
            if attrs['status'] == "disabled":
                stderr.write("""
Warning: pcm%s is disabled. No data will go in or out.
         %s
         Hint: enable L1 with 'gth.py'
""" % (span, message))
            else:
                stderr.write("""
Warning: pcm%s status is %s

Is there really a signal on pcm%s? %s
""" % (span, attrs['status'], span, message))

#--------------------
# Rest of the file is just functions, i.e. not part of the class

def sinks(span, timeslots):
    "Return a string with an XML representation of the sinks"

    sinks = ""
    for ts in timeslots:
        sinks += xml_tag("pcm_sink", {'span': span, 'timeslot': ts})

    return sinks

def sources(span, timeslots, first_bit = 0, n_bits = 8):
    "Return a string with an XML representation of the sources"

    sources = ""
    for ts in timeslots:
        sources += xml_tag("pcm_source", {'span': span, 'timeslot': ts})

    return sources

def sources_sinks(span, timeslots):
    return sources(span, timeslots) + sinks(span, timeslots)

def xml_tag(name, attributes, children = []):
    if (children == []):
        return "<%s %s/>" % (name, xml_tag_attributes(attributes))
    else:
        return "<%s %s>%s</%s>" \
            % (name, xml_tag_attributes(attributes), children, name)

def xml_tag_attributes(opts):
    return ' '.join(f'{k}="{v}"' for k, v in opts.items())

def xml_attribute( key_value ):
    (key, value) = key_value
    return xml_tag("attribute", {'name': key, 'value': value})

def xml_attributes(attrs):
    result = ""
    for x in attrs:
        result += xml_attribute(x)
        return result

class SemanticError(Exception):
    def __init__(self, clue):
        self.clue = clue
