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

    def delete(self, ID):
        "Delete the given job"
        self.send("<delete id='%s'/>" % ID)
        self.check_ok("delete")

    def disable(self, name):
        "Disable an E1/T1 or SDH/SONET interface"
        self.send("<disable name='%s'/>" % name)
        self.check_ok("disable")

    def enable(self, name, attributes):
        "Enable an E1/T1 or SDH/SONET interface"
        self.send("<enable name='%s'>%s</enable>"
                         % (name, format_attributes(attributes)))
        self.check_ok("enable")

    def map(self, Type, Name):
        "Map (assign a name) an E1/T1 carried on an SDH/SONET interface"
        if Type != "pcm_source":
            raise SemanticError("tried to map something other than a pcm_source")
        self.send("<map target_type='pcm_source'>" \
                             "<sdh_source name='%s'/></map>" % Name)
        reply, _events = self.next_non_event()
        if reply[0] != "resource":
            stderr.write(reply + "\n")
            se = ("should have returned a resource", command, reply)
            raise SemanticError(se)
        print(reply.name)

    def new_atm_aal0_layer(self, span, timeslots, opts = {}):
        """Returns a (job_id, socket) tuple. Writing to the returned
        socket results in AAL0 cells (packets) being transmitted.
        The data format on the socket is described
        in the GTH API manual, under 'new atm_aal0_layer'."""

        IP, _api_port = self.socket._socket.getsockname()
        port, ls = tcp_listen()

        opts['ip_addr'] = IP
        opts['ip_port'] = "%d" % port
        self.send("<new><atm_aal0_layer %s>"\
                      "%s %s"\
                      "</atm_aal0_layer></new>"\
                      % (options(opts), sources(span, timeslots), \
                         sinks(span, timeslots) ))
        aal0_id, _ignored_events = self.receive_job_id()
        data, _remote_address = ls.accept()
        ls.close()

        return (aal0_id, data)


    def new_atm_aal5_monitor(self, span, timeslot_list, vpi_vci, opts = {}):
        """Returns a (job_id, socket) tuple.
        Monitor ATM AAL5 on a GTH. Socket returned uses the format defined in
        the GTH API manual, under new_atm_aal5_monitor."""

        IP, _api_port = self.socket._socket.getsockname()
        port, ls = tcp_listen()
        opts['ip_addr'] = IP
        opts['ip_port'] = "%d" % port
        (vpi, vci) = vpi_vci
        opts['vpi'] = "%d" % vpi
        opts['vci'] = "%d" % vci

        self.send("<new><atm_aal5_monitor %s>%s" \
                  "</atm_aal5_monitor></new>"\
                  % (options(opts), sources(span, timeslot_list)))
        aal5_id, _ignored_events = self.receive_job_id()
        data, _remote_address = ls.accept()
        ls.close()

        return (aal5_id, data)

    def new_fr_layer(self, span, timeslots):
        """Returns a (job_id, socket) tuple. Writing to the returned
        socket results in signal units (packets) being transmitted
        on the timeslot. The data format on the socket is described
        in the GTH API manual, under 'new fr_layer'."""

        IP, _api_port = self.socket._socket.getsockname()
        port, ls = tcp_listen()

        self.send("<new><fr_layer ip_addr='%s' ip_port='%s'>"\
                      "%s %s"\
                      "</fr_layer></new>"\
                      % (IP, port, sources(span, timeslots), \
                         sinks(span, timeslots) ))

        fr_id, _ignored_events = self.receive_job_id()
        data, _remote_address = ls.accept()
        ls.close()

        return (fr_id, data)


    def new_fr_monitor(self, span, timeslots):
        """Returns a (job_id, socket) tuple.  Monitor Frame Relay on a
        GTH. Socket returned uses the format defined in the GTH API
        manual, under new_fr_monitor."""

        IP, _api_port = self.socket._socket.getsockname()
        port, ls = tcp_listen()
        self.send("<new><fr_monitor ip_addr='%s' ip_port='%s'>"\
                      "%s"\
                      "</fr_monitor></new>"\
                      % (IP, port, sources(span, timeslots)) )
        fr_id, _ignored_events = self.receive_job_id()
        data, _remote_address = ls.accept()
        ls.close()

        return (fr_id, data)

    def new_mtp2_monitor(self, span, timeslot):
        """Returns a (job_id, socket) tuple.
        Monitor MTP-2 on a GTH. Socket returned uses the format defined in
        the GTH API manual, under new_mtp2_monitor."""

        IP, _api_port = self.socket._socket.getsockname()
        port, ls = tcp_listen()
        self.send("<new><mtp2_monitor ip_addr='%s' ip_port='%s'>"\
                                "<pcm_source span='%s' timeslot='%d'/>"\
                                "</mtp2_monitor></new>"\
                                % (IP, port, span, timeslot) )
        mtp2_id, _ignored_events = self.receive_job_id()
        data, _remote_address = ls.accept()
        ls.close()

        return (mtp2_id, data)

    def new_player(self, span, timeslot):
        """Returns a (job_id, socket) tuple.
        Create a timeslot player on a GTH."""

        IP, _api_port = self.socket._socket.getsockname()

        port, ls = tcp_listen()
        self.send("<new><player>" \
                                "<tcp_source ip_addr='%s' ip_port='%d'/>"\
                                "<pcm_sink span='%s' timeslot='%d'/>" \
                                "</player></new>"\
                                % (IP, port, span, timeslot) )
        player_id, _ignored_events = self.receive_job_id()
        data, _remote_address = ls.accept()
        ls.close()

        return (player_id, data)

    def new_recorder(self, span, timeslot):
        """Returns a (job_id, socket) tuple.
        Create a timeslot recorder on a GTH."""

        IP, _api_port = self.socket._socket.getsockname()
        port, ls = tcp_listen()
        self.send("<new><recorder>"\
                                "<pcm_source span='%s' timeslot='%d'/>"\
                                "<tcp_sink ip_addr='%s' ip_port='%d'/>"\
                                "</recorder></new>"\
                                % (span, timeslot, IP, port) )
        recorder_id, _ignored_events = self.receive_job_id()
        data, _remote_address = ls.accept()
        ls.close()

        return (recorder_id, data)

    def new_v110_monitor(self, span, timeslot, first_bit, n_bits, ra0="no"):
        """Returns a (job_id, socket) tuple.
        Monitor V.110. Socket returned uses the format defined in
        the GTH API manual, under new_v110_monitor."""

        IP, _api_port = self.socket._socket.getsockname()
        port, ls = tcp_listen()
        self.send("<new>"\
                  "<v110_monitor ip_addr='%s' ip_port='%s' rate='%d' ra0='%s'>"\
                  "<pcm_source span='%s' timeslot='%d'"\
                  " first_bit='%d' bandwidth='%d'/>"\
                  "</v110_monitor></new>"\
                  % (IP, port, 4800 * n_bits, ra0,\
                     span, timeslot, first_bit, n_bits * 8) )
        id, _ignored_events = self.receive_job_id()
        data, _remote_address = ls.accept()
        ls.close()

        return (id, data)

    def query_resource(self, name):
        """Returns a dict of attributes
        Query a GTH resource"""
        self.send("<query><resource name='%s'/></query>" % name)
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
        "Reset (reboot) the GTH"
        self.send("<reset><resource name='cpu'/></reset>")
        self.check_ok("reset");

    def set(self, name, attributes):
        "Set attributes on a resource"
        self.send("<set name='%s'>%s</set>"
                         % (name, format_attributes(attributes)))
        self.check_ok("set");

    def unmap(self, Name):
        "Unmap a resource"
        self.send("<unmap name='%s'/>" % Name)
        self.check_ok("unmap")

    def zero_job(self, id):
        "Clear the counters on a job"
        self.send("<zero><job id='%s'/></zero>" % id)
        self.check_ok("zero")

    def zero_resource(self, name):
        "Clear the counters on a resource"
        self.send("<zero><resource name='%s'/></zero>" % name)
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
        """Block, waiting for an event
        Return that event"""
        return self.socket.receive()

    def check_ok(self, command):
        reply, _events = self.next_non_event()
        if reply[0] != "ok":
            stderr.write("expected OK, got %s\n" % reply)
            se = ("should have returned OK", command, reply)
            raise SemanticError(se)

    def receive_job_id(self):
        """Return a tuple (ID, events)
        If the next reply from the GTH is not a jobId, we raise SemanticError"""
        answer, events = self.next_non_event()
        if answer[0] == 'job':
            return (answer[1][1], events)
        else:
            raise SemanticError(answer)

def options(dict):
    "Returns a string with an XML representation of a list of key/value opts"

    list = ""
    for key,val in dict.items():
        list += " " + key + "='" + val + "'"
    return list

def sinks(span, timeslots):
    "Returns a string with an XML representation of the sources"

    list = ""
    for ts in timeslots:
        list += "<pcm_sink span='%s' timeslot='%d'/>" % (span, ts)

    return list

# To-do: Hardware and API supports subrate; this code doesn't (yet)
def sources(span, timeslots):
    "Returns a string with an XML representation of the sources"

    list = ""
    for ts in timeslots:
        list += "<pcm_source span='%s' timeslot='%d'/>" % (span, ts)

    return list


def tcp_listen():
    """Create a server socket, i.e. one which listens.
    Returns (port_number, socket)"""

    s = socket.socket(socket.AF_INET)
    s.bind(("", 0))
    s.listen(1)
    addr, port = s.getsockname()
    return (port, s)

def udp_listen():
    "Returns (port_number, socket)"

    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    s.bind(("", 0))
    addr, port = s.getsockname()
    return (port, s)

def format_attribute( key_value ):
    (key, value) = key_value
    return "<attribute name='" + key + "' value='" + value + "'/>"

def format_attributes( list):
    result = ""
    for x in list:
        result += format_attribute(x)
    return result

class SemanticError(Exception):
    def __init__(self, clue):
        self.clue = clue
