"""
A python API for controlling a Corelatus GTH

The methods in this class follow the naming scheme in the API manual:

https://corelatus.se/gth/api/gth_api.pdf
"""
# Copyright (c) 2020–2025, Corelatus AB
# All rights reserved.
#
# Licensed under the BSD 3-Clause License. See the LICENSE file
# in the project root for full license information.
#
# See the examples, e.g. 'sniff_isup.py', to understand how to use this.

from sys import stderr, version_info
import socket

from gth.transport import API_socket
import gth.xml_commands as xmlc

class API:
    """A GTH API connection instance"""

    def __init__(self, gth_ip_or_hostname, verbosity=0):
        """
        verbosity=0: keep mostly quiet
        verbosity=1: print event counts
        verbosity=2: print events
        verbosity=3: print all commands, responses and events
        """

	# We don't run Python 2 any more. The code might work on Python 2
  	# but we don't test it.
        if version_info < (3, 8):
            raise RuntimeError("Python 3.8 or higher is required")
        self.verbosity = verbosity
        self.socket = API_socket(gth_ip_or_hostname)
        self.my_ip = self.socket.my_ip()

    def __enter__(self):
        return self

    def __exit__(self, _exception_type, _exception, _traceback):
        self.bye()

    def bye(self):
        "Terminate the API connection"

        self.send(xmlc.tag("bye"))
        self.check_ok("bye")
        self.socket.close()

    def delete(self, job_id):
        "Delete the given job"

        self.send(xmlc.tag("delete", {"id": job_id}))
        self.check_ok("delete")

    def disable(self, name):
        "Disable an E1/T1 or SDH/SONET interface"

        self.send(xmlc.tag("disable", {"name": name}))
        self.check_ok("disable")

    def enable(self, name, attributes):
        "Enable an E1/T1 or SDH/SONET interface"

        children = xmlc.attributes(attributes)
        self.send(xmlc.tag("enable", {'name': name}, children))
        self.check_ok("enable")

    def map(self, p_type, name):
        "Map (assign a name) an E1/T1 carried on an SDH/SONET interface"

        if p_type != "pcm_source":
            raise SemanticError("tried to map something other than a pcm_source")
        sdh = xmlc.tag("sdh_source", {"name": name})
        command = xmlc.tag("map", {"target_type": "pcm_source"}, sdh)
        self.send( command )
        reply, _events = self.next_non_event()
        if reply[0] != "resource":
            stderr.write(reply[0])
            se = ("should have returned a resource", command, reply)
            raise SemanticError(se)
        print(reply.name)

    def new_atm_aal0_layer(self, span, timeslots, opts = {}):
        """Return a (job_id, socket) tuple. Writing to the returned
        socket results in AAL0 cells (packets) being transmitted.
        The data format on the socket is described
        in the GTH API manual, under 'new atm_aal0_layer'."""

        opts, ls = self._tcp_listen()
        AAL0 = xmlc.tag("atm_aal0_layer", opts, _sources_sinks(span, timeslots))
        return self._new_data(ls, AAL0)

    def new_atm_aal5_monitor(self, span, timeslot_list, vpi_vci, opts = None):
        """Return a (job_id, socket) tuple.
        Monitor ATM AAL5 on a GTH. Socket returned uses the format defined in
        the GTH API manual, under new_atm_aal5_monitor."""

        if opts is None:
            opts = {}
        (l_opts, ls) = self._tcp_listen()
        opts |= l_opts
        (opts['vpi'], opts['vci']) = vpi_vci

        xml_sources = xmlc.pcm_sources(span, timeslot_list)
        aal5 = xmlc.tag("atm_aal5_monitor", opts, xml_sources)
        return self._new_data(ls, aal5)

    def new_fr_layer(self, span, timeslots):
        """Return a (job_id, socket) tuple. Writing to the returned
        socket results in signal units (packets) being transmitted
        on the timeslot. The data format on the socket is described
        in the GTH API manual, under 'new fr_layer'."""

        opts, ls = self._tcp_listen()
        fr = xmlc.tag("fr_layer", opts, _sources_sinks(span, timeslots))
        return self._new_data(ls, fr)

    def new_fr_monitor(self, span, timeslots):
        """Return a (job_id, socket) tuple.  Monitor Frame Relay on a
        GTH. Socket returned uses the format defined in the GTH API
        manual, under new_fr_monitor."""

        opts, ls = self._tcp_listen()
        src = xmlc.pcm_sources(span, timeslots)
        fr = xmlc.tag("fr_monitor", opts, src)
        return self._new_data(ls, fr)

    def new_mtp2_monitor(self, span, timeslot):
        """Return a (job_id, socket) tuple.
        Monitor MTP-2 on a GTH. Socket returned uses the format defined in
        the GTH API manual, under new_mtp2_monitor."""

        opts, ls = self._tcp_listen()
        src = xmlc.pcm_sources(span, [timeslot])
        mtp2 = xmlc.tag("mtp2_monitor", opts, src)
        return self._new_data(ls, mtp2)

    def new_player(self, span, timeslot):
        """Return a (job_id, socket) tuple.
        Create a timeslot player on a GTH."""

        opts, ls = self._tcp_listen()
        source_sink = xmlc.tag("tcp_source", opts)
        source_sink += xmlc.tag("pcm_sink", {"span": span, "timeslot": timeslot})
        player = xmlc.tag("player", {}, source_sink)
        return self._new_data(ls, player)

    def new_recorder(self, span, timeslot):
        """Return a (job_id, socket) tuple.
        Create a timeslot recorder on a GTH."""

        opts, ls = self._tcp_listen()
        source_sink = xmlc.pcm_source(span, timeslot)
        source_sink += xmlc.tag("tcp_sink", opts)
        recorder = xmlc.tag("recorder", {}, source_sink)
        return self._new_data(ls, recorder)

    def new_v110_monitor(self, span, timeslot, first_bit, n_bits, ra0="no"):
        """Return a (job_id, socket) tuple.
        Monitor V.110. Socket returned uses the format defined in
        the GTH API manual, under new_v110_monitor."""

        opts, ls = self._tcp_listen()

        pcm_opts = {"span": span, "timeslot": timeslot}
        pcm_opts |= {"first_bit": first_bit, "bandwidth": n_bits * 8}
        source = xmlc.tag("pcm_source", pcm_opts)
        opts |= {"rate": 4800 * n_bits, "ra0": ra0}
        v110 = xmlc.tag("v110_monitor", opts, source)
        return self._new_data(ls, v110)

    def query_resource(self, name):
        """Return a dict of attributes
        Query a GTH resource"""
        self.send( xmlc.query_resource(name) )
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

        return reply[3]

    def reset(self):
        "Reset (reboot) the GTH"
        self.send(xmlc.tag("reset", [], xmlc.resource('cpu')))
        self.check_ok("reset")

    def set(self, name, attributes):
        "Set attributes on a resource"

        children = xmlc.attributes(attributes)
        self.send(xmlc.tag("set", {'name': name}, children))
        self.check_ok("set")

    def unmap(self, name):
        "Unmap a resource"
        self.send(xmlc.tag("unmap", {"name": name}))
        self.check_ok("unmap")

    def zero_job(self, job_id):
        "Clear the counters on a job"
        self.send(xmlc.tag("zero", None, xmlc.job(job_id)))
        self.check_ok("zero")

    def zero_resource(self, name):
        "Clear the counters on a resource"
        self.send(xmlc.tag("zero", None, xmlc.resource(name)))
        self.check_ok("zero")

    def warn_if_l1_dead(self, span):
        """Print a warning message if the input E1 is in a state that won't
        result in useful data being processed"""

        attrs = self.query_resource("pcm" + span)
        if attrs['status'] in ["OK", "RAI"]:
            return

        if attrs['status'] == "disabled":
            stderr.write( (f"Warning: pcm{span} is disabled.\n"
                           f"Hint: enable L1 with 'gth.py'\n") )
        else:
            stderr.write( (f"Warning: pcm{span} status is {attrs['status']}\n"
                           f"Is there really a signal on pcm{span}?\n"))

    #---- The remaining functions are primarily intended for internal
    #     use. They're also useful for implementing new commands.
    def send(self, xml):
        "Send data, usually a command, to the GTH"

        if self.verbosity >= 3:
            stderr.write(f"C: {xml}\n")
        self.socket.send(xml)

    def next_non_event(self):
        """Return a tuple (answer, events).
        Answer is the next reply from the GTH and events is a list of all
        asynchronous data before that"""

        events = []

        while True:
            answer = self.socket.receive()
            if answer[0] == 'event':
                if self.verbosity >= 2:
                    stderr.write(f"G: {answer}\n")
                events.append(answer)
            else:
                if self.verbosity == 1:
                    stderr.write(f"G: skipping {len(events)} events\n")
                if self.verbosity >= 3:
                    stderr.write(f"G: {answer}\n")
                return (answer, events)

    def next_event(self, timeout):
        """Block, waiting for an event
        Return that event"""
        return self.socket.receive(timeout)

    def check_ok(self, command):
        """Make sure a command returned 'ok'; otherwise raise"""

        reply, _events = self.next_non_event()
        if reply[0] != "ok":
            stderr.write(f"expected OK, got {reply}\n")
            se = ("should have returned OK", command, reply)
            raise SemanticError(se)

    def receive_job_id(self):
        """Return a tuple (id, events)
        If the next reply from the GTH is not a job-id, raise SemanticError"""

        answer, events = self.next_non_event()
        if answer[0] != 'job':
            raise SemanticError(answer)

        return (answer[1][1], events)


    def _tcp_listen(self):
        """Create a server socket, i.e. one which listens. Return
        the port and IP in an 'opts' dictionary, plus the socket itself."""

        s = socket.socket(socket.AF_INET)
        s.bind(("", 0))
        s.listen(1)
        _addr, port = s.getsockname()

        opts = {'ip_addr': self.my_ip, 'ip_port': port}
        return (opts, s)


    def _new_data(self, ls, xml):
        """Given a listen-socket and an XML command, start a job and
        accept the socket it creates."""

        self.send(xmlc.new(xml))
        job_id, _ignored_events = self.receive_job_id()
        data, _remote_address = ls.accept()
        ls.close()
        return (job_id, data)

#--------------------
# Rest of the file is just functions, i.e. not part of the class

def _sinks(span, timeslots):
    "Return a string with an XML representation of the sinks"

    return "".join(
        xmlc.tag("pcm_sink", {"span": span, "timeslot": ts})
        for ts in timeslots
    )

def _sources(span, timeslots, first_bit = 0, bandwidth = 64):
    "Return a string with an XML representation of the sources"

    opts = {'span': span,
            'first_bit': first_bit,
            'bandwidth': bandwidth}

    return "".join(xmlc.tag("pcm_source", opts | {'timeslot': ts}) for ts in timeslots)

def _sources_sinks(span, timeslots):
    return _sources(span, timeslots) + _sinks(span, timeslots)

#--------------------

class SemanticError(Exception):
    """Exception raised when command processing fails"""
    def __init__(self, clue):
        self.clue = clue
