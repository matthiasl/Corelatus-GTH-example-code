# Title: Utilities for talking to a Corelatus GTH from python
# Author: Matthias Lang (matthias@corelatus.se)
#
# I could have derived API from API_socket. I couldn't see a compelling
# reason to do so, which is why I just composed the classes.

from transport import API_socket
import socket

class API:
    def __init__(self, gth_ip_or_hostname):
        self.socket = API_socket(gth_ip_or_hostname)

    def send(self, XML):
        self.socket.send(XML)

    def bye(self):
        self.socket.send("<bye/>")
        reply, _events = self.next_non_event()
        if reply[0] != 'ok':
            raise SemanticError(("bye should have returned OK", reply))

    def next_non_event(self):
        """Return a tuple (answer, events).
        Answer is the next reply from the GTH and events is a list of all
        asynchronous data before that"""

        events = []

        while True:
            answer = self.socket.receive()
            if answer[0] == 'event':
                events.append(answer)
            else:
                return (answer, events)

    def next_event(self):
        """Block, waiting for an event
        Return that event"""
        return self.socket.receive()

    def new_player(self, span, timeslot):
        """Returns a (job_id, socket) tuple.
        Create a timeslot player on a GTH."""

        IP, _api_port = self.socket._socket.getsockname()

        port, ls = self.listen()
        self.socket.send("<new><player>" \
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
        port, ls = self.listen()
        self.socket.send("<new><recorder>"\
                                "<pcm_source span='%s' timeslot='%d'/>"\
                                "<tcp_sink ip_addr='%s' ip_port='%d'/>"\
                                "</recorder></new>"\
                                % (span, timeslot, IP, port) )
        recorder_id, _ignored_events = self.receive_job_id()
        data, _remote_address = ls.accept()
        ls.close()

        return (recorder_id, data)

    def new_mtp2_monitor(self, span, timeslot):
        """Returns a (job_id, socket) tuple.
        Monitor MTP-2 on a GTH. Socket returned uses the format defined in
        the GTH API manual, under new_fr_monitor."""

        IP, _api_port = self.socket._socket.getsockname()
        port, ls = self.listen()
        self.socket.send("<new><mtp2_monitor ip_addr='%s' ip_port='%s'>"\
                                "<pcm_source span='%s' timeslot='%d'/>"\
                                "</mtp2_monitor></new>"\
                                % (IP, port, span, timeslot) )
        mtp2_id, _ignored_events = self.receive_job_id()
        data, _remote_address = ls.accept()
        ls.close()

        return (mtp2_id, data)

    def delete(self, ID):
        "Delete the given job"
        self.socket.send("<delete id='%s'/>" % ID)
        reply = self.socket.receive()
        if reply[0] != "ok":
            raise SemanticError(("delete should have returned OK", reply))

    def receive_job_id(self):
        """Return a tuple (ID, events)
        If the next reply from the GTH is not a jobId, we raise SemanticError"""
        answer, events = self.next_non_event()
        if answer[0] == 'job':
            return (answer[1][1], events)
        else:
            raise SemanticError(answer)

    # REVISIT: this could be a static method. Does python have static methods?
    def listen(self):
        """Create a server socket, i.e. one which listens.
        Returns (port_number, socket)"""

        s = socket.socket(socket.AF_INET)
        s.bind(("", 0))
        s.listen(1)
        addr, port = s.getsockname()
        return (port, s)

class SemanticError(Exception):
    def __init__(self, clue):
        self.clue = clue
