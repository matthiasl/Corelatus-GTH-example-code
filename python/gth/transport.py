# Copyright (c) 2020–2025, Corelatus AB
# All rights reserved.
#
# Licensed under the BSD 3-Clause License. See the LICENSE file
# in the project root for full license information.
#

"""
Python version of the transport layer of the GTH API.
"""

# This is blocking code. The GTH can send asynchronous information at any
# time, so a more sophisticated application might want to relax this
# restriction. Options:
#
#    0. Ignore/delay the asynchronous information. Fine for
#       lab use and testing. Not an option for a robust application.
#
#    1. Poll the receive() method, catching the transport error
#       when nothing arrives. Requires the poll loop to be in a
#       thread of its own.
#
#    2. Rewrite/extend this code to be non-blocking.
#       This doesn't look that hard. We could just call
#       select on the API socket or sockets whenever we've
#       got nothing else to do and then read from the ones
#       which are readable.
#

from sys import stderr
import socket
import select
import pyparsing
import gth.parse


SERVER_PORT = 2089
SOCKET_TIMEOUT = 5.0   # seconds

class API_socket:
    "handle port 2089 socket transport to a Corelatus GTH module"

    def __init__(self, gth_ip_or_hostname):
        self.remotehost = gth_ip_or_hostname
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.settimeout(SOCKET_TIMEOUT)

        try:
            s.connect((self.remotehost, SERVER_PORT))
        except socket.error as exc:
            raise TransportError(("unable to connect to", \
                                  self.remotehost, SERVER_PORT)
                                 ) from exc

        self._parser = gth.parse.gth_out()
        self._socket = s
        self._file = s.makefile()

    def __enter__(self):
        return self

    def __exit__(self, _exception_type, _exception, _traceback):
        self.close()

    def close(self):
        "Close the command socket to the GTH"

        self._file.close()
        self._socket.close()  # probably superfluous. Better safe than sorry.

    def my_ip(self):
        "Return this machine's IP, as seen from the GTH"

        (ip, _api_port) = self._socket.getsockname()
        return ip

    def send(self, data, c_type="text/xml"):
        "Send an XML command to the GTH"

        s = self._socket
        message = f"Content-type: {c_type}\r\n"
        s.sendall(message.encode())
        message = f"Content-length: {len(data)}\r\n\r\n"
        s.sendall(message.encode())
        s.sendall(data.encode())

    def send_audio(self, data):
        "Send an audio clip to the GTH"

        self.send(data, "binary/audio")

    def send_fs_image(self, data):
        "Send a firmware image to the GTH"

        self.send(data, "binary/filesystem")

    def _check_is_readable(self, timeout):
        readable, _writable, _exceptional = select.select([self._file], [], [self._file], timeout)
        if readable == []:
            raise TransportError("timeout")

    def receive_raw(self, timeout):
        """Return the next block from the API socket"""

        self._check_is_readable(timeout)

        try:
            _first = self._file.readline(100)
            second = self._file.readline(100)
            _blank = self._file.readline(100)

            length = int(second.strip().split(":")[1])

        except socket.error as exc:
            raise TransportError("didn't get all three header lines") from exc

        except IndexError as exc:
            raise TransportError("corrupt length header") from exc

        except ValueError as exc:
            raise TransportError("corrupt length header") from exc

        return self._definite_read(length)

    def receive(self, timeout = SOCKET_TIMEOUT):
        """Return the next block from the API socket, parsed"""
        string = self.receive_raw(timeout)
        try:
            return self._parser.parseString(string)
        except pyparsing.ParseException as exc:
            raise ParseError(f"unable to parse {string}") from exc

    # Read exactly the number of bytes requested, or fail.
    def _definite_read(self, length):
        data = self._file.read(length)

        if len(data) != length:                # PEP 3116: impossible in the
            raise TransportError("short read") # absense of failure.

        return data

class TransportError(Exception):
    "Exception raised on transport problems"

    def __init__(self, clue):
        super().__init__(clue)
        self._clue = clue

class ParseError(Exception):
    "Exception raised on transport problems"

    def __init__(self, text):
        self._text = text
        super().__init__(text)
        stderr.write("Unexpected and unparseable GTH reply:\n")
        stderr.write(text)
        stderr.write("\n")
