# Python version of the transport layer of the GTH API.
#
# Author: Matthias Lang (matthias@corelatus.se)
#
# This is blocking code. The GTH can send asynchronous information at any
# time, so blocking isn't so great. Options:
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

import sys
from sys import stderr
import socket
import select
import gth.parse
import pyparsing

server_port = 2089

class API_socket:
  "handle port 2089 socket transport to a Corelatus GTH module"

  def __init__(self, gth_ip_or_hostname):
    self.remotehost = gth_ip_or_hostname
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.settimeout(5.0)

    try:
      s.connect((self.remotehost, server_port))
    except socket.error:
      raise TransportError(("unable to connect to", \
                            self.remotehost, server_port))

    self._parser = gth.parse.gth_out()
    self._socket = s
    self._file = s.makefile()

  def send(self, data, type="text/xml"):
    s = self._socket
    s.sendall("Content-type: %s\r\n" % type)
    s.sendall("Content-length: %d\r\n\r\n" % len(data))
    s.sendall(data)

  def send_audio(self, data):
    send(self, data, "binary/audio")

  def send_fs_image(self, data):
    send(self, data, "binary/filesystem")

  def check_is_readable(self, timeout):
    readable, writable, exceptional = select.select([self._file], [], [self._file], timeout)
    if readable == []:
      raise TransportError("timeout")

  def receive_raw(self, timeout = 5000):
    """Return the next block from the API socket"""

    self.check_is_readable(timeout)

    try:
      first = self._file.readline(100)
      second = self._file.readline(100)
      blank = self._file.readline(100)

      length = int(second.strip().split(":")[1])

    except socket.error:
      raise TransportError("didn't get all three header lines")

    except IndexError:
      raise TransportError("corrupt length header")

    except ValueError:
      raise TransportError("corrupt length header")

    return self._definite_read(length)

  def receive(self, timeout = 5000):
    """Return the next block from the API socket, parsed"""
    string = self.receive_raw(timeout)
    try:
      return self._parser.parseString(string)
    except pyparsing.ParseException, detail:
      raise ParseError, (string, detail)

  # read exactly the number of bytes requested, or fail
  def _definite_read(self, length):
    data = self._file.read(length)

    # Python seems to guarantee that we get the requested number of
    # octets from the file descriptor above, as long as we're in
    # blocking IO mode. So the code below is a placeholder.
    if len(data) != length:
      data.extend(_definite_read(self, length - len(data)))

    return data

class TransportError(Exception):
  def __init__(self, clue):
    self._clue = clue

class ParseError(Exception):
  def __init__(self, text, exception):
    self._text = text
    self._exception = exception
    stderr.write("Unexpected and unparseable GTH reply:\n")
    stderr.write(text)
    stderr.write("\n")
