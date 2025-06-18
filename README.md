# Example code for the GTH API
**Author:** Matthias (matthias@corelatus.com)

---

A Corelatus GTH is a standalone unit which connects to the telephone
network through the E1/T1 or SDH interface. It's used to build
voicemail systems, billing systems and network monitoring systems.

---

The examples are broken up by language, and each language is
independent of the rest, i.e. the C examples don't use any code from
the Python examples.

### Python: A Python API for the GTH.

Supports a wide range of GTH features and includes a collection of
examples showing bare-bones use of key features.

### erlang: A feature-complete GTH API

Both the parser and API are in widespread live use. This code covers
all GTH features.

### C: A complete parser for GTH responses

Multiple customers use the `save_to_pcap` example in production. This
code has support for compling on either Unix-like systems or Windows.

---

The Perl and Java code is rudimentary. Some customers use it for ad-hoc O&M.

More information about GTH hardware:

https://www.corelatus.com/

More information about the GTH API:

https://www.corelatus.com/gth/api/

Git repo of this code, with commit history:

https://github.com/matthiasl/Corelatus-GTH-example-code/
