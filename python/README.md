# Corelatus GTH Python Example Code

**Author:** matthias@corelatus.se

## Introduction

This is example code in Python for a Corelatus GTH. It depends on the [`pyparsing`](https://pypi.org/project/pyparsing/) package.

The examples were tested on Python 3 on Linux.

Each example is built on top of a small library, `gth/apilib.py`, which lets you write Python code for almost anything supported by the GTH—no need to write your own parser.

---

## Command Line for Exploring GTH Configuration

`gth.py` replicates most of the functionality of the GTH's built-in SSH CLI:

- Enable/disable E1/T1 and SDH/SONET interfaces
- Query attributes on all resources (e.g., counters for E1/T1, SDH/SONET, Ethernet)
- Set attributes on resources (e.g., IP address, NTP server)
- Reset counters (e.g., clear error counters on interfaces)
- Reboot the GTH

Run `gth.py` without arguments to see usage examples.

---

## E1/T1 Timeslot Dump to a File

Records a bit-exact copy of an E1 timeslot to a local file:

```bash
./record.py 172.16.1.10 1A 16 4000 signalling.raw
All done, wrote 4000 octets
```

---

## Recorded File Playback

> **Requires 'Messenger' hardware**

Plays back a previously recorded timeslot on an E1/T1:

```bash
./play.py 172.16.1.10 1A 16 signalling.raw
All done, sent 2276 octets
```

The file can contain audio (8kHz A-law, no header) or signalling data. The GTH treats both formats the same. Usually, this file was created using the timeslot dump example above.

---

## Sniff ISUP

Prints when calls start and stop on an SS7 link.

Uses the GTH to decode MTP-1 and MTP-2. Python handles MTP-3 and ISUP decoding.

```bash
./sniff_isup.py 172.16.1.10 1A 16
IAM called party: 21255512 calling party: 11313555 CIC=0
ignoring ISUP address complete
ignoring ISUP answer
ignoring ISUP release
RLC on CIC=0
```

---

## Transmit Signalling

> **Requires 'Messenger' hardware**

Demonstrates transmitting:
- 64 kbit/s MTP-2
- MTP-2 Annex A (1534 kbit/s high speed)
- ATM AAL0 signalling

```bash
./gth.py 172.16.1.10 enable pcm1A mode T1
./transmit_ss7.py gth30 1A
```

You can connect a loopback cable between 1A and 2A (P1 and P2 connectors) to observe the signalling using either:

- `record.py` (raw bit-stream)
- `save_to_pcap` (C example to decode signalling)
