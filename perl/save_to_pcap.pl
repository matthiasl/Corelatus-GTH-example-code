#!/usr/bin/perl -Igth_control/lib/
#
# Title: Sniff SS7 from an E1/T1 timeslot, save to a file (or stdout) in
#        libpcap format.
#
# The libpcap file format is documented here:
#
#   http://wiki.wireshark.org/Development/LibpcapFileFormat
#
# The GTH API is documented here:
#
#   http://www.corelatus.com/gth/api/
#
# Author: Matthias Lang (matthias@corelatus.se)
#

use 5.008;         # Probably works with earlier versions too.
use strict;
use warnings;
use gth_control;
use bigint;
use Data::Dumper;

sub usage() {
    print("
    save_to_pcap.pl <hostname> <span> <timeslot> <filename>
       <hostname>: the hostname or IP address of a GTH
           <span>: the name of an E1/T1, e.g. 1A or 4D on a GTH 2.x
       <timeslot>: 1--31 on an E1 or 1--24 on a T1
       <filename>: a writeable file; - means standard output

Typical invocation #1: ./save_to_pcap.pl 172.16.1.10 1A 16 /tmp/captured.pcap
Typical invocation #2: ./save_to_pcap.pl 172.16.1.10 1A 16 - | tshark -i -
Typical invocation #3: ./save_to_pcap.pl 172.16.1.10 1A 16 - | wireshark -k -i -
");
}

# Check that a given PCM is in a state where it could give useful data.
# That means in 'OK' or 'RAI' status.
sub warn_if_l1_dead {
    my ($api, $span) = @_;

    my $status = $api->query_resource("pcm$span", "status");

    if ($status eq "OK") {
    } elsif ($status eq "RAI") {
    } elsif ($status eq "disabled") {
	print STDERR "Warning: pcm$span is disabled. The GTH won't actually emit any data.\n".
	    "Hint: enable L1 with enable_l1.pl\n";
    } else {
	print STDERR "Warning: pcm$span status is $status\n" .
	    "Chances are the other end of your E1 isn't plugged in (or enabled)\n";
    }
}

# PCAP has a global header at the start. It's safe to write in machine-endian
# order, libpcap uses the magic to figure out which endianness it's in.
sub write_pcap_global_header {
    my ($file) = @_;

    my ($magic, $major_version, $minor_version, $GMT_to_localtime, $sigfigs);
    my ($snaplen, $network);

    $magic = 0xa1b2c3d4;
    $major_version = 2;
    $minor_version = 4;
    $GMT_to_localtime = 0;
    $sigfigs = 0;
    $snaplen = 65535;
    $network = 140;   # 140 == MTP-2

    my $header = pack("LSSLLLL", $magic, $major_version, $minor_version, 
		      $GMT_to_localtime, $sigfigs, $snaplen, $network);

    print $file $header;
}

sub monitor_mtp2 {
    my ($host, $span, $timeslot, $filename) = @_;

    my $api = gth_control->new($host);
    my $file;

    warn_if_l1_dead($api, $span);

    if ($filename eq "-") {
	$file = *STDOUT;
	$| = 1;
    } else {
	open($file, ">", $filename) || die("can't open $filename");
    }

    write_pcap_global_header($file);

    my ($mtp2_id, $data) = $api->new_mtp2_monitor($span, $timeslot);

    while (1) {
	read($data, my $b, 2);
	my $length = unpack("n", $b);
	read($data, my $packet, $length);

	my ($tag, $flags, $timestamp_hi, $timestamp_lo) 
	    = unpack("nnnN", $packet);
	my ($ts_sec, $ts_us);

	$ts_us = ($timestamp_lo + ($timestamp_hi << 32)) * 1000;

	$ts_sec = $ts_us / 1000000;
	$ts_us  = $ts_us % 1000000;

	my $payload_length = $length - 10;  # we'll strip the GTH header

	my $pcap_packet_header = pack("LLLL", $ts_sec, $ts_us,
				      $payload_length, $payload_length);

	print $file $pcap_packet_header;
	my $payload = substr($packet, 10);  # strip the GTH header
	print $file $payload;
    }
    
    $api->delete($mtp2_id);
    $data->close();

    $api->bye();
}

# Entry point
if ($#ARGV + 1 != 4) {
    usage();
    exit(-1);
}

monitor_mtp2($ARGV[0], $ARGV[1], $ARGV[2], $ARGV[3]);
