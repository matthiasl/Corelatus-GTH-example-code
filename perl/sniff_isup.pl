#!/usr/bin/perl -Igth_control/lib/
#
# Title: Sniff SS7 ISUP call setup/teardown from an E1/T1 timeslot
# Author: Matthias Lang (matthias@corelatus.se)
#
# Reference: ISUP decoding is in ITU-T Q.767 Annex C
#            MTP-2 is in ITU-T Q.703
#            MTP-3 is in ITU-T Q.704
#
# $Id: sniff_isup.pl,v 1.2 2009-05-28 12:51:39 matthias Exp $

use 5.008;         # Probably works with earlier versions too.
use strict;
use warnings;
use gth_control;
use Data::Dumper;

sub usage() {
    print("
    sniff_isup.pl <hostname> <span> <timeslot>
       <hostname>: the hostname or IP address of a GTH
           <span>: the name of an E1/T1, e.g. 1A or 4D on a GTH 2.x
       <timeslot>: 1--31 on an E1 or 1--24 on a T1

Typical invocation: ./sniff_isup.pl 172.16.1.10 1A 16
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

sub decode_mtp2 {
    my ($packet) = @_;
    # The first 10 octets is the GTH header, which we can ignore
    #     next 3 octets is MTP-2 FSN, BSN and LI, which we can ignore
    #     next 1 octet SIO and at least 4 octets of SIF
    #     finally 2 octets of CRC
    # 
    # So ignore anything shorter than 20 octets
    length($packet) >= 20 || return;

    my ($_tag, $_flags, $_reserved, $_ts_high, 
	$_ts_low, $_fsn, $_bsn, $_li, $sio, $sif_crc) 
	= unpack("nCCnN" . "CCC" . "Ca*", $packet);

    my $masked = $sio & 0x0f;
    
    my $sif = substr($sif_crc, 0, -2);
    decode_mtp3($sio, $sif);
}

# Q.704 14.2.1 and 14.2.2 defines the service codes.
sub decode_mtp3 {
    my ($sio, $sif) = @_;
    if (($sio & 0x0f) == 5) {          # Mask out the service indicator
	decode_isup(substr($sif, 4));  # skip the routing label
    }
}

sub decode_isup {
    my ($sif) = @_;
    my ($CIC, $type) = unpack("nC", $sif);
    my $rest = substr($sif, 3);
    my $action = {
	0x01 => \&isup_iam,
	0x10 => \&isup_rlc
    };

    my $decoder = $action->{$type};
    if (! defined $decoder) {
	$decoder = \&isup_ignore;
    }
    $decoder->($type, $CIC, $rest);
}

sub isup_iam {
    my ($ignore, $CIC, $sif) = @_;
    # First 5 octets can be ignored
    my ($_ignore, $_ignore2, $bnum_pointer, $anum_pointer) 
	= unpack("NCCC", $sif);
    my $bnum = substr($sif, 5 + $bnum_pointer);
    my $anum = substr($sif, 7 + $anum_pointer);
    my $decoded_bnum = isup_number($bnum);
    my $decoded_anum = isup_number($anum);
    print "IAM called party: $decoded_bnum calling party: $decoded_anum CIC=$CIC\n";
}

# Decode an ISUP number, as per C 3.7
sub isup_number {
    my ($num) = @_;
    my ($length, $even_odd, $_number_plan, $digits) = unpack("CCCa*", $num);
    $length -= 2;
    my $is_odd = ($even_odd & 0x80);

    my $string = "";

    for (my $index = 0; $index < $length; $index++) {
	my $digit = ord(substr($digits, $index, 1));
        if ($length == 1 && $is_odd) {
            $string .= chr($digit + 0x30);
	} else {
	    my $hi = $digit & 0x0f;
	    my $lo = $digit >> 4;
	    $string .= chr($hi + 0x30) . chr($lo + 0x30);
	}
    }
    return $string;
}

sub isup_rlc {
    my ($unused, $CIC, $unused_sif) = @_;
    print "RLC on CIC=$CIC\n";
}

sub isup_ignore {
    my ($type, $CIC, $rest) = @_;
    my $types = {
	0x02 => "subsequent address",
	0x06 => "address complete",
	0x09 => "answer",
	0x0c => "release",
	0x2c => "call progress"
    };
    my $pretty_type = $types->{$type};
    defined $pretty_type || die("unknown ISUP message type: $type");
    print "ignoring ISUP $pretty_type\n";
}

sub monitor_mtp2 {
    my ($host, $span, $timeslot) = @_;

    my $api = gth_control->new($host);

    warn_if_l1_dead($api, $span);

    my ($mtp2_id, $data) = $api->new_mtp2_monitor($span, $timeslot);


    while (1) {
	read($data, my $b, 2);
	my $length = unpack("n", $b);
	read($data, my $packet, $length);
	decode_mtp2($packet);
    }
    
    $api->delete($mtp2_id);
    $data->close();

    $api->bye();
}

# Entry point
if ($#ARGV + 1 != 3) {
    usage();
    die();
}

monitor_mtp2($ARGV[0], $ARGV[1], $ARGV[2]);
