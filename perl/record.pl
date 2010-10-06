#!/usr/bin/perl -Igth_control/lib/
#
# Title: Record E1/T1 timeslot data from a Corelatus GTH 
# Author: Matthias Lang (matthias@corelatus.se)
#

use 5.008;         # Probably works with earlier versions too.
use strict;
use warnings;
use gth_control;
use Data::Dumper;

sub usage() {
    print("
record <hostname> <span> <timeslot> <octets> <filename>

  <hostname>: the hostname or IP address of a GTH
      <span>: the name of an E1/T1, e.g. 1A or 4D on a GTH 2.x
  <timeslot>: 1--31 on an E1 or 1--24 on a T1
    <octets>: how many octets to record. There are 8000 per second
  <filename>: which file to write the data to. - means stdout

Typical invocation: ./record.pl 172.16.1.10 1A 16 16000 signalling.raw
");
}

# Check that a given PCM is in a state where it could give useful data.
# That means in 'OK' or 'RAI' status.
sub warn_if_l1_dead {
    my ($api, $span) = @_;

    my $status = $api->query_resource("pcm$span", "status");

    defined $status || die(Dumper($status) . $span);

    if ($status eq "OK") {
    } elsif ($status eq "RAI") {
    } elsif ($status eq "disabled") {
	print STDERR "Warning: pcm$span is disabled. The GTH will emit all constant data.\n".
	    "Hint: enable L1 with enable_l1.pl\n";
    } else {
	print STDERR "Warning: pcm$span status is $status\n" .
	    "Chances are the other end of your E1 isn't plugged in (or enabled)\n";
    }
}

sub record {
    my ($host, $span, $timeslot, $octets_wanted, $file) = @_;
    my $api = new gth_control($host);
    warn_if_l1_dead($api, $span);

    my ($recorder_id, $data) = $api->new_recorder($span, $timeslot);
    my $octets_received = 0;
    my $buffer = "";

    while ($octets_received < $octets_wanted) {
	read($data, $buffer, $octets_wanted) || die("socket read failed");
	$octets_received += length($buffer);
	print $file $buffer;
    }

    close($file);
    close($data);
    print "recorder_id is $recorder_id\n";
    $api->delete($recorder_id);
    print STDERR "All done, saved $octets_received octets\n";
    $api->bye();
}

# Entry point
($#ARGV == 4) || usage() && die();

my $host = $ARGV[0];
my $span = $ARGV[1];
my $file;

if ($ARGV[4] eq "-") {
    $file = *STDOUT;
} else {
    open($file, ">", $ARGV[4]) || die("can't open $ARGV[4]");
}

record($ARGV[0], $ARGV[1], $ARGV[2], $ARGV[3], $file);
