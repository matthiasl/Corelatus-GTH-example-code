#!/usr/bin/perl -Igth_control/lib/
#
# Title: Play a file to an E1/T1 timeslot using a Corelatus GTH
# Author: Matthias Lang (matthias@corelatus.se)
#

use 5.008;         # Probably works with earlier versions too.
use strict;
use warnings;
use gth_control;
use Data::Dumper;

sub usage() {
    print("
play [-v] <hostname> <span> <timeslot> <filename>

  <hostname>: the hostname or IP address of a GTH
      <span>: the name of an E1/T1, e.g. 1A or 4D on a GTH 2.x
  <timeslot>: 1--31 on an E1 or 1--24 on a T1
  <filename>: which file to write the data to. - means stdin
          -v: verbose, for debugging

Typical invocation: ./play 172.16.1.10 1A 16 signalling.raw
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

sub play {
    my ($host, $span, $timeslot, $file, $verbose) = @_;
    my $api = new gth_control($host, $verbose);
    warn_if_l1_dead($api, $span);

    my ($player_id, $data) = $api->new_player($span, $timeslot);
    my $octets_sent = 0;
    my $buffer = "";

    # Stream out the message
    while (read($file, $buffer, 1000)) {
	$octets_sent += length($buffer);
	print $data $buffer;
    }

    close($file);
    close($data);

    # Check for a message-ended event. Since we only start one player
    # we don't need to check the ID on the event.
    while (1) {
	my $event = $api->next_event();
	my $message_ended = $event->{"event"}[0]->{"message_ended"};
	!defined $message_ended || last;
    }

    print STDERR "All done, sent $octets_sent octets\n";

    $api->bye();
}

# Entry point
my $verbose = 0;
if ($ARGV[0] eq "-v") {
    $verbose = 1;
    shift @ARGV;
}

($#ARGV == 3) || usage() && die();

my $host = $ARGV[0];
my $span = $ARGV[1];
my $file;

if ($ARGV[3] eq "-") {
    $file = *STDIN;
} else {
    open($file, $ARGV[3]) || die("can't open $ARGV[3]");
}

play($ARGV[0], $ARGV[1], $ARGV[2], $file, $verbose);

