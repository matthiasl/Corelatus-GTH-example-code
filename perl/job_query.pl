#!/usr/bin/perl -Igth_control/lib/
#
# Title: Query job attributes on a Corelatus GTH
# Author: Matthias Lang (matthias@corelatus.se)
#

use 5.008;         # Probably works with earlier versions too.
use strict;
use warnings;
use gth_control;
use Data::Dumper;

sub usage() {
    print("
job_query.pl [-v] <hostname> <job-id>

  <hostname>: the hostname or IP address of a GTH
    <job-id>: the ID of a currently running job on the GTH
          -v: verbose; for debugging

Another Perl script, query_set.pl, can list all currently running
jobs, e.g. ./query_set.pl 172.16.1.10 schedule

Examples:
  ./job_query.pl 172.16.1.10 ldmo13
");
}

# Entry point
my $verbose = 0;
if ($ARGV[0] eq "-v") {
    $verbose = 1;
    shift @ARGV;
}

($#ARGV >= 1) || usage() && die();

my $host = $ARGV[0];
my $id = $ARGV[1];
my $api = new gth_control($host, $verbose);

my $result = $api->query_job_verbose($id);

# Specific example for LAPD: walk enough of the data structure to print
# the source and a couple of packet counters
if ( defined($result->{lapd_monitor}) ) {
    my $top = $result->{lapd_monitor}->{$id};

    my $source = $top->{pcm_source}[0];
    my $attributes = $top->{attribute};
    my $n_su = $attributes->{n_su}->{value};

    print "LAPD monitor id=$id span=$source->{span} first_bit=$source->{first_bit} bandwidth=$source->{bandwidth} n_su=$n_su\n";
}
else
{
    print Dumper($result);
}


$api->bye();
