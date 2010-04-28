#!/usr/bin/perl -Igth_control/lib/
#
# Title: Query and/or set resource attributes on a Corelatus GTH
# Author: Matthias Lang (matthias@corelatus.se)
#

use 5.008;         # Probably works with earlier versions too.
use strict;
use warnings;
use gth_control;
use Data::Dumper;

sub usage() {
    print("
query_set.pl <hostname> <resource> [<attribute> [<value>]]

  <hostname>: the hostname or IP address of a GTH
  <resource>: the name of a resource on the GTH
 <attribute>: optional, the name of an attribute of the given resource
     <value>: optional, sets the given attribute of the given resource

If no <value> argument is given, just query the GTH.
If a  <value> argument is given, set the value
Multiple <attribute> <value> arguments can be given to set multiple attributes.

Examples:
  ./query_set.pl 172.16.1.10 pcm1A
  ./query_set.pl 172.16.1.10 board
  ./query_set.pl 172.16.1.10 inventory
  ./query_set.pl 172.16.1.10 sync 'primary ntp' 172.16.2.3
  ./query_set.pl 172.16.1.10 pcm1A status enabled  monitoring true  mode T1
");
}

# Entry point
($#ARGV >= 1) || usage() && die();

my $host = $ARGV[0];
my $resource = $ARGV[1];
my $api = new gth_control($host);

if ($#ARGV == 1) {
    if ($resource eq "inventory") {
	my @result = $api->query_resource($resource);
	foreach (sort @result) {
	    print $_ . "\n";
	}
    }
    else {
	my %result = $api->query_resource($resource);
	print Dumper(\%result);
    }
}

elsif ($#ARGV == 2) {
    my $result = $api->query_resource($resource, $ARGV[2]);
    print "$result\n";
}

else {
    shift @ARGV;
    shift @ARGV;
    $api->set($resource, @ARGV);
}

$api->bye();
