#!/usr/bin/perl -Igth_control/lib/
#
# Title: Map an E1/T1 carried on an SDH/SONET link, usually a fiber
# Author: Matthias Lang (matthias@corelatus.se)
#

use 5.008;         # Probably works with earlier versions too.
use strict;
use warnings;
use gth_control;
use Data::Dumper;

sub usage() {
    print("
map.pl <hostname> <resource>

  <hostname>: the hostname or IP address of a GTH
  <resource>: an SDH/SONET LOP resource (hint: 'query inventory')

Example:
   ./map.pl 172.16.1.10 sdh1:hop1_1:lop1_1_1

Output:
   on success, this program prints the name of the newly mapped resource,
   e.g. 'pcm1'.
");
}

# Entry point
($#ARGV == 1) || usage() && die();

my ($host, $resource) = @ARGV;

my $api = new gth_control($host);
my $new_name = $api->map($resource);
print "$new_name\n";

$api->bye();
