#!/usr/bin/perl -Igth_control/lib/
#
# Title: Unmap a previously mapped E1/T1 carried on an SDH/SONET link.
# Author: Matthias Lang (matthias@corelatus.se)
#

use 5.008;         # Probably works with earlier versions too.
use strict;
use warnings;
use gth_control;
use Data::Dumper;

sub usage() {
    print("
unmap.pl <hostname> <resource>

  <hostname>: the hostname or IP address of a GTH
  <resource>: a previously mapped E1/T1 (hint: 'query inventory')

Example:
   ./unmap.pl 172.16.1.10 pcm1

");
}

# Entry point
($#ARGV == 1) || usage() && die();

my ($host, $resource) = @ARGV;

my $api = new gth_control($host);
$api->unmap($resource);

$api->bye();
