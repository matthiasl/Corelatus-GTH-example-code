#!/usr/bin/perl -Igth_control/lib/
#
# Title: Enable telecom interfaces on E1/T1 and SDH/SONET hardware
# Author: Matthias Lang (matthias@corelatus.se)
#

use 5.008;         # Probably works with earlier versions too.
use strict;
use warnings;
use gth_control;
use Data::Dumper;

sub usage() {
    print("
enable.pl <hostname> <resource> [<attribute> [<value>]]

  <hostname>: the hostname or IP address of a GTH
  <resource>:  an SDH/SONET or E1/T1 link on the GTH (hint: 'query inventory')
 <attribute>: optional, the name of an attribute of the given resource
     <value>: optional, sets the given attribute of the given resource

Multiple <attribute> <value> arguments can be given to set multiple attributes.

Examples:
   ./enable.pl 172.16.1.10 sdh1
   ./enable.pl 172.16.1.10 sdh1 AU 4  TU 12
   ./enable.pl 172.16.1.10 sdh1 SONET true  OC 3  VT 2
   ./enable.pl 172.16.1.10 pcm1A
   ./enable.pl 172.16.1.10 pcm2A  framing multiframe
   ./enable.pl 172.16.1.10 pcm2A  framing multiframe  monitoring true
");
}

# Entry point
($#ARGV >= 1) || usage() && die();

my $host = $ARGV[0];
shift @ARGV;
my $resource = $ARGV[0];
shift @ARGV;
my $api = new gth_control($host);

$api->enable($resource, @ARGV);

$api->bye();
