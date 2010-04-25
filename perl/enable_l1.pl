#!/usr/bin/perl -Igth_control/lib/
#
# Title: Enable and E1 on a Corelatus GTH
# Author: Matthias Lang (matthias@corelatus.se)
#
# $Id: enable_l1.pl,v 1.2 2009-05-28 21:05:56 matthias Exp $

use 5.008;         # Probably works with earlier versions too.
use strict;
use warnings;
use gth_control;

sub usage() {
    print("
  enable_l1.pl <hostname> <span>

  <hostname>: the hostname or IP address of a GTH
      <span>: the name of an E1/T1, e.g. 1A or 4D on a GTH 2.x

Typical invocation: ./enable_l1.pl 172.16.1.10 1A
");
}

# Entry point
if ($#ARGV + 1 != 2) {
    usage();
    die();
}

my $host = $ARGV[0];
my $span = $ARGV[1];

my $api = gth_control->new($host);

$api->send("<set name='pcm$span'><attribute name='mode' value='E1'/></set>");
defined $api->next_non_event()->{ok} || die("error from GTH (bogus PCM?)");
$api->bye();
