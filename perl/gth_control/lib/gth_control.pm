package gth_control;

use 5.008;         # Probably works with earlier versions too.
use strict;
use warnings;
use IO::Socket;
use Data::Dumper;
use XML::Simple;   # From CPAN. In Debian it's called libxml-simple-perl

sub new {
    my ($unused, $gth_ip_or_hostname, $verbose) = @_;
    my $self = {};

    $self->{VERBOSE} = $verbose;

    debug($self, "connecting to $gth_ip_or_hostname...");
    # Open a socket to the GTH command socket
    my $sock = new IO::Socket::INET->new(PeerAddr => $gth_ip_or_hostname,
					 PeerPort => 2089,
					 Proto => 'tcp');
    $sock || die("couldn't open the socket to $gth_ip_or_hostname");
    $self->{SOCKET} = $sock;

    my ($port, $ia) = unpack_sockaddr_in($sock->sockname());
    $self->{MY_IP} = inet_ntoa($ia);

    bless($self);
    $self->debug("...connected OK");

    return $self;
}

sub bye {
    my ($self) = @_;

    $self->send("<bye/>");
    my $parsed = $self->next_non_event();

    expect_xml($parsed, "ok", "bye failed");
}

sub debug {
    my ($self, $info) = @_;

    if ($self->{VERBOSE}) {
	print STDERR $info, "\n";
    }
}

sub delete {
    my ($self, $id) = @_;

    $self->send("<delete id='$id'/>");
    my $parsed = $self->next_non_event();

    defined $parsed->{"ok"}
}

sub enable {
    my ($self, $resource, %hash) = @_;

    my $attributes = "";
    while (my ($key, $value) = each %hash) {
	$attributes .= "<attribute name='$key' value='$value'/>"
    }

    $self->send("<enable name='$resource'>$attributes</enable>");

    my $parsed = $self->next_non_event();

    expect_xml($parsed, "ok", "enable failed");
}

sub map {
    my ($self, $resource) = @_;

    $self->send("<map target_type='pcm_source'>".
		"<sdh_source name='$resource'/></map>");
    my $resource_id = parse_resource_name($self->next_non_event());
    return $resource_id;
}

sub new_mtp2_monitor {
    my ($self, $span, $timeslot) = @_;

    my $listen_socket = IO::Socket::INET->new(Listen => 5, Proto => 'tcp');
    my $my_ip = $self->{MY_IP};
    my $my_port = $listen_socket->sockport();

    $self->send("<new><mtp2_monitor ip_addr='$my_ip' ip_port='$my_port'>".
		"<pcm_source span='$span' timeslot='$timeslot'/>".
		"</mtp2_monitor></new>");

    my $data_socket = $self->accept($listen_socket);
    my $job_id = parse_job_id($self->next_non_event());

    return ($job_id, $data_socket);
}

sub new_player {
    my ($self, $span, $timeslot) = @_;

    my $listen_socket = IO::Socket::INET->new(Listen => 5, Proto => 'tcp');
    my $my_ip = $self->{MY_IP};
    my $my_port = $listen_socket->sockport();

    $self->send("<new><player>".
		"<tcp_source ip_addr='$my_ip' ip_port='$my_port'/>".
		"<pcm_sink span='$span' timeslot='$timeslot'/>".
		"</player></new>");

    my $data_socket = $self->accept($listen_socket);
    my $job_id = parse_job_id($self->next_non_event());

    return ($job_id, $data_socket);
}

sub new_recorder {
    my ($self, $span, $timeslot) = @_;

    my $listen_socket = IO::Socket::INET->new(Listen => 5, Proto => 'tcp');
    my $my_ip = $self->{MY_IP};
    my $my_port = $listen_socket->sockport();

    $self->send("<new><recorder>".
		"<pcm_source span='$span' timeslot='$timeslot'/>".
		"<tcp_sink ip_addr='$my_ip' ip_port='$my_port'/>".
		"</recorder></new>");


    my $data_socket = $self->accept($listen_socket);
    my $job_id = parse_job_id($self->next_non_event());

    return ($job_id, $data_socket);
}

# Returns either a hash or a list, depending in what's queried.
#
# Limitation: doesn't work on logs, e.g. system_log
sub query_resource {
    my ($self, $name, $attribute) = @_;

    $self->send("<query><resource name='$name'/></query>");
    my $answer = $self->next_non_event();

    defined $answer->{state} || die("queried bogus resource $name");

    # Special-case for inventory, return a list of resources
    if ($name eq "inventory") {
	my $hash = $answer->{state}[0]->{resource};
	return keys(%$hash);
    }

    # Special-case for schedule, return a hash of job->owner
    if ($name eq "schedule") {
	my $attributes = $answer->{state}[0]->{job};
	my %flattened;

	while(my ($job, $owner) = each(%{$attributes})) {
	    $flattened{$job} = $owner->{"owner"};
	}
	return %flattened;
    }

    # General case, return a hash of attribute->value
    my $attributes = $answer->{state}[0]->{resource}->{$name}->{attribute};

    if (defined $attribute) {
	return $attributes->{$attribute}->{value};
    } else {
	my %flattened;

	while(my ($key, $value) = each(%{$attributes})) {
	    $flattened{$key} = $value->{value};
	}
	return %flattened;
    }
}

sub query_job_verbose {
    my ($self, $id) = @_;

    $self->send("<query verbose='true'><job id='$id'/></query>");
    my $answer = $self->next_non_event();

    defined $answer->{state} || die("queried bogus job $id");

    my $attributes = $answer->{state}[0];
}

sub set {
    my ($self, $resource, %hash) = @_;

    my $attributes = "";
    while (my ($key, $value) = each %hash) {
	$attributes .= "<attribute name='$key' value='$value'/>"
    }

    $self->send("<set name='$resource'>$attributes</set>");

    my $parsed = $self->next_non_event();

    expect_xml($parsed, "ok", "set failed");
}

sub unmap {
    my ($self, $resource) = @_;

    $self->send("<unmap name='$resource'/>");
    my $parsed = $self->next_non_event();

    expect_xml($parsed, "ok", "unmap failed");
}


#-- Internal functions.

sub accept {
    my ($self, $listen) = @_;

    $self->debug("waiting for accept...");
    my $s = $listen->accept();
    $self->debug("...accepted");

    return $s;
}

sub expect_xml {
    my ($parsed, $expected, $hint) = @_;

    if (defined $parsed->{$expected}) {
         return;
    }
    printf(STDERR "$hint\n");
    die(Dumper($parsed));
}

sub parse_job_id {
    my ($parsed) = @_;

    defined($parsed->{'job'}) || die(Dumper($parsed));

    my $hashref= $parsed->{'job'};
    return (keys %$hashref)[0];
}

sub parse_resource_name {
    my ($parsed) = @_;

    defined($parsed->{'resource'}) || die(Dumper($parsed));

    my $hashref= $parsed->{'resource'};
    return (keys %$hashref)[0];
}

sub send {
    my ($self, $data, $type) = @_;
    my $s = $self->{SOCKET};

    if (! defined($type)) {
	$type = "text/xml";
	$self->debug("API command: $data");
    }

    $s->send("Content-type: $type\r\n");
    $s->send("Content-length: " . length($data) . "\r\n\r\n");
    $s->send($data);
}

sub receive_raw {
    my ($self) = @_;
    my $s = $self->{SOCKET};

    my $first = <$s>;
    my $second = <$s>;
    my $blank = <$s>;

    $second =~ /Content-length: ([0-9]+)/;
    my $length = $1;

    read($s, my $buffer, $length);
    $self->debug("XML: $buffer");

    length($buffer) == $length || die("definite_read got a short read");

    return $buffer;
}

sub receive {
    my ($self) = @_;
    my $s = $self->{SOCKET};

    my $parsed = XMLin($self->receive_raw(), KeepRoot => 1, ForceArray => 1);

    return $parsed;
}

sub next_non_event {
    my ($self) = @_;
    my $parsed = $self->receive();

    while (defined $parsed->{"event"}) {
	$parsed = $self->receive();
    }

    return $parsed;
}

sub next_event {
    my ($self) = @_;
    my $parsed = $self->receive();

    defined $parsed->{"event"}
    || die("expected event, got " . Dumper($parsed) );

    return $parsed;
}

1;
__END__
=head1 NAME

gth_control - Perl extension to control a Corelatus GTH.

=head1 SYNOPSIS

  use gth_control;

  my $api = new gth_control("172.16.1.10");
  ...
  $api->bye();

=head1 DESCRIPTION

A Corelatus GTH is a device which can terminate or monitor (sniff) an
E1/T1 line to decode signalling, record or play back voice, detect
tones or connect timeslots.

A GTH is controlled through a socket using a text protocol. This
module wraps that text protocol in a Perl API. The function names map
1:1 to the commands in the API, e.g. new_mtp_monitor() corresponds to
the <new><mtp2_monitor>... command.

=head1 CONSTRUCTOR

  new(Hostname)

  Creates gth_control, which allows you to control a Corelatus GTH.

  The argument is a string, either a hostname or an IP address, which
  tells gth_control which GTH unit to control.

=head1 METHODS

=over

=item bye ()

  Terminates the API connection to the GTH. You can't use the object
  after calling bye().

=item delete (ID)

  Delete (stop) a job on the GTH.

=item enable (Resource, Attributes)

  The <enable> command.

=item map (Resource, Attributes)

  The <map> command.

=item new_mtp2_monitor (Span, Timeslot)

  The <new><mtp2_monitor> command.

  Commands the GTH to start decoding MTP-2 on the given timeslot.

  Returns a job identifier (needed to delete the MTP-2 decoding job)
  and a socket with the signalling on it. The socket uses the
  format described in the GTH API manual, under new_fr_monitor.

=item new_player (Span, Timeslot)

  Starts a TCP player on the given span/timeslot. Returns a job
  identifier and a socket. The caller can write data to the socket,
  indefinitely. The GTH plays out data at 8000 octets per second.

  The normal way to use TCP sockets is to put the write statement
  in a loop. It'll block as necessary, i.e. no rate control is necessary,
  TCP flow control takes care of managing buffering.

=item next_non_event ()

  Return the next response on the API socket, discarding any events
  which might come before it.

=item next_event ()

  Return the next event on the API socket. Crash if anything else
  comes along.

=item query_resource (Name)

  Queries the given resource.

  For most values of <Name>, returns a hash of all the attributes.

  For the special value of <Name>, 'inventory', returns a list of
    resources.

=item query_resource (Name, Attribute)

  Queries the given resource's attribute. Returns a value.

=item query_job_verbose (Id)

  Queries a job. Return a raw data structure.

=item set (Name, Attribute, Value, ...)

  Sets the given attribute of a resource.

  Multiple Attribute, Value pairs can be given.

=item unmap (Resource, Attributes)

  The <unmap> command.


=back

=head1 SEE ALSO

https://www.corelatus.com/gth/api/

=head1 AUTHOR

Matthias Lang (matthias@corelatus.com)

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2009, Corelatus AB

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.10.0 or,
at your option, any later version of Perl you may have available.

=cut
