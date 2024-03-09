//----------------------------------------------------------------------
// Title: Minimal demonstration of MTP-2 packet reception
// Author: Matthias Lang (matthias@corelatus.com)
// Created: October 2014
//
// Copyright (c) 2014 Corelatus AB
//
// This is demonstration code. Use at your own risk. Permission granted to
// copy, modify and integrate into other code.
//
// Receives MTP-2 packets on the specified interface and timeslot, prints
// out a summary of each packet.
//
// typical use:
//       java corelatus.gth.simple_mtp2 1A 1
//
//----------------------------------------------------------------------
package corelatus.gth;

import corelatus.gth.*;
import java.io.*;
import java.net.*;
import org.w3c.dom.*;

public class simple_mtp2 {
    private Socket signaling;
    private Client_conn api;

    // ctor.
    public simple_mtp2(String hostname, String span, Integer timeslot) {
	try {
	    ServerSocket l = new ServerSocket(0);
	    start_mtp2_job(hostname, span, timeslot, l.getLocalPort());
	    signaling = l.accept();
	    log("accepted ");
	    receive_packets();
	}
	catch (IOException e)
	    {
		log("IO exception: " + e);
	    }

    }

    private void start_mtp2_job(String hostname, String span,
			   Integer timeslot, Integer dest_port) {
	try {
	    api = new Client_conn(hostname);

	    log("Enabling PCM" + span);
	    api.send_command(xml.enable("pcm" + span,
                                        xml.make_map("monitoring", "true")));
	    Client_conn.assert_name(api.next_non_event(), "ok");

	    log("Enabling MTP-2 on timeslot " + timeslot);

            String source = xml.pcm_source(span, timeslot);
	    api.send_command(xml.new_mtp2_monitor(local_ip(),
                                                  dest_port,
                                                  source));
	    Node reply = api.next_non_event();
	    Client_conn.assert_name(reply, "job");
	} catch (IOException e) {
	    log("Unable to connect to " + hostname);
	    System.exit(-1);
	}
    }

    // Figure out the IP of this machine, from the GTH's point of view.
    //
    // This is a bit tricky. Asking the local machine is unreliable,
    // especially on multi-homed servers.
    //
    // What we do instead is find the job-id of the controller and query it.
    private String local_ip() throws IOException
    {
        api.send_command(xml.query_job("self"));
        Node qj1 = api.next_non_event().getChildNodes().item(0);
        String id = qj1.getAttributes().getNamedItem("id").getNodeValue();
        api.send_command(xml.query_job(id));
        Node qj2 = api.next_non_event().getChildNodes().item(0);
        String ip = qj2.getAttributes().getNamedItem("ip_addr").getNodeValue();

        return ip;
    }

    // The normal read in java.io.inputstream reads _up to_ the
    // number bytes we asked for. We loop to get all we want.
    private static int forced_read(InputStream is, byte packet[], int length)
    throws IOException {
	int result = 0;
	int offset = 0;

	while (result >= 0 && length > 0) {
	    result = is.read(packet, offset, length);
	    length -= result;
	    offset += result;
	}

	if (result < 0) log("forced read failed");

	return result;
    }

    private void receive_packets() {
	log("Ready to receive MTP-2 packets");
	try {
	    InputStream is = signaling.getInputStream();
	    int packets = 0;
	    byte packet[] = new byte[1000];  // mtp2 packets < 300 octets
	    int result;
	    int length;

	    do {
		result = forced_read(is, packet, 2);
		if (result < 0) break;
		length = packet[0] * 256 + packet[1];
		if (length > 1000) log("oversize packet received " + length);
		result = forced_read(is, packet, length);
		if (result < 0) break;

		pretty_print_packet(packet, length);

		packets++;
	    } while (true);
	} catch (Exception e) {
	    log("receive_packets threw exception" + e);
	}
	log("terminating");
    }

    static private void pretty_print_packet(byte packet[], int length) {
	System.out.println("packet: " + bytesToHex(packet, length));
    }

    final protected static char[] hexArray = "0123456789ABCDEF".toCharArray();
    public static String bytesToHex(byte[] bytes, int length) {
	char[] hexChars = new char[length * 3];
	for ( int j = 0; j < length; j++ ) {
	    int v = bytes[j] & 0xFF;
	    hexChars[j * 3] = hexArray[v >>> 4];
	    hexChars[j * 3 + 1] = hexArray[v & 0x0F];
	    hexChars[j * 3 + 2] = ' ';
	}
	return new String(hexChars);
    }

    static public void log(String message) {
	System.out.println(message);
    }

    public static void main(String[] args) {
	try {
	    new simple_mtp2(args[0], args[1], Integer.valueOf(args[2]));
	}
	catch (Exception e) {
	    usage();
	}
    }

    private static void usage() {
	System.err.println("usage: java simple_mtp2 <host> <span> <timeslot>");
	System.err.println("example: java simple_mtp2 172.16.1.10 1A 16");
	System.exit(-1);
    }

}

// eof
