//----------------------------------------------------------------------
// Title: Minimal demonstration of MTP-2 packet reception
// Author: Matthias Lang (matthias@corelatus.com)
// Created: October 2014
//
// Copyright (c) 2014 Corelatus AB Stockholm
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
	    api.send_command("<set name='pcm" + span + "'>"
			   + "<attribute name='status' value='enabled'/>"
			   + "</set>");
	    Client_conn.assert_name(api.next_non_event(), "ok");


	    log("Enabling MTP-2 on timeslot " + timeslot);

	    api.send_command("<new><mtp2_monitor ip_addr='" + local_ip_addr()
			   + "' ip_port='" + dest_port + "'>" +
			   "<pcm_source span='" + span + "' timeslot='"
			   + timeslot + "'/>" +
			   "</mtp2_monitor></new>");
	    Node reply = api.next_non_event();
	    Client_conn.assert_name(reply, "job");
	} catch (IOException e) {
	    log("Unable to connect to " + hostname);
	    System.exit(-1);
	}
    }

    //--------------------
    // Returns the local machine's IP address as a string
    private static String local_ip_addr() throws UnknownHostException {
	InetAddress local = InetAddress.getLocalHost();
	String ip_addr = "";
	byte[] b = local.getAddress();

	for (int j = 0; j < 4; j++) {
	    int i = b[j];
	    if (i < 0) i += 256;
	    ip_addr += i;
	    if (j != 3) ip_addr += ".";
	}
	return ip_addr;
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
	    new simple_mtp2(args[0], args[1], new Integer(args[2]));
	}
	catch (NumberFormatException e) {
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
