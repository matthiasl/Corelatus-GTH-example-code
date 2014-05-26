//----------------------------------------------------------------------
// Title: GTH MTP-2 monitoring packet counter
// Author: Matthias Lang (matthias@corelatus.com)
// Created: February 2003
//
// Copyright (c) 2003 Corelatus AB Stockholm
//
// This is demonstration code. Use at your own risk. Permission granted to
// copy, modify and integrate into other code.
//
// Emulates an MTP-2 packet processor which does not have any way of
// communicating with the entity controlling the GTH through an API.
// The failure situations emulated are:
//
//     1. lockup (we no longer call read() on the socket
//     2. gradual slowdown (we don't accept data fast enough)
//     3. random socket shutdown (and subsequent re-accept)
//
// A java VM crash can be simulated by killing the VM via OS methods.
//
// typical use:
//       java corelatus.gth.mtp2_receiver 1234
//
// where '1234' is the port number the MTP-2 packets will arrive on.
//----------------------------------------------------------------------
package corelatus.gth;

import java.io.*;
import java.net.*;
import java.util.Calendar;
import java.util.Random;

public class mtp2_receiver implements Runnable {
    private Socket s;
    private Thread t;

    // ctor.
    public mtp2_receiver(Socket the_sock) {
	s = the_sock;
	try {
	    t = new Thread(this);
	    t.start();
	} catch (Exception e) {
	    log("mtp2_receiver threw while starting thread");
	}
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

    public void run() {
	log("mtp2_receiver running");
	try {
	    Random random = new Random();
	    InputStream is = s.getInputStream();
	    int packets = 0;
	    byte packet[] = new byte[1000];  // mtp2 packets < 300 octets
	    int result;
	    int length;
	    int sleep = 0;
	    int next_event = random.nextInt() % 40000;
	    if (next_event < 0) next_event = -next_event;
	    log("next event will be at " + next_event);

	    do {
		result = forced_read(is, packet, 12);
		if (result < 0) break;
		length = packet[8] * 256 + packet[9];
		if (length > 1000) log("oversize packet received " + length);
		result = forced_read(is, packet, length);
		if (result < 0) break;
		packets++;

		if (sleep > 0) Thread.sleep(sleep);

		if (packets % 10000 == 0) log(packets + " packets ");

		if (packets == next_event) {
		    next_event = random.nextInt() % 40000;
		    if (next_event < 0) next_event = -next_event;

		    // simulate various socket problems by changing
		    // the test from false to true
		    if (false) {
			log("simulated lockup");
			Thread.sleep(999888777);
		    }

		    if (false) {
			log("simulated slowdown");
			if (sleep == 0) {
			    sleep = random.nextInt() % 200;
			    next_event = next_event % 200;
			}
			else sleep = 0;
		    }

		    if (true) {
			log("simulated thread shutdown");
			break;
		    }
		}
	    } while (true);
	} catch (Exception e) {
	    log("mtp2_receiver:run threw exception" + e);
	}
	log("terminating receiver thread");
    }

    // Writes a timestamped message to stdout. Doesn't zero-pad digits.
    static public void log(String message) {
	Calendar c = Calendar.getInstance();
	System.out.println(c.get(Calendar.HOUR) + ":" +
			   c.get(Calendar.MINUTE) + ":" +
			   c.get(Calendar.SECOND) + " " + message);
    }

    public static void main(String[] args) {
	try {
	    int port = Integer.parseInt(args[0]);
	    ServerSocket l = new ServerSocket(port);

	    while (true) {
		Socket mtp2_sock = l.accept();
		log("accepted on port " + port);
		mtp2_receiver m = new mtp2_receiver(mtp2_sock);
	    }
	} catch (NumberFormatException e) {
	    System.err.println("run this program with one numerical arg");
	} catch (IOException e) {
	    System.err.println("ioexception in mtp2_listener");
	}
    }
}

// eof
